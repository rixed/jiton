open Jiton

let hashtbl_keys h =
	Hashtbl.fold (fun k _ prevs -> k :: prevs) h []

let bitsize_of cst =
	let rec aux size max_cst =
		if cst < max_cst then size
		else aux (size+1) (max_cst lsl 1) in
	aux 0 1

module Compiler (Impl_: IMPLEMENTER)
	: COMPILER with module Impl = Impl_ =
struct
	module Impl = Impl_

	type program_step = Impl.impl_lookup * string array * (string * data_type) array
	type program = program_step array
	
	type program_param = string * data_type

	(*
	 * Phase 1 : choosing a plan that implements the user program
	 *)
	
	(* Before proceeding to register allocation we choose the implementations for each
	 * program_step and build a plan *)
	type plan_step_kind = Loop_body | Loop_head | Loop_tail
	type plan_step =
		{ impl : Impl.op_impl ;
		  kind : plan_step_kind ;
		  input_names : string array ;
		  output_names : string array }
	type plan = plan_step array

	(* This phase requires to track user symbols *)
	type user_symbol = { mutable bank : int ; data_type : data_type }

	(* For debugging *)
	let string_of_data_type (size, sign) =
		let string_of_sign = function
			| Signed -> "signed"
			| Unsigned -> "unsigned" in
		Printf.sprintf "%d bits, %s" size (string_of_sign sign)
	let string_of_spec_in specs =
		let string_of_spec = function
			| Impl.Reg (bank, dtype) -> Printf.sprintf "Reg (bank=%d, dtype=%s)" bank (string_of_data_type dtype)
			| Impl.Cst c -> Printf.sprintf "Cst %s" (Impl.string_of_word c) in
		(Array.fold_left (fun prefix spec -> prefix^(string_of_spec spec)^"; ") "[ " specs)^" ] "
	let string_of_spec_out specs =
		let string_of_spec dtype = Printf.sprintf "dtype=%s" (string_of_data_type dtype) in
		(Array.fold_left (fun prefix spec -> prefix^(string_of_spec spec)^"; ") "[ " specs)^" ] "
	let string_of_plan_kind = function
		| Loop_head -> "HEAD"
		| Loop_tail -> "tail"
		| Loop_body -> "body"
	let print_plan plan =
		Array.iteri (fun i p ->
			let print_names arr =
				Array.iter (fun n -> Printf.printf "%s " n) arr in
			Printf.printf "Plan[%d](%s) : " i (string_of_plan_kind p.kind) ;
			print_names p.input_names ; Printf.printf " -> " ;
			print_names p.output_names ; Printf.printf "\n") plan

	(* From a "user program" giving only the main ingredients, build a plan
	 * by cooking possible implementations, served with loop and procedure call machinery.
	 *)
	let make_plan (program : program) func_params =
		let func_param_names = Array.map fst func_params in
		let func_param_dtypes = Array.map snd func_params in
		(* Returns a path of largest allowed scale.
		 * A "path" is a plan for the loop body only. *)
		let make_path min_scale max_scale =
			let find_path scale =
				Printf.printf "Looking for path of scale %d.\n" scale ;
				(* Build a symbol table giving the expected type and register bank of symbols. *)
				let symbols = Hashtbl.create 10 in
				(* Symbols are known from function parameters and later output variables. *)
				let add_symbol name dtype bank =
					Printf.printf "Add symbol %s, bank=%d, type=%s.\n"
						name bank (string_of_data_type dtype) ;
					Hashtbl.add symbols name { bank = bank ; data_type = dtype } in
				let add_symbols bank = Array.iter (fun (name, dtype) -> add_symbol name dtype bank) in
				add_symbols 0 func_params ;
				Array.map (fun (impl_lookup, inputs, outputs) ->
					(* Build input and output specifier. *)
					let specs_in = Array.map (fun sym_name ->
						(* Is it a constant ? *)
						try Impl.Cst (Impl.word_of_string sym_name)
						with Failure _ -> (	(* Or get info from the symbol table. *)
							let symbol = Hashtbl.find symbols sym_name in
							Impl.Reg (symbol.bank, symbol.data_type))) inputs in
					let specs_out = Array.map snd outputs in
					(* If we can't find it, we could still achieve the same result by repeating scale
					 * times the implementation for scale=1. *)
					Printf.printf "\tLooking for an impl @scale=%d, for specs = %s -> %s.\n" scale (string_of_spec_in specs_in) (string_of_spec_out specs_out) ;
					let impl = impl_lookup (scale, specs_in, specs_out) in
					Printf.printf "\tfound an impl giving %d outputs.\n" (Array.length impl.Impl.out_banks) ;
					assert ((Array.length impl.Impl.out_banks) = (Array.length outputs)) ;
					(* Add new symbols for outputs. *)
					Array.iteri (fun i (name, dtype) ->
						add_symbol name dtype impl.Impl.out_banks.(i)) outputs ;
					(* Returns the prealloc_plan. *)
					{ impl = impl ; kind = Loop_body ;
					  input_names = inputs ;
					  output_names = Array.map fst outputs }) program in
			let rec aux scale =
				if scale < min_scale then raise Not_found
				else try (find_path scale), scale with Not_found -> (
					Printf.printf "No path for scale %d.\n" scale ;
					aux (scale - 1)) in
			aux max_scale in
		(* Add func parameters loading into registers *)
		let load_params =
			let loader n =
				{ impl = Impl.load_param (1, [| Impl.Cst (Impl.word_of_int n) |], [| func_param_dtypes.(n) |]) ;
				  kind = Loop_body ;
				  input_names = [||] ;
				  output_names = [| func_param_names.(n) |] } in
			Array.mapi (fun n _dtype -> loader n) func_param_dtypes in
		(* Given a path, expand inline helper emitters, add loop_head/tail and
		 * invariant emitters. *)
		let helper_expanded = Hashtbl.create 10 in
		let loop_of_path path scale =
			let invariants = ref []
			and body = ref [] in
			let rec expand_step kind step =
				(* Add helpers to inputs *)
				let add_inputs = ref [] in
				(* Add outputing steps in invariants or body, if not already done. *)
				Array.iter (fun (kind, name, impl') ->
					add_inputs := name :: !add_inputs ;
					if not (Hashtbl.mem helper_expanded name) then (
						Hashtbl.add helper_expanded name true ;
						let inserted_step =
							{ impl = impl' ;
							  input_names = [||] ; output_names = [| name |] ;
							  kind = Loop_body (* FIXME: rename Loop_body by Operation? *)} in
						expand_step kind inserted_step))
					step.impl.Impl.helpers ;
				let new_step =
					{ step with
					  input_names = Array.concat
					  	[ step.input_names ; Array.of_list !add_inputs ] } in
				let l = if kind = Inline then body else invariants in
				l := new_step :: !l in
			let loop_head =
				(* TODO: here the first param is assumed to be the length of the loop... *)
				{ impl = Impl.loop_head (scale, [| Impl.Reg (0, func_param_dtypes.(0)) |], [||]) ;
				  input_names = [| func_param_names.(0) |] ; output_names = [||] ;
				  kind = Loop_head } in
			let loop_tail =
				{ impl = Impl.loop_tail (scale, [||], [||]) ;
				  input_names = [||] ; output_names = [||] ;
				  kind = Loop_tail } in
			let new_plan = Array.concat [ [|loop_head|] ; path ; [|loop_tail|] ] in
			Array.iter (expand_step Inline) new_plan ;
			Array.of_list (List.rev_append !invariants (List.rev !body)) in
		let rename_vars suffix plan =
			let array_exits arr e =
				try (
					Array.iter (fun e' -> if e = e' then raise Exit) arr ;
					false
				) with Exit -> true in
			let is_constant s = try (ignore (int_of_string s) ; true) with Failure _ -> false in
			let renamed arr = Array.map (fun s ->
				if array_exits func_param_names s || is_constant s then s else s^suffix) arr in
			Array.map (fun p ->
				{	p with
					input_names  = (renamed p.input_names) ;
					output_names = (renamed p.output_names) }) plan in
		let plan_of_path path =
			Array.concat [ load_params ; loop_of_path path 1 ] in
		let combine_paths slow_path fast_path scale =
			(* FIXME: we do not take into account data alignment here. but how can we ? *)
			(* We must force order of evalutation to have invariants outputed in front of the
			 * first loop. *)
			let fast_loop = loop_of_path (rename_vars "[fast]" fast_path) scale
			and slow_loop = loop_of_path (rename_vars "[slow_finish]" slow_path) 1 in
			Array.concat [ load_params ; fast_loop ; slow_loop ] in
		(* First, build the slow path (1 item at a time) *)
		let slow_path, _ = make_path 1 1 in
		(* Then look for a fast path (N items at a time) *)
		try (
			let fast_path, scale = make_path 2 8 in
			(* Then build the final path using both slow and fast paths. *)
			combine_paths slow_path fast_path scale
		) with Not_found -> (
			(* Or using only the slow path if no fast path was found. *)
			plan_of_path slow_path
		)

	(*
	 * Phase 2 : registers allocation
	 *)

	(* Used to build a symbol table during register allocation phase *)
	type symbol =
		{ outputer : int (*index in plan*) * int (*index in inputs*) ;
		  mutable inputers : (int (*index in plan*) * int (*index in inputs*)) list ;
		  mutable birth : int ;	(* first step that needs this var. *)
		  mutable death : int ;	(* last step that needs it. >=birth. If = we will not use the value but we need a reg to store a value nonetheless. *)
		  mutable alloc_bank : bank_num ;
		  mutable alloc_reg : reg_num }
	
	(* Build the getter function (ie. the symbol table, functional style). *)
	let reg_allocation plan =
		let used_regs = Hashtbl.create 10 in
		let nb_banks  = Array.length Impl.register_sets in

		(* Build symbol table. *)
		Printf.printf "Build symbol table.\n" ;
		let symbols = Hashtbl.create 10 in
		Array.iteri (fun plan_idx pp ->
			let add_inputer input_idx sym_name =
				(* sym_name may be an integer constant *)
				(* FIXME: Ocaml's integers are not great for constants, word would be better. *)
				try ignore (int_of_string sym_name)
				with Failure _ -> (
					let s = Hashtbl.find symbols sym_name in
					s.inputers <- (plan_idx, input_idx) :: s.inputers ;
					s.death <- plan_idx) in
			let create_symbol output_idx sym_name =
				assert (Hashtbl.find_all symbols sym_name = []) ;
				Hashtbl.add symbols sym_name {
					outputer = plan_idx, output_idx ;
					inputers = [] ;
					birth = plan_idx ;
					death = plan_idx ;
					alloc_bank = plan.(plan_idx).impl.Impl.out_banks.(output_idx) ;
					alloc_reg = -1 } in
			(* For each output names, create the symbol. *)
			Array.iteri create_symbol pp.output_names ;
			(* For each input names, add me as an inputer. *)
			Array.iteri add_inputer pp.input_names) plan ;
		
		(* But we have loops : all vars outputed before a looping
		 * point and inputed after it must be kept until loop end. *)
		let postpone_death sym_name symbol =
			let (outputer_idx, _) = symbol.outputer in
			let last_user = List.fold_left
				(fun prev_max (inputer_idx, _) -> max prev_max inputer_idx)
				0 symbol.inputers in
			let last_loop_head =
				let rec aux last_head idx =
					if idx > last_user then last_head
					else aux (if plan.(idx).kind = Loop_head then idx else last_head) (idx+1) in
				aux 0 0 in
			if last_loop_head > outputer_idx then (
				let this_loop_tail =
					let rec aux level idx = match plan.(idx).kind with
						| Loop_head -> aux (level+1) (idx+1)
						| Loop_tail -> if level = 1 then idx else aux (level-1) (idx+1)
						| _ -> aux level (idx+1) in
					aux 0 last_loop_head in
				Printf.printf "Make symbol %s immortal in loop [%d -> %d].\n"
					sym_name last_loop_head this_loop_tail ;
				symbol.death <- this_loop_tail) in
		Hashtbl.iter postpone_death symbols ;
		
		(* Find out how many registers are needed at each step. *)
		let nb_var_regs = Array.init nb_banks (fun _ ->
			Array.make (Array.length plan) 0) in
		Hashtbl.iter (fun _sym_name symbol ->
			for i = symbol.birth to symbol.death do
				nb_var_regs.(symbol.alloc_bank).(i) <- succ nb_var_regs.(symbol.alloc_bank).(i)
			done) symbols ;
		let nb_vars = Array.init nb_banks (fun b ->
			Array.fold_left max 0 nb_var_regs.(b)) in
		Printf.printf "For vars we need :\n" ;
		Array.iteri (fun bank nb -> Printf.printf "\t%d regs from bank %d\n" nb bank) nb_vars ;
		
		(* For allocating var registers we need a bitmap of these registers : *)
		let var_reg_bitmap = Array.init nb_banks (fun b ->
			Array.make nb_vars.(b) false) in
		let alloc_var_reg bank =
			let rec aux i =
				if var_reg_bitmap.(bank).(i) then aux (i+1)
				else (var_reg_bitmap.(bank).(i) <- true ; i) in
			aux 0 in
		let free_var_reg (bank, i) =
			var_reg_bitmap.(bank).(i) <- false in
		let freelist_at_step = Array.make ((Array.length plan)+1) [] in

		(* Now perform allocation for all symbols, stepping the plan and allocating/freeing
		 * regs as required *)
		for step = 0 to Array.length plan do
			(* Free the vars that will not be used any more. *)
			List.iter free_var_reg freelist_at_step.(step) ;
			(* Alloc a reg for all symbols born at this step *)
			Hashtbl.iter (fun sym_name symbol ->
				if symbol.birth = step then (
					symbol.alloc_reg <- alloc_var_reg symbol.alloc_bank ;
					freelist_at_step.(symbol.death + 1) <-
						(symbol.alloc_bank, symbol.alloc_reg) :: freelist_at_step.(symbol.death + 1) ;
					Hashtbl.replace used_regs (symbol.alloc_bank, symbol.alloc_reg) true ;
					Printf.printf "Using register %d.%d for %s from step %d to %d\n"
						symbol.alloc_bank symbol.alloc_reg sym_name symbol.birth symbol.death))
				symbols ;
		done ;

		(* Then build the getter *)
		let getter step name =
			let get_symbol n =
				let symbol = Hashtbl.find symbols n in
				assert (symbol.birth <= step && symbol.death >= step) ;
				symbol.alloc_bank, symbol.alloc_reg in
			let get_input idx =
				get_symbol plan.(step).input_names.(idx) in
			let get_output idx =
				get_symbol plan.(step).output_names.(idx) in
			(* By convention, names matching '<i' means ith input
			 * while '>o' means oth output *)
			try Scanf.sscanf name "<%d" get_input
			with Scanf.Scan_failure _ ->
			try Scanf.sscanf name ">%d" get_output
			with Scanf.Scan_failure _ ->
			get_symbol name in
		getter, hashtbl_keys used_regs

	let compile (program : program) (params : program_param array) =
		let proc = Impl.make_proc (Array.length params) in
		(* Choosing an implementation *)
		let plan = make_plan program params in
		print_plan plan ;
		(* Register allocation. *)
		let getter, used_regs = reg_allocation plan in
		(* Emit procedure code *)
		Impl.emit_entry_point proc [||] used_regs ;
		(* Emit function body *)
		Array.iteri (fun step p ->
			p.impl.Impl.emitter proc (getter step)) plan ;
		(* Emit exit code *)
		Impl.emit_exit proc ;
		(* Return the function *)
		Impl.exec proc
	
end
