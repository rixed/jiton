(*
	Attempt at a simple rgb darken function :
	- read pixel from pixels
	- unpack565 into r, g, b
	- read alpha into a
	- mul r, a into r'
	- mul g, a into g'
	- mul b, a into b'
	- pack565 r', g', b' into pixel'
	- write pixel' into image
*)

open Jiton
open Impl_virtual
open Virtual
open Big_int

type symbol = {
	outputer : int (*index in plan*) * int (*index in inputs*) ;
	mutable inputers : (int (*index in plan*) * int (*index in inputs*)) list ;
	mutable birth : int ;	(* first step that needs this var. *)
	mutable death : int ;	(* last step that needs it. >=birth. If = we will not use the value but we need a reg to store a value nonetheless. *)
	mutable alloc_bank : bank_num ;
	mutable alloc_reg : reg_num }

type plan = {
	impl : op_impl ;
	perm_regs : int array ;
	mutable in_regs : int array ;
	scratch_regs : int array ;
	out_regs : int array }

type prealloc_plan = {
	prealloc_impl : op_impl ;
	input_names : string array ;
	output_names : string array }

(* For this to work, the first step of the plan must be the entry point, taking
 * no input and outputing all the function parameters as var_regs. *)
let reg_allocation preplan loops =
	(* Build symbol table. *)
	Printf.printf "Build symbol table.\n" ;
	let symbols = Hashtbl.create 10 in
	Array.iteri (fun plan_idx pp ->
		let add_inputer input_idx sym_name =
			(* sym_name may be an integer constant *)
			(* FIXME: Ocaml's integers are not great for constants, word would be better. *)
			try ignore (int_of_string sym_name)
			with Failure _ ->
				let s = Hashtbl.find symbols sym_name in
				s.inputers <- (plan_idx, input_idx) :: s.inputers ;
				s.death <- plan_idx in
		let create_symbol output_idx sym_name =
			assert (Hashtbl.find_all symbols sym_name = []) ;
			Hashtbl.add symbols sym_name {
				outputer = plan_idx, output_idx ;
				inputers = [] ;
				birth = plan_idx ;
				death = plan_idx ;
				alloc_bank = preplan.(plan_idx).prealloc_impl.out_banks.(output_idx) ;
				alloc_reg = 0 } in
		(* For each input names, add me as an inputer. *)
		Array.iteri add_inputer pp.input_names ;
		(* For each output names, create the symbol. *)
		Array.iteri create_symbol pp.output_names) preplan ;
	
	(* But we have loops : all vars outputed before the looping
	 * point and inputed after it must be kept untill loop end. *)
	let check_death sym_name symbol = 
		let (outputer_idx, _) = symbol.outputer in
		let check_loop (loop_start_idx, loop_end_idx) =
			if
				outputer_idx < loop_start_idx &&
				List.exists (fun (inputer_idx, _) -> inputer_idx >= loop_start_idx) symbol.inputers
			then (
				Printf.printf "Make register %s immortal in loop [%d -> %d].\n"
					sym_name loop_start_idx loop_end_idx ;
				symbol.death <- loop_end_idx) in
		List.iter check_loop loops in
	Hashtbl.iter check_death symbols ;
	
	(* Find out how many registers we need to store vars. *)
	let nb_var_regs = Array.init (Array.length register_sets) (fun _ ->
		Array.make (Array.length preplan) 0) in
	Hashtbl.iter (fun _sym_name symbol ->
		for i = symbol.birth to symbol.death do
			nb_var_regs.(symbol.alloc_bank).(i) <- nb_var_regs.(symbol.alloc_bank).(i) + 1
		done) symbols ;
	let nb_vars = Array.init (Array.length register_sets) (fun s ->
		Array.fold_left max 0 nb_var_regs.(s)) in
	Printf.printf "For vars we need :\n" ;
	Array.iteri (fun bank nb -> Printf.printf "\t%d regs from bank %d\n" nb bank) nb_vars ;
	
	(* Find out how many registers we need for perm and scratch regs, per bank. *)
	let arr_or_zero arr idx =
		if idx < Array.length arr then arr.(idx) else 0 in
	let get_required_perms bank =
		Array.fold_left (fun prev pp -> prev + (arr_or_zero pp.prealloc_impl.perm bank)) 0 preplan in
	let get_required_scratchs bank =
		Array.fold_left (fun prev pp -> max prev (arr_or_zero pp.prealloc_impl.scratch bank)) 0 preplan in
	let nb_banks   = Array.length register_sets in
	let nb_perms   = Array.init nb_banks get_required_perms in
	let nb_scratch = Array.init nb_banks get_required_scratchs in
	for bank = 0 to nb_banks - 1 do
		Printf.printf "Bank %d : need %d perms registers, %d scratch registers.\n"
			bank nb_perms.(bank) nb_scratch.(bank)
	done ;

	(* Partition register banks like this : first the permanent registers,
	 * then the scratch registers, then the var registers. *)
	let first_perm _bank = 0 in
	let first_scratch bank = (first_perm bank) + nb_perms.(bank) in
	let first_var bank = (first_scratch bank) + nb_scratch.(bank) in

	(* For allocating var registers we need a bitmap of these registers : *)
	let var_reg_bitmap = Array.init (Array.length register_sets) (fun s ->
		Array.make nb_vars.(s) false) in
	let alloc_var_reg bank =
		let rec aux i =
			if var_reg_bitmap.(bank).(i) then aux (i+1)
			else (var_reg_bitmap.(bank).(i) <- true ; i) in
		aux 0 in
	let free_var_reg (bank, i) =
		var_reg_bitmap.(bank).(i) <- false in
	let freelist_at_step = Array.make ((Array.length preplan)+1) [] in

	(* Then build the plan *)
	let next_perm = Array.init nb_banks (fun b -> first_perm b) in
	let nb_regs reqs = Array.fold_left (+) 0 reqs in
	let bank_of reqs i =	(* from [|5;4|] and 7, return 1,2 *)
		let rec aux bank i =
			if i <= reqs.(bank) then bank, i
			else aux (bank+1) (i-reqs.(bank)) in
		aux 0 i in
	let alloc_perm_of_bank bank =
		let res = next_perm.(bank) in
		next_perm.(bank) <- succ next_perm.(bank) ;
		res in
	let init_plan plan_idx =
		(* Free the vars that will not be used any more. *)
		List.iter free_var_reg freelist_at_step.(plan_idx) ;
		(* Build plan *)
		let pp = preplan.(plan_idx) in
		let impl = pp.prealloc_impl in {
			impl = impl ;
			perm_regs = Array.init (nb_regs impl.perm)
				(fun i -> alloc_perm_of_bank (fst (bank_of impl.perm i))) ;
			scratch_regs = Array.init (nb_regs impl.scratch)
				(fun i ->
					let bank, reg_in_bank = bank_of impl.scratch i in
					(first_scratch bank) + reg_in_bank) ;
			out_regs = Array.init (Array.length pp.output_names) (fun i ->
				let sym_name = pp.output_names.(i) in
				let symbol = Hashtbl.find symbols sym_name in
				let free_slot = alloc_var_reg symbol.alloc_bank in
				let bank_num = symbol.alloc_bank in
				let reg_num = free_slot + (first_var bank_num) in
				freelist_at_step.(symbol.death + 1) <-
					(bank_num, free_slot) :: freelist_at_step.(symbol.death + 1) ;
				Printf.printf "Using register %d.%d for %s up to step %d\n" bank_num reg_num sym_name symbol.death ;
				reg_num) ;
			(* We cannot initialize in_regs for now, since we need to refer back to
			 * previously created plan entries. So we create an dummy array here and will finish
			 * initialization hereafter. *)
			in_regs = [||] } in
	let plan = Array.init (Array.length preplan) init_plan in
	(* Finish init of in_regs. *)
	for plan_idx = 0 to (Array.length plan) - 1 do
		let pp = preplan.(plan_idx) in
		plan.(plan_idx).in_regs <- Array.init (Array.length pp.input_names) (fun i ->
			let sym_name = pp.input_names.(i) in
			try int_of_string sym_name
			with Failure _ ->
				let symbol = Hashtbl.find symbols sym_name in
				let outputer_idx, output_idx = symbol.outputer in
				plan.(outputer_idx).out_regs.(output_idx))
	done ;
	plan

(* From a "user program" giving only the main ingredients, build a preplan
 * by cooking possible implementations, served with loop and func machinery.
 *)
type user_sym = string * data_type
type user_op = impl_lookup * string array * user_sym array
type user_plan = user_op array
type user_symbol = { mutable bank : int ; size : data_type }

let string_of_spec_in specs =
	let string_of_spec = function
		| Reg (bank, size) -> Printf.sprintf "Reg (bank=%d, size=%d)" bank size
		| Cst size -> Printf.sprintf "Cst size=%d" size
		| Auto size -> Printf.sprintf "Auto size=%d" size in
	(Array.fold_left (fun prefix spec -> prefix^(string_of_spec spec)^"; ") "[ " specs)^" ] "
let string_of_spec_out specs =
	let string_of_spec spec = Printf.sprintf "size=%d" spec in
	(Array.fold_left (fun prefix spec -> prefix^(string_of_spec spec)^"; ") "[ " specs)^" ] "

let make_preplan user_plan func_params =
	let func_param_names = Array.map fst func_params in
	let func_param_sizes = Array.map snd func_params in
	(* Returns a path of larger allowed scale.
	 * A "path" is a prealloc_plan for the loop body (ie user_plan). *)
	let make_path min_scale max_scale =
		let find_path scale =
			Printf.printf "Looking for path of scale %d.\n" scale ;
			(* Build a symbol table giving the expected size and register bank of symbols. *)
			let symbols = Hashtbl.create 10 in
			(* Symbols are known from function parameters and later output variables. *)
			let add_symbol name size bank =
				Printf.printf "Add symbol %s, bank=%d, size=%d.\n" name bank size ;
				Hashtbl.add symbols name { bank = bank ; size = size } in
			let add_symbols bank = Array.iter (fun (name, size) -> add_symbol name size bank) in
			add_symbols 0 func_params ;
			Array.map (fun (impl_lookup, inputs, outputs) ->
				(* Build input and output specifier. *)
				let specs_in = Array.map (fun sym_name ->
					(* Is it a constant ? *)
					try Cst (int_of_string sym_name)
					with Failure _ -> (	(* Or get info from the symbol table. *)
						let symbol = Hashtbl.find symbols sym_name in
						Reg (symbol.bank, symbol.size))) inputs in
				let specs_out = Array.map (fun (_, sym_size) -> sym_size) outputs in
				(* If we can't find it, we could still achieve the same result by repeating scale
				 * times the implementation for scale=1. *)
				Printf.printf "\tLooking for an impl @scale=%d, for specs = %s -> %s.\n" scale (string_of_spec_in specs_in) (string_of_spec_out specs_out) ;
				let impl = impl_lookup (scale, specs_in, specs_out) in
				Printf.printf "\tfound an impl giving %d outputs.\n" (Array.length impl.out_banks) ;
				(* Add new symbols for outputs. *)
				Array.iteri (fun i (name, size) ->
					add_symbol name size impl.out_banks.(i)) outputs ;
				(* Returns the prealloc_plan. *)
				{	prealloc_impl = impl ;
					input_names = inputs ;
					output_names = Array.map fst outputs }) user_plan in
		let rec aux scale =
			if scale < min_scale then raise Not_found
			else try (find_path scale), scale with Not_found -> (
				Printf.printf "No path for scale %d.\n" scale ;
				aux (scale - 1)) in
		aux max_scale in
	let preplan_of_path path =
		(* Merely add entry/exit points and loop head/tail. *)
		let preplan = Array.concat [
			[|	{	prealloc_impl = load_params (1, [||], func_param_sizes) ;
					input_names = [||] ; output_names = func_param_names } ;
				{	prealloc_impl = loop_head (1, [| Reg (1, 32) |], [||]) ;
					input_names = [| "width" |] ; output_names = [||] } |] ;
			path ;
			[|	{	prealloc_impl = loop_tail (1, [||], [||]) ;
					input_names = [||] ; output_names = [||] } |] ] in
		let loops = [ 1, (Array.length preplan)-1 ] in
		preplan, loops in
	let path_with_renamed_vars suffix path =
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
				output_names = (renamed p.output_names) }) path in
	let combine_paths slow_path fast_path scale =
		(* FIXME: we do not take into account data alignment here. but how can we ? *)
		let preplan = Array.concat [
			[|	{	prealloc_impl = load_params (scale, [||], func_param_sizes) ;
					input_names = [||] ; output_names = func_param_names } ;
				{	prealloc_impl = loop_head (scale, [| Reg (1, 32) |], [||]) ;
					input_names = [| "width" |] ; output_names = [||] } |] ;
			path_with_renamed_vars "[fast]" fast_path ;
			[|	{	prealloc_impl = loop_tail (scale, [||], [||]) ;
					input_names = [||] ; output_names = [||] } ;
				{	prealloc_impl = loop_head (1, [| Reg (1, 32) |], [||]) ;
					input_names = [| "width" |] ; output_names = [||] } |] ;
			path_with_renamed_vars "[slow_finish]" slow_path ;
			[|	{	prealloc_impl = loop_tail (1, [||], [||]) ;
					input_names = [||] ; output_names = [||] } |] ] in
		let loop_tail_1 = Array.length fast_path + 2 in
		let loops = [ 1, loop_tail_1 ; loop_tail_1 + 1, (Array.length preplan)-1 ] in
		preplan, loops in
	(* First, build the slow path (1 item at a time) *)
	let slow_path, _ = make_path 1 1 in
	(* Then look for a fast path (N items at a time) *)
	try (
		let fast_path, scale = make_path 2 8 in
		(* Then build the final path using both slow and fast paths. *)
		combine_paths slow_path fast_path scale
	) with Not_found -> (
		(* Or using only the slow path if no fast path was found. *)
		preplan_of_path slow_path
	)

let () =
	let proc = make_proc 3 in
	(* User program. *)
	let user_program = [|
		stream_read, [| "pixels" |], [| "pixel", 16 |] ;
		unpack565, [| "pixel" |], [| "r", 8 ; "g", 8 ; "b", 8 |] ;
		stream_read, [| "alpha" |], [| "a", 8 |] ;
		mul_rshift, [| "r" ; "a" ; "8" |], [| "r'", 8 |] ;
		mul_rshift, [| "g" ; "a" ; "8" |], [| "g'", 8 |] ;
		mul_rshift, [| "b" ; "a" ; "8" |], [| "b'", 8 |] ;
		pack565, [| "r'" ; "g'" ; "b'" |], [| "pixel'", 16 |] ;
		stream_write, [| "image" ; "pixel'" |], [||] |] in
	let func_params = [| "width", 32 ; "pixels", 32 ; "alpha", 32 ; "image", 32 |] in
	let preplan, loops = make_preplan user_program func_params in
	(* Register allocation. *)
	let plan = reg_allocation preplan loops in
	(* Function exec plan. *)
	(* Emit entry point *)
	emit_entry_point proc [||] ;
	(* Emit loop invariants *)
	Array.iter (fun { impl=impl; perm_regs=perm_regs } -> impl.preamble_emitter proc perm_regs) plan ;
	(* Emit function body *)
	Array.iter (fun { impl=impl; in_regs=in_regs; scratch_regs=scratch_regs; out_regs=out_regs} ->
		impl.emitter proc in_regs scratch_regs out_regs) plan ;
	(* Emit exit code *)
	emit_exit proc ;
	(* Copy pixels and alpha into memory *)
	let nb_pixels = 2 in
	let pixels_addr = 0 in
	let alpha_addr = pixels_addr + nb_pixels*2 in
	let image_addr = alpha_addr + nb_pixels*1 in
	memory_write_32 pixels_addr (big_int_of_int 0x12345678) ;
	assert (eq_big_int (big_int_of_int 0x12345678) (memory_read_32 pixels_addr)) ;
	memory_write_16 alpha_addr (big_int_of_int 0x8040) ; (* first pixel is /4 and second is /2 *)
	(* Run *)
	exec proc (Array.map word_of_int [|nb_pixels ; pixels_addr ; alpha_addr ; image_addr|]) ;
	(* Show result *)
	let result = memory_read_32 image_addr in
	Printf.printf "Image = %x\n" (int_of_string (string_of_big_int result)) ;
	assert (eq_big_int result (big_int_of_int 0x90a1186))

