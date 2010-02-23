(* We simulate a (little endian) machine with 2 sets of registers, with 32 and 128 bits regs. *)

open Big_int

(* Some addition to Big_int *)

let lsr_big_int bi shift = div_big_int bi (power_int_positive_int 2 shift)

let lsl_big_int bi shift = mult_big_int bi (power_int_positive_int 2 shift)

let land_big_int, lor_big_int =
	let big_2 = big_int_of_int 2 in
	let cond_or  ba bb = eq_big_int ba unit_big_int || eq_big_int bb unit_big_int in
	let cond_and ba bb = eq_big_int ba unit_big_int && eq_big_int bb unit_big_int in
	let rec aux res power cond a b =
		if eq_big_int a zero_big_int && eq_big_int b zero_big_int then res
		else (
			let a', ba = quomod_big_int a big_2 in
			let b', bb = quomod_big_int b big_2 in
			let new_res =
				if cond ba bb then add_big_int res power
				else res in
			aux new_res (mult_big_int power big_2) cond a' b') in
	aux zero_big_int unit_big_int cond_and, aux zero_big_int unit_big_int cond_or

let big_int_mul_shift a b shift = lsr_big_int (mult_big_int a b) shift

let big_int_mask nb_bits = pred_big_int (power_int_positive_int 2 nb_bits)

let big_int_unpack nb_packs pack_sz v =
	let mask = big_int_mask pack_sz in
	let rec aux prevs v rem_packs =
		if rem_packs <= 0 then prevs
		else aux ((land_big_int v mask) :: prevs) (lsr_big_int v pack_sz) (rem_packs - 1) in
	aux [] v nb_packs

let big_int_pack pack_sz vals =
	let rec aux v = function
		| [] -> v
		| p :: others ->
			aux (add_big_int (lsl_big_int v pack_sz) p) others in
	aux zero_big_int vals

(* Memory *)

let memory_size = 1024
let memory = Array.make memory_size 0

let memory_read_8 addr = big_int_of_int (memory.(addr) land 0xff)
let memory_write_8 addr value =
	let v = (int_of_big_int value) land 0xff in
	Printf.printf "mem.(%d) <- 0x%x\n" addr v ;
	memory.(addr) <- v

let memory_read_seq num addr =
	let rec aux value offset =
		if offset >= num then value
		else aux (lor_big_int value
			(lsl_big_int (memory_read_8 (addr + offset)) (offset * 8))) (offset+1) in
	aux zero_big_int 0

let memory_write_seq num addr value =
	let rec aux offset =
		if offset >= num then ()
		else (
			memory_write_8 (addr + offset) (lsr_big_int value (offset * 8)) ;
			aux (offset + 1)) in
	aux 0

let memory_read_16 = memory_read_seq 2
let memory_write_16 = memory_write_seq 2
let memory_read_32 = memory_read_seq 4
let memory_write_32 = memory_write_seq 4

open Jiton

module Virtual : IMPLEMENTER =
struct
	type word = int32
	let word_of_int = Int32.of_int

	type initer = Param of int | Const of word
	(* loop_descr is used to remember where a loop started and where it stops *) 
	type loop_descr_record = { start : int ; mutable quit : int ; top : loop_descr }
	and loop_descr = loop_descr_record option
	type instruction = unit -> unit
	type proc =
		{ mutable pc : int ;	(* program counter *)
		  mutable loops : loop_descr ;	(* used only when emitting code not when running *)
		  mutable code_size : int ;
		  mutable params : word array ;	(* used by load_param operation *)
		  mutable clock_reg : int ;	(* the number of our perm register that store loop counter *)
		  mutable auto : word array ;	(* auto storage *)
		  (* We use an array for code since we use index in this array as loop labels *)
		  code : instruction array }

	type op_impl =
		{ scratch : int array ;
		  perm : int array ;
		  out_banks : bank_num array ;
		  preamble_emitter : proc -> int array (* perm regs *) -> unit ;
		  emitter : proc ->
		  	int array (* input regs *) ->
		  	int array (* scratch regs *) ->
		  	int array (* output regs *) ->
		  	unit }

	type impl_lookup = scale * spec_in array * spec_out array -> op_impl

	(* Registers *)
	let register_sets = [|
		10 (* number of general registers *) ;
		32 (* number of simd registers *) |]

	let register_sizes = [| 32 ; 128 |]
	let register_masks = Array.map (fun sz -> pred_big_int (power_int_positive_int 2 sz)) register_sizes

	let nb_banks = Array.length register_sets
	let _ = assert (nb_banks = Array.length register_sizes)

	let regs = Array.init nb_banks (fun bank -> Array.make register_sets.(bank) zero_big_int)

	let reg_write ?(bank=0) r v_ =
		let v = land_big_int v_ register_masks.(bank) in
		Printf.printf "reg.(%d).(%d) <- %s\n" bank r (string_of_big_int v) ;
		regs.(bank).(r) <- v

	let add_code proc f =
		proc.code.(proc.code_size) <- f ;
		proc.code_size <- proc.code_size + 1

	(* Misc *)
	let unopt = function
		| None -> failwith "Nothing where something was expected."
		| Some x -> x

	let min_data_size cst =
		let rec aux size max_cst =
			if cst < max_cst then size
			else aux (size+1) (max_cst lsl 1) in
		aux 0 1

	(* Implemented Operations. *)

	let add = function
		| 1, [| Reg (bank, sz) ; Reg (bank', sz') |], [| sz'' |]
			when bank = bank' &&
				sz = sz' && sz = sz'' && sz = register_sizes.(bank) -> {
			scratch = [||] ; perm = [||] ; out_banks = [| bank |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(0) (add_big_int regs.(0).(ins.(0)) regs.(0).(ins.(1))))) }
		| _ -> raise Not_found

	let mul_rshift = function
		| 1, [| Reg (bank, sz) ; Reg (bank', sz') ; Cst shift |], [| sz'' |]
			when bank = bank' &&
				sz = sz' && sz = sz'' &&
				bank < nb_banks && sz <= register_sizes.(bank) -> {
			scratch = [||] ; perm = [||] ; out_banks = [| bank |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs ->
				assert (shift = ins.(2)) ;
				add_code proc (fun () ->
					reg_write ~bank:bank outs.(0)
						(big_int_mul_shift
							(land_big_int regs.(bank).(ins.(0)) (big_int_mask sz))
							(land_big_int regs.(bank).(ins.(1)) (big_int_mask sz))
							shift))) }
		| scale, [| Reg (1, sz) ; Reg (1, sz') ; Cst shift |], [| sz'' |]
			when scale * sz <= register_sizes.(1) &&
				sz = sz' && sz = sz'' -> {
			scratch = [| 0 |] ; perm = [||] ; out_banks = [| 1 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs ->
				assert (shift = ins.(2)) ;
				add_code proc (fun () ->
					let bank = 1 in
					let parts_a = big_int_unpack scale sz regs.(bank).(ins.(0)) in
					let parts_b = big_int_unpack scale sz regs.(bank).(ins.(1)) in
					let results = List.map2 (fun a b ->
						big_int_mul_shift a b shift) parts_a parts_b in
					reg_write ~bank:bank outs.(0) (big_int_pack sz results))) }
		| _ -> raise Not_found

	let pack565 specs =
		let pack565_helper col_regs out_reg scale bank =
			let pack_r v = lsl_big_int (land_big_int v (big_int_of_int 0x1f)) 11 in
			let pack_g v = lsl_big_int (land_big_int v (big_int_of_int 0x3f)) 5 in
			let pack_b v = land_big_int v (big_int_of_int 0x1f) in
			let out_v = ref zero_big_int in
			for s = 0 to scale - 1 do
				let r = lsr_big_int (regs.(bank).(col_regs.(0))) (s*8) in
				let g = lsr_big_int (regs.(bank).(col_regs.(1))) (s*8) in
				let b = lsr_big_int (regs.(bank).(col_regs.(2))) (s*8) in
				let new_pack = add_big_int (pack_r r) (add_big_int (pack_g g) (pack_b b)) in
				out_v := add_big_int !out_v (lsl_big_int new_pack (s*8))
			done ;
			reg_write ~bank:bank out_reg !out_v in
		match specs with
		| scale, [| Reg (0, 8) ; Reg (0, 8) ; Reg (0, 8) |], [| 16 |] when scale <= 2 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				pack565_helper ins outs.(0) scale 0)) }
		| scale, [| Reg (1, 8) ; Reg (1, 8) ; Reg (1, 8) |], [| 16 |] when scale <= 8 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 1 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				pack565_helper ins outs.(0) scale 1)) }
		| _ -> raise Not_found


	let unpack565 specs =
		let unpack565_helper pixel_reg out_regs scale bank =
			let unpack_r v = land_big_int (lsr_big_int v 11) (big_int_of_int 0x1f) in
			let unpack_g v = land_big_int (lsr_big_int v 5) (big_int_of_int 0x3f) in
			let unpack_b v = land_big_int v (big_int_of_int 0x1f) in
			let cols = Array.make 3 zero_big_int in
			for s = 0 to scale - 1 do
				let color = lsr_big_int regs.(bank).(pixel_reg) (s*16) in
				cols.(0) <- add_big_int cols.(0) (lsl_big_int (unpack_r color) (s*8)) ;
				cols.(1) <- add_big_int cols.(1) (lsl_big_int (unpack_g color) (s*8)) ;
				cols.(2) <- add_big_int cols.(2) (lsl_big_int (unpack_b color) (s*8))
			done ;
			Array.iteri (fun i reg -> reg_write ~bank:bank reg cols.(i)) out_regs in
		match specs with
		| scale, [| Reg (0, 16) |], [| 8 ; 8 ; 8 |] when scale <= 2 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 ; 0 ; 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				unpack565_helper ins.(0) outs scale 0)) }
		| scale, [| Reg (1, 16) |], [| 8 ; 8 ; 8 |] when scale <= 8 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 1 ; 1 ; 1 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				unpack565_helper ins.(0) outs scale 1)) }
		| _ -> Printf.printf "No impl of unpack565.\n" ; raise Not_found

	let var_read = function
		| 1, [| Reg (0, 32) |], [| 8 |] -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(0) (memory_read_8 (int_of_big_int regs.(0).(ins.(0)))))) }
		| 1, [| Reg (0, 32) |], [| 32 |] -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(0) (memory_read_32 (int_of_big_int regs.(0).(ins.(0)))))) }
		| _ -> raise Not_found

	let var_write = function
		| 1, [| Reg (0, 32) ; Reg (0, 8) |], [||] -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_8 (int_of_big_int regs.(0).(ins.(0))) regs.(0).(ins.(1)))) }
		| 1, [| Reg (0, 32) ; Reg (0, 32) |], [||] -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_32 (int_of_big_int regs.(0).(ins.(0))) regs.(0).(ins.(1)))) }
		| _ -> raise Not_found

	let stream_read = function
		| scale, [| Reg (0, 32) |], [| 8 |] when scale <= 4 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(0)
					(memory_read_seq scale ((int_of_big_int regs.(0).(ins.(0))) + (int_of_big_int regs.(0).(proc.clock_reg)))))) }
		| scale, [| Reg (0, 32) |], [| 16 |] when scale <= 2 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(0)
					(memory_read_seq (scale*2) ((int_of_big_int regs.(0).(ins.(0))) + 2 * (int_of_big_int regs.(0).(proc.clock_reg)))))) }
		| 1, [| Reg (0, 32) |], [| 32 |] -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(0)
					(memory_read_32 ((int_of_big_int regs.(0).(ins.(0))) + 4 * (int_of_big_int regs.(0).(proc.clock_reg)))))) }
		| scale, [| Reg (0, 32) |], [| 8 |] when scale <= 16 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 1 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(1)
					(memory_read_seq scale ((int_of_big_int regs.(0).(ins.(0))) + (int_of_big_int regs.(0).(proc.clock_reg)))))) }
		| scale, [| Reg (0, 32) |], [| 16 |] when scale <= 8 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 1 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(1)
					(memory_read_seq (scale*2)
						((int_of_big_int regs.(0).(ins.(0))) + 2 * (int_of_big_int regs.(0).(proc.clock_reg)))))) }
		| scale, [| Reg (0, 32) |], [| 32 |] when scale <= 4 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch outs -> add_code proc (fun () ->
				reg_write outs.(1)
					(memory_read_seq (scale*4)
						((int_of_big_int regs.(0).(ins.(0))) + 4 * (int_of_big_int regs.(0).(proc.clock_reg)))))) }
		| _ -> raise Not_found

	let stream_write = function
		| scale, [| Reg (0, 32) ; Reg (0, 8) |], [||] when scale <= 4 -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_seq scale
					((int_of_big_int regs.(0).(ins.(0))) + (int_of_big_int regs.(0).(proc.clock_reg)))
					regs.(0).(ins.(1)))) }
		| scale, [| Reg (0, 32) ; Reg (0, 16) |], [||] when scale <= 2 -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_seq (2*scale)
					((int_of_big_int regs.(0).(ins.(0))) + 2 * (int_of_big_int regs.(0).(proc.clock_reg)))
					regs.(0).(ins.(1)))) }
		| 1, [| Reg (0, 32) ; Reg (0, 32) |], [||] -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_32
					((int_of_big_int regs.(0).(ins.(0))) + 4 * (int_of_big_int regs.(0).(proc.clock_reg)))
					regs.(0).(ins.(1)))) }
		| scale, [| Reg (0, 32) ; Reg (1, 8) |], [||] when scale <= 16 -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_seq scale
					((int_of_big_int regs.(0).(ins.(0))) + (int_of_big_int regs.(0).(proc.clock_reg)))
					regs.(1).(ins.(1)))) }
		| scale, [| Reg (0, 32) ; Reg (1, 16) |], [||] when scale <= 8 -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_seq (2*scale)
					((int_of_big_int regs.(0).(ins.(0))) + 2 * (int_of_big_int regs.(0).(proc.clock_reg)))
					regs.(1).(ins.(1)))) }
		| scale, [| Reg (0, 32) ; Reg (1, 32) |], [||] when scale <= 4 -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs -> add_code proc (fun () ->
				memory_write_seq (4*scale)
					((int_of_big_int regs.(0).(ins.(0))) + 4 * (int_of_big_int regs.(0).(proc.clock_reg)))
					regs.(1).(ins.(1)))) }
		| _ -> raise Not_found

	let loop_head = function
		| scale, [| Reg (1, 32) |], [||] -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc ins _scratch _outs ->
				let loop = { start = proc.code_size ; quit = 0 ; top = proc.loops } in
				proc.loops <- Some loop ;
				add_code proc (fun () ->
					Printf.printf "(Re)Entering loop [%d -> %d].\n" loop.start loop.quit ;
					if gt_big_int
						(add_big_int regs.(0).(proc.clock_reg) (big_int_of_int scale))
						regs.(0).(ins.(0))
					then (
						proc.pc <- loop.quit ;
						Printf.printf "Leaving loop [%d -> %d].\n" loop.start loop.quit))) } 
		| _ -> raise Not_found

	let loop_tail = function
		| scale, [||], [||] -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = (fun _proc _perms -> ()) ;
			emitter = (fun proc _ins _scratch _outs ->
				let loop = unopt proc.loops in
				let loop_start = loop.start in
				add_code proc (fun () ->
					reg_write proc.clock_reg
						(add_big_int (big_int_of_int scale) regs.(0).(proc.clock_reg)) ;
					proc.pc <- loop_start) ;
				loop.quit <- proc.code_size ;
				proc.loops <- loop.top) }
		| _ -> raise Not_found

	(* This one is special. It's emitted before the loop just after entry_point to load all
	 * function parameters into registers.
	 * With regards to the plan, it has no inputs and as many outputs as used parameters. *)
	let load_params (scale, ins, outs) =
		assert (ins = [||]) ;
		ignore scale ;	(* For constants, load them scale times ? *)
		{
			scratch = [||] ; perm = [| 1 |] ; out_banks = Array.make (Array.length outs) 0 ;
			(* We use a perm register as the clock source for our loops. *)
			preamble_emitter = (fun proc perms ->
				proc.clock_reg <- perms.(0) ;
				add_code proc (fun () -> reg_write proc.clock_reg zero_big_int)) ;
			(* FIXME: if we have no constant but only registers, do this ;
			 * otherwise code must init regs with constant as well. *)
			emitter = (fun proc _ins _scratch outs -> add_code proc (fun () ->
				Array.iteri (fun i out_reg -> reg_write out_reg (big_int_of_int32 proc.params.(i))) outs)) }

	(* Returns the context used by emitters. *)
	let make_proc _nb_sources =
		let nop () = () in {
			pc = 0 ;
			loops = None ;
			code_size = 0 ;
			clock_reg = 0 ;
			params = [||] ;
			auto = [||] ;
			code = Array.make 100 nop }

	let emit_entry_point proc inits =
		proc.auto <- Array.make (Array.length inits) Int32.zero ;
		Array.iteri (fun i -> function
			| Param p -> add_code proc (fun () -> proc.auto.(i) <- proc.params.(p))
			| Const c -> add_code proc (fun () -> proc.auto.(i) <- c)) inits

	let emit_exit proc = add_code proc (fun () -> raise Exit)

	(* Source values are given as integers. *)
	let exec proc params =
		Printf.printf "Start execution of function which code_size=%d\n" proc.code_size ;
		proc.params <- params ;
		let rec to_top_loop prev = function
			| None -> prev
			| Some loop -> to_top_loop (Some loop) loop.top in
		proc.loops <- to_top_loop None proc.loops ;
		let rec aux pc =
			Printf.printf "Exec function at pc=%d\n" pc ;
			let f = proc.code.(pc) in
			proc.pc <- succ pc ;
			f () ;
			aux proc.pc in
		try aux 0 with Exit -> Printf.printf "Exit from proc\n"

end
