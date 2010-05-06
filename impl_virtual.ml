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

external peek_byte :
	nativeint -> int -> int = "wrap_peek_byte"
external poke_byte :
	nativeint -> int -> int -> unit = "wrap_poke_byte"

let memory_read_8 addr' offset =
	let addr = nativeint_of_big_int addr' in
	let v = peek_byte addr offset in
	Printf.printf "mem.(%nx+%d) -> %d\n" addr offset v ;
	big_int_of_int v
let memory_write_8 addr' offset value =
	let addr = nativeint_of_big_int addr' in
	let v = int_of_big_int (land_big_int value (big_int_of_int 0xff)) in
	Printf.printf "mem.(%nx+%d) <- %d\n" addr offset v ;
	poke_byte addr offset v

let memory_read_seq num addr offset =
	let rec aux value dec =
		if dec >= num then value
		else aux (lor_big_int value
			(lsl_big_int (memory_read_8 addr (offset + dec)) (dec * 8))) (dec+1) in
	aux zero_big_int 0

let memory_write_seq num addr offset value =
	let rec aux dec =
		if dec >= num then ()
		else (
			memory_write_8 addr (offset + dec) (lsr_big_int value (dec * 8)) ;
			aux (dec + 1)) in
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
	let int_of_word = Int32.to_int
	let word_of_string = Int32.of_string
	let string_of_word = Int32.to_string
	let nativeint_of_word = Nativeint.of_int32

	type initer = Param of int | Const of word
	(* loop_descr is used to remember where a loop started and where it stops *) 
	type loop_descr_record = { start : int ; mutable quit : int ; top : loop_descr }
	and loop_descr = loop_descr_record option
	type instruction = unit -> unit
	type proc =
		{ mutable pc : int ;	(* program counter *)
		  mutable loops : loop_descr ;	(* used only when emitting code not when running *)
		  mutable code_size : int ;
		  mutable params : nativeint array ;	(* used by load_param operation *)
		  mutable auto : word array ;	(* auto storage *)
		  (* We use an array for code since we use index in this array as loop labels *)
		  code : instruction array }

	type emitter = proc -> (string -> reg_id) -> unit
	
	type op_impl =
		{ helpers : (helper_kind * string * op_impl) array ;
		  out_banks : bank_num array ;
		  emitter : emitter }

	type spec_in =
		| Reg of (bank_num * data_type)
		| Cst of word
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

	let reg_write (bank, r) v_ =
		let v = land_big_int v_ register_masks.(bank) in
		Printf.printf "reg.(%d).(%d) <- %s\n" bank r (string_of_big_int v) ;
		regs.(bank).(r) <- v
	
	let reg_read (bank, r) = regs.(bank).(r)
	
	let add_code proc f =
		proc.code.(proc.code_size) <- f ;
		proc.code_size <- proc.code_size + 1

	(* Misc *)
	let unopt = function
		| None -> failwith "Nothing where something was expected."
		| Some x -> x

	(* Helpers *)
	(* We need a var as the clock source for our loops and stream reads/writes. *)
	let clock_var = Invariant, "clock",
		{ out_banks = [| 0 |] ; helpers = [||] ;
		  emitter = (fun proc g ->
		  	add_code proc (fun () -> reg_write (g "clock") zero_big_int)) }

	(* Implemented Operations. *)

	let add = function
		| 1, [| Reg (bank, (sz, _)) ; Reg (bank', (sz', _)) |], [| (sz'', _) |]
			when bank = bank' &&
				sz = sz' && sz = sz'' && sz = register_sizes.(bank) -> {
			out_banks = [| bank |] ; helpers = [||] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0") (add_big_int (reg_read (g "<0")) (reg_read (g "<1"))))) }
		| _ -> raise Not_found

	let mul_rshift = function
		| 1, [| Reg (bank, (sz, sign)) ; Reg (bank', (sz', sign')) ; Cst shift |], [| (sz'', sign'') |]
			when bank = bank' &&
				sz = sz' && sz = sz'' &&
				bank < nb_banks && sz <= register_sizes.(bank) &&
				sign = sign' && sign = sign'' -> {
			out_banks = [| bank |] ; helpers = [||] ;
			emitter = (fun proc g ->
				add_code proc (fun () ->
					reg_write (g ">0")
						(big_int_mul_shift
							(land_big_int (reg_read (g "<0")) (big_int_mask sz))
							(land_big_int (reg_read (g "<1")) (big_int_mask sz))
							(int_of_word shift)))) }
		| scale, [| Reg (1, (sz, sign)) ; Reg (1, (sz', sign')) ; Cst shift |], [| (sz'', sign'') |]
			when scale * sz <= register_sizes.(1) &&
				sz = sz' && sz = sz'' &&
				sign = sign' && sign = sign'' -> {
			out_banks = [| 1 |] ; helpers = [||] ;
			emitter = (fun proc g ->
				add_code proc (fun () ->
					let parts_a = big_int_unpack scale sz (reg_read (g "<0")) in
					let parts_b = big_int_unpack scale sz (reg_read (g "<1")) in
					let results = List.map2 (fun a b ->
						big_int_mul_shift a b (int_of_word shift)) parts_a parts_b in
					reg_write (g ">0") (big_int_pack sz results))) }
		| _ -> raise Not_found

	let pack565 specs =
		let pack565_helper r g b out_reg scale =
			let pack_r v = lsl_big_int (land_big_int v (big_int_of_int 0x1f)) 11 in
			let pack_g v = lsl_big_int (land_big_int v (big_int_of_int 0x3f)) 5 in
			let pack_b v = land_big_int v (big_int_of_int 0x1f) in
			let out_v = ref zero_big_int in
			for s = 0 to scale - 1 do
				let r = lsr_big_int ((reg_read r)) (s*8) in
				let g = lsr_big_int ((reg_read g)) (s*8) in
				let b = lsr_big_int ((reg_read b)) (s*8) in
				let new_pack = add_big_int (pack_r r) (add_big_int (pack_g g) (pack_b b)) in
				out_v := add_big_int !out_v (lsl_big_int new_pack (s*8))
			done ;
			reg_write out_reg !out_v in
		match specs with
		| scale, [| Reg (0, (8, _)) ; Reg (0, (8, _)) ; Reg (0, (8, _)) |], [| 16, _ |] when scale <= 2 -> {
			out_banks = [| 0 |] ; helpers = [||] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				pack565_helper (g "<0") (g "<1") (g "<2") (g ">0") scale)) }
		| scale, [| Reg (1, (8, _)) ; Reg (1, (8, _)) ; Reg (1, (8, _)) |], [| 16, _ |] when scale <= 8 -> {
			out_banks = [| 1 |] ; helpers = [||] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				pack565_helper (g "<0") (g "<1") (g "<2") (g ">0") scale)) }
		| _ -> raise Not_found


	let unpack565 specs =
		let unpack565_helper pixel_reg r g b scale =
			let unpack_r v = land_big_int (lsr_big_int v 11) (big_int_of_int 0x1f) in
			let unpack_g v = land_big_int (lsr_big_int v 5) (big_int_of_int 0x3f) in
			let unpack_b v = land_big_int v (big_int_of_int 0x1f) in
			let cols = Array.make 3 zero_big_int in
			for s = 0 to scale - 1 do
				let color = lsr_big_int (reg_read pixel_reg) (s*16) in
				cols.(0) <- add_big_int cols.(0) (lsl_big_int (unpack_r color) (s*8)) ;
				cols.(1) <- add_big_int cols.(1) (lsl_big_int (unpack_g color) (s*8)) ;
				cols.(2) <- add_big_int cols.(2) (lsl_big_int (unpack_b color) (s*8))
			done ;
			reg_write r cols.(0) ;
			reg_write g cols.(1) ;
			reg_write b cols.(2) in
		match specs with
		| scale, [| Reg (0, (16, _)) |], [| 8, _ ; 8, _ ; 8, _ |] when scale <= 2 -> {
			out_banks = [| 0 ; 0 ; 0 |] ; helpers = [||] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				unpack565_helper (g "<0") (g ">0") (g ">1") (g ">2") scale)) }
		| scale, [| Reg (1, (16, _)) |], [| 8, _ ; 8, _ ; 8, _ |] when scale <= 8 -> {
			out_banks = [| 1 ; 1 ; 1 |] ; helpers = [||] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				unpack565_helper (g "<0") (g ">0") (g ">1") (g ">2") scale)) }
		| _ -> raise Not_found

	let stream_read_aligned = function
		| scale, [| Reg (0, (32, Unsigned)) |], [| 8, _ |] when scale <= 4 -> {
			out_banks = [| 0 |] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0")
					(memory_read_seq scale (reg_read (g "<0"))
					(int_of_big_int (reg_read (g "clock")))))) }
		| scale, [| Reg (0, (32, Unsigned)) |], [| 16, _ |] when scale <= 2 -> {
			out_banks = [| 0 |] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0")
					(memory_read_seq (scale*2) (reg_read (g "<0"))
					(2 * (int_of_big_int (reg_read (g "clock"))))))) }
		| 1, [| Reg (0, (32, Unsigned)) |], [| 32, _ |] -> {
			out_banks = [| 0 |] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0")
					(memory_read_32 (reg_read (g "<0"))
					(4 * (int_of_big_int (reg_read (g "clock"))))))) }
		| scale, [| Reg (0, (32, Unsigned)) |], [| 8, _ |] when scale <= 16 -> {
			out_banks = [| 1 |] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0")
					(memory_read_seq scale (reg_read (g "<0"))
					(int_of_big_int (reg_read (g "clock")))))) }
		| scale, [| Reg (0, (32, Unsigned)) |], [| 16, _ |] when scale <= 8 -> {
			out_banks = [| 1 |] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0")
					(memory_read_seq (scale*2)
						(reg_read (g "<0"))
						(2 * (int_of_big_int (reg_read (g "clock"))))))) }
		| scale, [| Reg (0, (32, Unsigned)) |], [| 32, _ |] when scale <= 4 -> {
			out_banks = [| 0 |] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				reg_write (g ">0")
					(memory_read_seq (scale*4)
						(reg_read (g "<0"))
						(4 * (int_of_big_int (reg_read (g "clock"))))))) }
		| _ -> raise Not_found

	let stream_write_aligned = function
		| scale, [| Reg (0, (32, Unsigned)) ; Reg (0, (8, _)) |], [||] when scale <= 4 -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				memory_write_seq scale
					(reg_read (g "<0"))
					(int_of_big_int (reg_read (g "clock")))
					(reg_read (g "<1")))) }
		| scale, [| Reg (0, (32, Unsigned)) ; Reg (0, (16, _)) |], [||] when scale <= 2 -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				memory_write_seq (2*scale)
					(reg_read (g "<0"))
					(2 * (int_of_big_int (reg_read (g "clock"))))
					(reg_read (g "<1")))) }
		| 1, [| Reg (0, (32, Unsigned)) ; Reg (0, (32, _)) |], [||] -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				memory_write_32
					(reg_read (g "<0"))
					(4 * (int_of_big_int (reg_read (g "clock"))))
					(reg_read (g "<1")))) }
		| scale, [| Reg (0, (32, Unsigned)) ; Reg (1, (8, _)) |], [||] when scale <= 16 -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				memory_write_seq scale
					(reg_read (g "<0"))
					(int_of_big_int (reg_read (g "clock")))
					(reg_read (g "<1")))) }
		| scale, [| Reg (0, (32, Unsigned)) ; Reg (1, (16, _)) |], [||] when scale <= 8 -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				memory_write_seq (2*scale)
					(reg_read (g "<0"))
					(2 * (int_of_big_int (reg_read (g "clock"))))
					(reg_read (g "<1")))) }
		| scale, [| Reg (0, (32, Unsigned)) ; Reg (1, (32, _)) |], [||] when scale <= 4 -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g -> add_code proc (fun () ->
				memory_write_seq (4*scale)
					(reg_read (g "<0"))
					(4 * (int_of_big_int (reg_read (g "clock"))))
					(reg_read (g "<1")))) }
		| _ -> raise Not_found

	let loop_head = function
		| scale, [| Reg (0, (sz, _)) |], [||] when sz <= register_sizes.(0)-> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g ->
				let loop = { start = proc.code_size ; quit = 0 ; top = proc.loops } in
				proc.loops <- Some loop ;
				add_code proc (fun () ->
					Printf.printf "(Re)Entering loop [%d -> %d].\n" loop.start loop.quit ;
					if gt_big_int
						(add_big_int (reg_read (g "clock")) (big_int_of_int scale))
						(reg_read (g "<0"))
					then (
						proc.pc <- loop.quit ;
						Printf.printf "Leaving loop [%d -> %d].\n" loop.start loop.quit))) } 
		| _ -> raise Not_found

	let loop_tail = function
		| scale, [||], [||] -> {
			out_banks = [||] ; helpers = [| clock_var |] ;
			emitter = (fun proc g ->
				let loop = unopt proc.loops in
				let loop_start = loop.start in
				add_code proc (fun () ->
					reg_write (g "clock")
						(add_big_int (big_int_of_int scale) (reg_read (g "clock"))) ;
					proc.pc <- loop_start) ;
				loop.quit <- proc.code_size ;
				proc.loops <- loop.top) }
		| _ -> raise Not_found

	let load_param = function
		| _, [| Cst p |], [| sz, _ |] when sz <= register_sizes.(0) ->
			{ out_banks = [| 0 |] ; helpers = [||] ;
			  emitter = (fun proc g -> add_code proc (fun () ->
				let value = big_int_of_nativeint proc.params.(int_of_word p) in
			  	reg_write (g ">0") value)) }
		| _ -> raise Not_found

	(* Returns the context used by emitters. *)
	let make_proc _nb_sources =
		let nop () = () in {
			pc = 0 ;
			loops = None ;
			code_size = 0 ;
			params = [||] ;
			auto = [||] ;
			code = Array.make 100 nop }

	let emit_entry_point proc inits _used_regs =
		proc.auto <- Array.make (Array.length inits) Int32.zero ;
		Array.iteri (fun i -> function
			| Param p -> add_code proc (fun () -> proc.auto.(i) <- Nativeint.to_int32 proc.params.(p))
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
			Printf.printf "Exec function at pc=%d\n%!" pc ;
			let f = proc.code.(pc) in
			proc.pc <- succ pc ;
			f () ;
			aux proc.pc in
		try aux 0 with Exit -> Printf.printf "Exit from proc\n"

end
