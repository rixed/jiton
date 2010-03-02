(* Loongson is basically a MIPS3 architecture, with the addition of some
 * multimedia instructions.  It thus have :
 *
 * - 32 64 bits wide general purpose registers, of which 30 can be used freely
 * as reg 0 has a fixed content of 0 and reg 29 is used as the stack pointer.
 * On procedure entry we save register 31 (which holds the return address)
 * onto the stack so that it became available for processing. HI and LO
 * registers does not appear as available registers of course, even if they are
 * used to implement some operations. We don't care about reg 30 (frame pointer)
 * that we use as a disposable register.
 *
 * - 32 64 bits wide general floating point registers, all of which being
 * available (denoted by prefix "f").
 *
 * SIMD instructions can perform integer operations on data of 8, 16, 32 or 64
 * bits in size, using the floating point registers.
 *
 * Loongson supports only little endian mode (which is the good one).
 * All memory accesses must be aligned by a multiple of the data size accessed.
 *
 * Functions are called (and returned from) according to the N32 ABI, which is
 * summarized here :
 *
 * - Stack holds 64bits values
 * - we use registers 4 to 11 to pass integers arguments
 * - we use FP registers f12 to f19 to pass FP arguments
 * - these registers are "allocated" even if not used. For instance, prototype
 *   int, float, int will use register 4, f13, 6
 * - registers 16 to 23, as well a 28 to 30 are callee saved
 * - FP registers f20 to f31 are callee saved
 *
 * A map of the general purpose registers may help :
 * 
 * 0 -> zero
 * 1..11 -> used to pass arguments
 * 12..15 ->
 * 16..23 -> callee-saved
 * 24..27 ->
 * 28 -> callee-saved
 * 29 -> stack pointer, callee-saved
 * 30 -> (frame pointer), callee-saved
 * 31 -> (return address), callee-saved
 *
 * Note : for now we suppose that we took only integer parameters, which seams reasonable
 * as we expect mainly pointers to buffers and a width.
 *)

open Jiton

module rec Codebuf : CODEBUFFER = Codebuffer_impl.Codebuffer (Loongson)
and Loongson : IMPLEMENTER =
struct
	type word = int32
	let word_of_int = Int32.of_int
	let nativeint_of_word = Nativeint.of_int32

	(* Registers *)
	let register_sets = [|
		30 (* number of general registers *) ;
		32 (* number of FP/MMX registers *) |]
	
	let reg_of = function
		| Reg (0, r) ->
			if r = 0 then 30
			else if r = 29 then 31
			else r
		| Reg (_, c) -> c
		| _ -> failwith "Asking for the reg number of not a Reg"
	
	let const_of = function
		| Cst x -> x
		| _ -> failwith "Asking for the constant value of not a Cst"

	(* loop_descr is used to remember where a loop started and where it stops *) 
	type loop_descr_record = { start : int ; blez : int ; top : loop_descr }
	and loop_descr = loop_descr_record option

	type proc =
		{ buffer : Codebuf.t ;	(* program counter *)
		  mutable loops : loop_descr ;	(* used only when emitting code not when running *)
		  mutable params : word array ;	(* used by load_param operation *)
		  mutable clock_reg : int ;	(* the number of our perm register that store loop counter *)
		  mutable frame_size : int ; (* the size of our stack frame *)
		  mutable callee_saved : (int * int) list } (* the reg and location of saved caller regs *)

	type op_impl =
		{ scratch : int array ;
		  perm : int array ;
		  out_banks : bank_num array ;
		  preamble_emitter : proc -> spec_in array (* perm regs *) -> unit ;
		  emitter : proc ->
		  	spec_in array (* input regs *) ->
		  	spec_in array (* scratch regs *) ->
		  	spec_in array (* output regs *) ->
		  	unit }

	type impl_lookup = scale * spec_in array * spec_out array -> op_impl

	(* Misc *)
	let unopt = function
		| None -> failwith "Nothing where something was expected."
		| Some x -> x

	(* MIPS instruction encoding *)
	let append_hw buffer value =
		Codebuf.append buffer (value land 0xff) ;
		Codebuf.append buffer ((value lsr 8) land 0xff)
	
	let emit_I_type buffer op rs rt imm =
		assert (imm >= -32768 && imm < 65536) ;	(* we ignore here if imm is signed *)
		append_hw buffer imm ;
		append_hw buffer
			((op lsl 10) lor ((rs land 0b11111) lsl 5) lor (rt land 0b11111))

	let emit_J_type buffer op instr_idx =
		append_hw buffer instr_idx ;
		append_hw buffer
			((op lsl 10) lor ((instr_idx lsr 16) land 0b1111111111))

	let emit_R_type buffer op rs rt rd sa fn =
		assert (op <= 0b111111) ;
		assert (rs <= 0b11111) ;
		assert (rt <= 0b11111) ;
		assert (rd <= 0b11111) ;
		assert (sa <= 0b11111) ;
		assert (fn <= 0b111111) ;
		append_hw buffer
			((rd lsl 11) lor ((sa land 0b11111) lsl 6) lor (fn land 0b111111)) ;
		append_hw buffer
			((op lsl 10) lor ((rs land 0b11111) lsl 5) lor (rt land 0b11111))
			
	let emit_SB buffer reg base offset = emit_I_type buffer 0b101000 base reg offset
	let emit_SH buffer reg base offset = emit_I_type buffer 0b101001 base reg offset
	let emit_SW buffer reg base offset = emit_I_type buffer 0b101011 base reg offset
	let emit_SD buffer reg base offset = emit_I_type buffer 0b111111 base reg offset
	
	let emit_LB  buffer reg base offset = emit_I_type buffer 0b100000 base reg offset
	let emit_LBU buffer reg base offset = emit_I_type buffer 0b100100 base reg offset
	let emit_LH  buffer reg base offset = emit_I_type buffer 0b100001 base reg offset
	let emit_LHU buffer reg base offset = emit_I_type buffer 0b100101 base reg offset
	let emit_LW  buffer reg base offset = emit_I_type buffer 0b100011 base reg offset
	let emit_LWU buffer reg base offset = emit_I_type buffer 0b100111 base reg offset
	let emit_LD  buffer reg base offset = emit_I_type buffer 0b110111 base reg offset

	let emit_DADDIU buffer dest source imm = emit_I_type buffer 0b011001 source dest imm
	let emit_ADDIU  buffer dest source imm = emit_I_type buffer 0b001001 source dest imm
	let emit_ANDI   buffer dest source imm = emit_I_type buffer 0b001100 source dest imm
	let emit_ORI    buffer dest source imm = emit_I_type buffer 0b001101 source dest imm

	let emit_DADDU buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b101101
	let emit_ADDU  buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100001
	let emit_AND   buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100100
	let emit_OR    buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100101
	let emit_SUBU  buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100011

	let emit_JR buffer reg = emit_J_type buffer 0b000000 ((reg lsl 21) lor 0b1000)

	let emit_BLEZ buffer reg offset = emit_I_type buffer 0b000110 reg 0 offset
	let emit_BLTZ buffer reg offset = emit_I_type buffer 0b000001 reg 0 offset
	let emit_BEQ  buffer r1 r2 offset = emit_I_type buffer 0b000100 r1 r2 offset
	
	let emit_MULTUG  buffer dest a b = emit_R_type buffer 0b011100 a b dest 0b00000 0b010010
	let emit_DMULTUG buffer dest a b = emit_R_type buffer 0b011100 a b dest 0b00000 0b010011
	
	let emit_SLL  buffer dest reg shift = emit_R_type buffer 0b000000 0 reg dest shift 0b000000
	let emit_SRL  buffer dest reg shift = emit_R_type buffer 0b000000 0 reg dest shift 0b000000
	let emit_SRA  buffer dest reg shift = emit_R_type buffer 0b000000 0 reg dest shift 0b000011
	let emit_SRL  buffer dest reg shift = emit_R_type buffer 0b000000 0 reg dest shift 0b000010
	let emit_DSLL buffer dest reg shift =
		if shift < 32 then
			emit_R_type buffer 0b000000 0 reg dest shift 0b111000
		else
			emit_R_type buffer 0b000000 0 reg dest (shift-32) 0b111100
	let emit_NOP buffer = emit_OR buffer 1 1 0 (*emit_SLL buffer 0 0 0*)
	
	let patch_imm buffer addr imm =
		Codebuf.patch_byte buffer addr (imm land 0xff) 0xff ;
		Codebuf.patch_byte buffer (addr+1) (imm lsr 8) 0xff

	(* Implemented Operations. *)

	let add = function
		| 1, [| Reg (0, sz) ; Reg (0, sz') |], [| sz'' |]
			when sz = sz' && sz = sz'' && sz <= 64 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins _scratch outs ->
				(if sz <= 32 then emit_ADDU else emit_DADDU)
					proc.buffer (reg_of outs.(0)) (reg_of ins.(1)) (reg_of ins.(2))) }
		| _ -> raise Not_found

	let mul_rshift = function
		| 1, [| Reg (0, sz) ; Reg (0, sz') ; Cst shift |], [| sz'' |]
			when sz = sz' && sz <= 32 && sz'' <= 32 && shift <= 31 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins _scratch outs ->
				(if sz <= 32 && sz'' <= 32 then emit_MULTUG else emit_DMULTUG)
					proc.buffer (reg_of outs.(0)) (reg_of ins.(0)) (reg_of ins.(1)) ;
				let shift = const_of ins.(2) in
				if shift > 0 then emit_SRL proc.buffer (reg_of outs.(0)) (reg_of outs.(0)) shift) } 
		| _ -> raise Not_found

	let pack565 = function
		| 1, [| Reg (0, 8) ; Reg (0, 8) ; Reg (0, 8) |], [| 16 |] ->
			{ scratch = [| 1 |] ; perm = [||] ; out_banks = [| 0 |] ;
			  preamble_emitter = no_preamble ;
			  emitter = (fun proc ins scratch outs ->
			  	emit_ANDI proc.buffer (reg_of outs.(0)) (reg_of ins.(0)) 0xf8 ; (* R *)
				emit_SLL  proc.buffer (reg_of outs.(0)) (reg_of outs.(0)) 8 ;
				emit_ANDI proc.buffer (reg_of scratch.(0)) (reg_of ins.(1)) 0xfc ; (* G *)
				emit_SLL  proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) 3 ;
				emit_OR   proc.buffer (reg_of outs.(0)) (reg_of outs.(0)) (reg_of scratch.(0)) ;
				emit_SRL  proc.buffer (reg_of scratch.(0)) (reg_of ins.(2)) 3 ; (* B *)
				emit_OR   proc.buffer (reg_of outs.(0)) (reg_of outs.(0)) (reg_of scratch.(0))) }
		| _ -> raise Not_found

	let unpack565 = function
		| 1, [| Reg (0, 16) |], [| 8 ; 8 ; 8 |] ->
			{ scratch = [||] ; perm = [||] ; out_banks = [| 0 ; 0 ; 0 |] ;
			  preamble_emitter = no_preamble ;
			  emitter = (fun proc ins _scratch outs ->
			  emit_SRL  proc.buffer (reg_of outs.(0)) (reg_of ins.(0)) 8 ; (* R *)
			  emit_SRL  proc.buffer (reg_of outs.(1)) (reg_of ins.(0)) 3 ; (* G *)
			  emit_SLL  proc.buffer (reg_of outs.(2)) (reg_of ins.(0)) 3 ; (* B *)
			  emit_ANDI proc.buffer (reg_of outs.(0)) (reg_of outs.(0)) 0xf8 ;
			  emit_ANDI proc.buffer (reg_of outs.(1)) (reg_of outs.(1)) 0xfc ;
			  emit_ANDI proc.buffer (reg_of outs.(2)) (reg_of outs.(2)) 0xff) }
		| _ -> raise Not_found

	let var_read = function
		| 1, [| Reg (0, 32) |], [| sz |] when sz <= 64 -> {
			scratch = [||] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins _scratch outs ->
				(if sz <= 8 then emit_LBU
				 else if sz <= 16 then emit_LHU
				 else if sz <= 32 then emit_LWU
				 else emit_LD)
					proc.buffer (reg_of outs.(0)) (reg_of ins.(0)) 0) }
		| _ -> raise Not_found

	let var_write = function
		| 1, [| Reg (0, 32) ; Reg (0, sz) |], [||] when sz <= 64 -> {
			scratch = [||] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins _scratch _outs ->
				(if sz <= 8 then emit_SB
				 else if sz <= 16 then emit_SH
				 else if sz <= 32 then emit_SW
				 else emit_SD)
				 	proc.buffer (reg_of ins.(1)) (reg_of ins.(0)) 0) }
		| _ -> raise Not_found

	let stream_read = function
		| 1, [| Reg (0, 32) |], [| 64 |] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch outs ->
				emit_SLL proc.buffer (reg_of scratch.(0)) proc.clock_reg 3 ;
				emit_ADDU proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) (reg_of ins.(0)) ;
				emit_LD proc.buffer (reg_of outs.(0)) (reg_of scratch.(0)) 0) }
		| 1, [| Reg (0, 32) |], [| 32 |] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch outs ->
				emit_SLL proc.buffer (reg_of scratch.(0)) proc.clock_reg 2 ;
				emit_ADDU proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) (reg_of ins.(0)) ;
				emit_LWU proc.buffer (reg_of outs.(0)) (reg_of scratch.(0)) 0) }
		| 1, [| Reg (0, 32) |], [| 16 |] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch outs ->
				emit_ADDU proc.buffer (reg_of scratch.(0)) proc.clock_reg proc.clock_reg ;
				emit_ADDU proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) (reg_of ins.(0)) ;
				emit_LHU proc.buffer (reg_of outs.(0)) (reg_of scratch.(0)) 0) }
		| 1, [| Reg (0, 32) |], [| 8 |] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [| 0 |] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch outs ->
				emit_ADDU proc.buffer (reg_of scratch.(0)) proc.clock_reg (reg_of ins.(0)) ;
				emit_LBU proc.buffer (reg_of outs.(0)) (reg_of scratch.(0)) 0) }
		| _ -> raise Not_found

	let stream_write = function
		| 1, [| Reg (0, 32) ; Reg (0, 64) |], [||] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch _outs ->
				emit_SLL proc.buffer (reg_of scratch.(0)) proc.clock_reg 3 ;
				emit_ADDU proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) (reg_of ins.(0)) ;
				emit_SD proc.buffer (reg_of ins.(1)) (reg_of scratch.(0)) 0) }
		| 1, [| Reg (0, 32) ; Reg (0, 32) |], [||] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch _outs ->
				emit_SLL proc.buffer (reg_of scratch.(0)) proc.clock_reg 2 ;
				emit_ADDU proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) (reg_of ins.(0)) ;
				emit_SW proc.buffer (reg_of ins.(1)) (reg_of scratch.(0)) 0) }
		| 1, [| Reg (0, 32) ; Reg (0, 16) |], [||] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch _outs ->
				emit_ADDU proc.buffer (reg_of scratch.(0)) proc.clock_reg proc.clock_reg ;
				emit_ADDU proc.buffer (reg_of scratch.(0)) (reg_of scratch.(0)) (reg_of ins.(0)) ;
				emit_SH proc.buffer (reg_of ins.(1)) (reg_of scratch.(0)) 0) }
		| 1, [| Reg (0, 32) ; Reg (0, 8) |], [||] -> {
			scratch = [| 1 |] ; perm = [||] ; out_banks = [||] ;
			preamble_emitter = no_preamble ;
			emitter = (fun proc ins scratch _outs ->
				emit_ADDU proc.buffer (reg_of scratch.(0)) proc.clock_reg (reg_of ins.(0)) ;
				emit_SB proc.buffer (reg_of ins.(1)) (reg_of scratch.(0)) 0) }
		| _ -> raise Not_found

	let loop_head = function
		| scale, [| Reg (1, 32) |], [||] ->
			{ scratch = [| 1 |] ; perm = [||] ; out_banks = [||] ;
			  preamble_emitter = no_preamble ;
			  emitter = (fun proc ins scratch _outs ->
			  	let loop_start = Codebuf.offset proc.buffer in
			  	(* First test if clock + scale > width, and if so jump forward to quit label
			  	 * then save loop label *)
			  	emit_ADDIU proc.buffer (reg_of scratch.(0)) proc.clock_reg scale ;
			  	emit_SUBU  proc.buffer (reg_of scratch.(0)) (reg_of ins.(0)) (reg_of scratch.(0)) ;
			  	proc.loops <- Some
					{ start = loop_start ; blez = Codebuf.offset proc.buffer ; top = proc.loops } ;
				emit_BLTZ  proc.buffer (reg_of scratch.(0)) 0; (* actual offset will be patched later *)
				emit_NOP   proc.buffer) }
		| _ -> raise Not_found

	let loop_tail = function
		| scale, [||], [||] ->
			{ scratch = [||] ; perm = [||] ; out_banks = [||] ;
			  preamble_emitter = no_preamble ;
			  emitter = (fun proc _ins _scratch _outs ->
			  let loop = unopt proc.loops in
			  let offset = (loop.start - ((Codebuf.offset proc.buffer) + 4)) / 4 in
			  Printf.printf "BEQ to %d, from %d to %d\n" offset ((Codebuf.offset proc.buffer) + 8) loop.start ;
			  emit_BEQ   proc.buffer 0 0 offset ;
			  emit_ADDIU proc.buffer proc.clock_reg proc.clock_reg scale ;
			  patch_imm  proc.buffer loop.blez (((Codebuf.offset proc.buffer) - (loop.blez + 4)) / 4) ;
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
				proc.clock_reg <- (reg_of perms.(0)) ;
				Printf.printf "Using $%d as clock_reg\n" proc.clock_reg ;
				emit_DADDU proc.buffer proc.clock_reg 0 0) ;
			(* FIXME: if we have no constant but only registers, do this ;
			 * otherwise code must init regs with constant as well. *)
			emitter = (fun proc _ins _scratch outs ->
				(* Load parameters into allocated registers *)
				Array.iteri (fun i r -> emit_LD proc.buffer (reg_of r) 29 (8 * i)) outs) }

	(* Returns the context used by emitters. *)
	let make_proc _nb_sources =
		{ buffer = Codebuf.make 1024 "/tmp/test.code" ;
		  loops = None ;
		  clock_reg = 0 ;
		  params = [||] ;
		  frame_size = 0 ;
		  callee_saved = [] }

	type initer = Param of int | Const of word

	let emit_entry_point proc _inits used_regs =
		(* Callee-saved registers and return address, that we are going to save
		 * if we use them. *)
		let is_callee_saved = function
			| 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 28 | 30 -> true
			(* return address is not really a callee saved reg, but we will treat it the same *)
			| 31 -> true
			| _ -> false in
		(* These are used to store function arguments.
		 * We save them on the stack since we pretend these registers are available *)
		let arg_regs = [| 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 |] in
		(* We must compute the stack frame size. Reserve as much space for
		 * arguments registers (that we will save inconditionnaly) and for used
		 * callee-saved registers. *)
		proc.frame_size <- (Array.length arg_regs) * 8 ;
		proc.callee_saved <- [] ;
		Hashtbl.iter (fun r _ ->
			let r = reg_of r in
			if is_callee_saved r then (
				Printf.printf "We save register $%d at offset %d\n" r proc.frame_size ;
				(proc.callee_saved <- (proc.frame_size, r) :: proc.callee_saved ;
				proc.frame_size <- proc.frame_size + 8))) used_regs ;
		emit_ADDIU proc.buffer 29 29 (-proc.frame_size) ;
		(* Save all registers used to pass arguments on top of this frame,
		 * so that we can later easily retrieve them. *)
		let save_reg offset reg = emit_SD proc.buffer reg 29 offset in
		Array.iteri (fun i r -> save_reg (i*8) r) arg_regs ;
		(* Save the callee saved registers that we are going to use *)
		List.iter (fun (offset, r) -> save_reg offset r) proc.callee_saved

	let emit_exit proc =
		(* Restore r31 and other callee-saved registers that were saved on the stack. *)
		let restore_reg (offset, reg) = emit_LD proc.buffer reg 29 offset in
		List.iter restore_reg proc.callee_saved ;
(*		let arg_regs = [| 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 |] in
		Array.iteri (fun i r -> restore_reg (i*8, r)) arg_regs ; *)
		(* Restore stack pointer and return to caller *)
		emit_JR proc.buffer 31 ;
		emit_ADDIU proc.buffer 29 29 proc.frame_size

	(* Source values are given as integers. *)
	let exec proc params =
		let rec to_top_loop prev = function
			| None -> prev
			| Some loop -> to_top_loop (Some loop) loop.top in
		proc.loops <- to_top_loop None proc.loops ;
		Codebuf.exec proc.buffer 0 params
end