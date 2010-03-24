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
	let int_of_word = Int32.to_int
	let nativeint_of_word = Nativeint.of_int32
	let word_of_string = Int32.of_string
	let string_of_word = Int32.to_string

	(* Registers *)
	let register_sets = [|
		30 (* number of general registers *) ;
		32 (* number of FP/MMX registers *) |]
	
	let reg_of = function
		| 0, r ->
			if r = 0 then 30
			else if r = 29 then 31
			else r
		| _, c -> c
	
	let string_of_reg_id (b, r) =
		if b = 0 then Printf.sprintf "$%d" (reg_of (0, r))
		else (
			assert (b = 1) ;
			Printf.sprintf "$f%d" (reg_of (1, r)))

	let alignment scale = scale

	(* loop_descr is used to remember where a loop started and where it stops *) 
	type loop_descr_record = { start : int ; blez : int ; top : loop_descr }
	and loop_descr = loop_descr_record option

	type proc =
		{ buffer : Codebuf.t ;	(* program counter *)
		  mutable loops : loop_descr ;	(* used only when emitting code not when running *)
		  mutable params : word array ;	(* used by load_param operation *)
		  mutable frame_size : int ; (* the size of our stack frame *)
		  mutable callee_saved : (int * reg_id) list } (* the reg and location of saved caller regs *)

	type emitter = proc -> (string -> reg_id) -> unit
	type op_impl =
		{ helpers : (helper_kind * string * op_impl) array ;
		  out_banks : bank_num array ;
		  emitter : emitter }

	type spec_in =
		| Reg of (bank_num * data_type)
		| Cst of word
	type impl_lookup = scale * spec_in array * spec_out array -> op_impl

	(* Misc *)
	let unopt = function
		| None -> failwith "Nothing where something was expected."
		| Some x -> x

	(*MIPS instruction encoding *)

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
	
	let emit_SDC copro buffer reg base offset = emit_I_type buffer (0b111100 lor copro) base reg offset
	let emit_LDC copro buffer reg base offset = emit_I_type buffer (0b110100 lor copro) base reg offset

	let emit_DADDIU buffer dest source imm = emit_I_type buffer 0b011001 source dest imm
	let emit_ADDIU  buffer dest source imm = emit_I_type buffer 0b001001 source dest imm
	let emit_ANDI   buffer dest source imm = emit_I_type buffer 0b001100 source dest imm
	let emit_ORI    buffer dest source imm = emit_I_type buffer 0b001101 source dest imm
	let emit_LUI    buffer dest        imm = emit_I_type buffer 0b001111 0      dest imm

	let emit_DADDU buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b101101
	let emit_ADDU  buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100001
	let emit_AND   buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100100
	let emit_OR    buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100101
	let emit_SUBU  buffer dest src1 src2 = emit_R_type buffer 0b000000 src1 src2 dest 0b00000 0b100011

	let emit_JR buffer reg = emit_J_type buffer 0b000000 ((reg lsl 21) lor 0b1000)

	let emit_BLEZ buffer reg offset = emit_I_type buffer 0b000110 reg 0 offset
	let emit_BLTZ buffer reg offset = emit_I_type buffer 0b000001 reg 0 offset
	let emit_BEQ  buffer r1 r2 offset = emit_I_type buffer 0b000100 r1 r2 offset
	
	let emit_MULTG   buffer dest a b = emit_R_type buffer 0b011100 a b dest 0b00000 0b010000
	let emit_MULTUG  buffer dest a b = emit_R_type buffer 0b011100 a b dest 0b00000 0b010010
	let emit_DMULTG  buffer dest a b = emit_R_type buffer 0b011100 a b dest 0b00000 0b010001
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
	let emit_DSRL buffer dest reg shift =
		if shift < 32 then
			emit_R_type buffer 0b000000 0 reg dest shift 0b111010
		else
			emit_R_type buffer 0b000000 0 reg dest (shift-32) 0b111110
	
	let emit_MTC1  buffer src dst = emit_R_type buffer 0b010001 0b00100 src dst 0 0
	let emit_DMTC1 buffer src dst = emit_R_type buffer 0b010001 0b00101 src dst 0 0

	let emit_LI buffer reg value =
		if value >= 0l && value < 65536l then
			emit_ORI buffer reg 0 (int_of_word value)
		else if value >= -32768l && value < 0l then
			emit_ADDIU buffer reg 0 (int_of_word value)
		else (
			let hi = Int32.shift_right_logical value 16
			and lo = Int32.logand value 0xffffl in
			emit_LUI buffer reg (int_of_word hi) ;
			if lo <> 0l then emit_ORI buffer reg reg (int_of_word lo))
	
	let emit_NOP buffer = emit_OR buffer 1 1 0 (*emit_SLL buffer 0 0 0*)

	let emit_PCMPGTB   buffer dst a b = emit_R_type buffer 0b010010 0b11101 b a dst 0b001001
	let emit_PCMPGTH   buffer dst a b = emit_R_type buffer 0b010010 0b11011 b a dst 0b001001
	let emit_PCMPGTW   buffer dst a b = emit_R_type buffer 0b010010 0b11001 b a dst 0b001001
	let emit_PMULHUH   buffer dst a b = emit_R_type buffer 0b010010 0b11101 b a dst 0b001010
	let emit_PMULHH    buffer dst a b = emit_R_type buffer 0b010010 0b11011 b a dst 0b001010
	let emit_PMULLH    buffer dst a b = emit_R_type buffer 0b010010 0b11010 b a dst 0b001010
	let emit_PMULUW    buffer dst a b = emit_R_type buffer 0b010010 0b11100 b a dst 0b001010
	let emit_PSRLH     buffer dst a b = emit_R_type buffer 0b010010 0b11001 b a dst 0b001011
	let emit_PSRLW     buffer dst a b = emit_R_type buffer 0b010010 0b11000 b a dst 0b001011
	let emit_PUNPCKLBH buffer dst a b = emit_R_type buffer 0b010010 0b11010 b a dst 0b000011
	let emit_PUNPCKLHW buffer dst a b = emit_R_type buffer 0b010010 0b11000 b a dst 0b000011
	let emit_PUNPCKLWD buffer dst a b = emit_R_type buffer 0b010010 0b11100 b a dst 0b001011
	let emit_POR       buffer dst a b = emit_R_type buffer 0b010010 0b11011 b a dst 0b001100
	let emit_PSLL      buffer dst a b = emit_R_type buffer 0b010010 0b11000 b a dst 0b001110
	let emit_PAND      buffer dst a b = emit_R_type buffer 0b010010 0b11110 b a dst 0b000010
	let emit_PDSLL     buffer dst a b = emit_R_type buffer 0b010010 0b11001 b a dst 0b001110
	let emit_PSRL      buffer dst a b = emit_R_type buffer 0b010010 0b11000 b a dst 0b001111
	let emit_PDSRL     buffer dst a b = emit_R_type buffer 0b010010 0b11001 b a dst 0b001111
	let emit_PSRA      buffer dst a b = emit_R_type buffer 0b010010 0b11001 b a dst 0b001111
	let emit_PDSRA     buffer dst a b = emit_R_type buffer 0b010010 0b11011 b a dst 0b001111
	
	let patch_imm buffer addr imm =
		Codebuf.patch_byte buffer addr (imm land 0xff) 0xff ;
		Codebuf.patch_byte buffer (addr+1) (imm lsr 8) 0xff
	
	(* Helper vars *)

	let clock_var = Invariant, "clock",
		{ out_banks = [| 0 |] ; helpers = [||] ;
		  emitter = (fun proc g -> emit_DADDU proc.buffer (reg_of (g "clock")) 0 0) }

	let make_scratch ?(kind=Inline) bank name = kind, name,
		{ out_banks = [| bank |] ; helpers = [||] ;
		  emitter = (fun _proc _g -> ()) }

	let const_name bank c = "const_"^(string_of_int bank)^"_"^(string_of_word c)
	let make_const bank c =
		let name = const_name bank c in
		let scratch = make_unique "scratch_for_const" in
		Invariant, name, { out_banks = [| bank |] ; helpers = [| make_scratch ~kind:Invariant 0 scratch |] ;
			emitter = (fun proc g ->
				if bank = 0 then
					emit_LI proc.buffer (reg_of (g name)) c
				else (
					assert (bank = 1) ;
					emit_LI proc.buffer (reg_of (g scratch)) c ;
					emit_DMTC1 proc.buffer (reg_of (g scratch)) (reg_of (g name)))) }

	(* Implemented Operations. *)

	let add = function
		| 1, [| Reg (0, (sz, _)) ; Reg (0, (sz', _)) |], [| sz'', _ |]
			when sz = sz' && sz = sz'' && sz <= 64 ->
			{ out_banks = [| 0 |] ;
			  helpers = [||] ;
			  emitter = (fun proc g ->
			  	(if sz <= 32 then emit_ADDU else emit_DADDU)
			  		proc.buffer (reg_of (g ">0")) (reg_of (g "<1")) (reg_of (g "<2"))) }
		| _ -> raise Not_found

	let mul_rshift = function
		| 1, [| Reg (0, (sz, sign)) ; Reg (0, (sz', sign')) ; Cst shift |], [| (sz'', sign'') |]
			when sz = sz' && sz <= 64 && sz'' <= 64
				&& sign = sign' && sign = sign''
				&& shift < 64l -> {
			out_banks = [| 0 |] ;
			helpers = [||] ;
			emitter = (fun proc g ->
				(if sz <= 32 && sz'' <= 32 then (
					if sign = Signed then emit_MULTG else emit_MULTUG
				) else (
					if sign = Signed then emit_DMULTG else emit_DMULTUG
				)) proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) (reg_of (g "<1")) ;
				let shift = int_of_word shift in
				if shift > 0 then (
					(if sz > 32 then emit_DSRL else emit_SRL)
						proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) shift)) } 
		(* SIMD *)
		| 4, [| Reg (1, (sz, sign)) ; Reg (1, (sz', sign')) ; Cst 16l |], [| sz'', sign'' |]
			when sz <= 16 && sz' <= 16 && sz'' <= 16 &&
				sign = sign' && sign = sign'' ->
			{ out_banks = [| 1 |] ;
			  helpers = [||] ;
			  emitter = (fun proc g ->
			  	(if sign = Signed then emit_PMULHH else emit_PMULHUH)
					proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) (reg_of (g "<1"))) }
		| 4, [| Reg (1, (sz, _)) ; Reg (1, (sz', _)) ; Cst shift |], [| sz'', _ |]
			when sz + sz' <= 16 && sz'' <= 16 && shift <= 16l ->
			{ out_banks = [| 1 |] ;
			  helpers = [| make_const 1 shift |] ;
			  emitter = (fun proc g ->
			  	emit_PMULLH proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) (reg_of (g "<1")) ;
				if shift > 0l then
					emit_PSRLH proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) (reg_of (g (const_name 1 shift)))) }
		| _ -> raise Not_found

	let pack565 = function
		| 1, [| Reg (0, (8, _)) ; Reg (0, (8, _)) ; Reg (0, (8, _)) |], [| 16, _ |] ->
			let scratch = make_unique "scratch_565" in
			{ out_banks = [| 0 |] ;
			  helpers = [| make_scratch 0 scratch |] ;
			  emitter = (fun proc g ->
			  	emit_ANDI proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) 0xf8 ; (* R *)
				emit_SLL  proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) 8 ;
				emit_ANDI proc.buffer (reg_of (g scratch)) (reg_of (g "<1")) 0xfc ; (* G *)
				emit_SLL  proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) 3 ;
				emit_OR   proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g scratch)) ;
				emit_SRL  proc.buffer (reg_of (g scratch)) (reg_of (g "<2")) 3 ; (* B *)
				emit_OR   proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g scratch))) }
		(* SIMD *)
		| 4, [| Reg (1, (8, _)) ; Reg (1, (8, _)) ; Reg (1, (8, _)) |], [| 16, _ |] ->
			let scratch = make_unique "scratch_565" in
			{ out_banks = [| 1 |] ;
			  helpers =
			  	[| make_scratch 0 scratch ;
				   make_const 1 3l ; make_const 1 8l ;
				   make_const 1 0b11111000l ; make_const 1 0b11111100l |] ;
			  emitter = (fun proc g ->
				let const_3 = const_name 1 3l
				and const_8 = const_name 1 8l
				and mask_RB = const_name 1 0b11111000l
				and mask_G  = const_name 1 0b11111100l in
				emit_PAND  proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) (reg_of (g mask_RB)) ;
				emit_PDSLL proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g const_8)) ;
				emit_PAND  proc.buffer (reg_of (g scratch)) (reg_of (g "<1")) (reg_of (g mask_G)) ;
				emit_PDSLL proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g const_3)) ;
				emit_POR   proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g scratch)) ;
				emit_PAND  proc.buffer (reg_of (g scratch)) (reg_of (g "<2")) (reg_of (g mask_RB)) ;
				emit_PDSRL proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g const_3)) ;
				emit_POR   proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g scratch))) }
		| _ -> raise Not_found

	let unpack565 = function
		| 1, [| Reg (0, (16, _)) |], [| 8, Unsigned ; 8, Unsigned ; 8, Unsigned |] ->
			{ out_banks = [| 0 ; 0 ; 0 |] ;
			  helpers = [||] ;
			  emitter = (fun proc g ->
			  	emit_SRL  proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) 8 ; (* R *)
			  	emit_SRL  proc.buffer (reg_of (g ">1")) (reg_of (g "<0")) 3 ; (* G *)
			  	emit_SLL  proc.buffer (reg_of (g ">2")) (reg_of (g "<0")) 3 ; (* B *)
			  	emit_ANDI proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) 0xf8 ;
			  	emit_ANDI proc.buffer (reg_of (g ">1")) (reg_of (g ">1")) 0xfc ;
			  	emit_ANDI proc.buffer (reg_of (g ">2")) (reg_of (g ">2")) 0xff) }
		(* SIMD *)
		| 4, [| Reg (1, (16, _)) |], [| 8, Unsigned ; 8, Unsigned ; 8, Unsigned |] ->
			{ out_banks = [| 1 ; 1 ; 1 |] ;
			  (* FIXME: the masks should be repeated scale time, see commented code below *)
			  helpers = [| make_const 1 3l ; make_const 1 8l ; make_const 1 0b11111000l ; make_const 1 0b11111100l |] ;
				(*if proc.mask_RB = None then (
					proc.mask_RB <- Some (reg_of perms.(2)) ;
					emit_ORI   proc.buffer (reg_of (g scratch)) 0 0b11111000 ;
					emit_DMTC1 proc.buffer (reg_of (g scratch)) proc.mask_RB ;
					for i = 1 to 3 do
						emit_PINSRH i proc.buffer (unopt proc.mask_RB) (unopt proc.mask_RB) (unopt proc.mask_RB)
					done) ;
				if proc.mask_G = None then (
					proc.mask_G = Some (reg_of perms.(3)) ;
					emit_ORI   proc.buffer (reg_of (g scratch)) 0 0b11111100 ;
					emit_DMTC1 proc.buffer (reg_of (g scratch)) (unopt proc.mask_G) ;
					for i = 1 to 3 do
						emit_PINSRH i proc.buffer (unopt proc.mask_G) (unopt proc.mask_G) (unopt proc.mask_G)
					done)) ;*)
			emitter = (fun proc g ->
				let const_3 = const_name 1 3l
				and const_8 = const_name 1 8l
				and mask_RB = const_name 1 0b11111000l
				and mask_G  = const_name 1 0b11111100l in
				emit_PDSRL proc.buffer (reg_of (g ">0")) (reg_of (g "<0")) (reg_of (g const_8)) ; 
				emit_PAND  proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g mask_RB)) ;
				emit_PDSRL proc.buffer (reg_of (g ">1")) (reg_of (g "<0")) (reg_of (g const_3)) ;
				emit_PAND  proc.buffer (reg_of (g ">1")) (reg_of (g ">1")) (reg_of (g mask_G)) ;
				emit_PDSLL proc.buffer (reg_of (g ">2")) (reg_of (g "<0")) (reg_of (g const_3)) ;
				emit_PAND  proc.buffer (reg_of (g ">2")) (reg_of (g ">2")) (reg_of (g mask_RB))) }
		| _ -> raise Not_found

	let stream_read = function
		| 1, [| Reg (0, (32, Unsigned)) |], [| 64, _ |] ->
			let scratch = make_unique "scratch_read" in
			{ out_banks = [| 0 |] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_SLL  proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) 3 ;
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g "<0")) ;
			  	emit_LD   proc.buffer (reg_of (g ">0")) (reg_of (g scratch)) 0) }
		| 1, [| Reg (0, (32, Unsigned)) |], [| 32, sign |] ->
			let scratch = make_unique "scratch_read" in
			{ out_banks = [| 0 |] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_SLL  proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) 2 ;
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g "<0")) ;
			  	(if sign = Signed then emit_LW else emit_LWU)
					proc.buffer (reg_of (g ">0")) (reg_of (g scratch)) 0) }
		| 1, [| Reg (0, (32, Unsigned)) |], [| 16, sign |] ->
			let scratch = make_unique "scratch_read" in
			{ out_banks = [| 0 |] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) (reg_of (g "clock")) ;
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g "<0")) ;
			  	(if sign = Signed then emit_LH else emit_LHU)
					proc.buffer (reg_of (g ">0")) (reg_of (g scratch)) 0) }
		| 1, [| Reg (0, (32, Unsigned)) |], [| 8, sign |] ->
			let scratch = make_unique "scratch_read" in
			{ out_banks = [| 0 |] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) (reg_of (g "<0")) ;
			  	(if sign = Signed then emit_LB else emit_LBU)
					proc.buffer (reg_of (g ">0")) (reg_of (g scratch)) 0) }
		(* SIMD *)
		| scale, [| Reg (0, (32, Unsigned)) |], [| sz, _ |] when scale * sz = 64 ->
			let scratch = make_unique "scratch_read" in
			{ out_banks = [| 1 |] ;
			  helpers = [| make_scratch 1 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU  proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) (reg_of (g "<0")) ;
			  	emit_LDC 1 proc.buffer (reg_of (g ">0")) (reg_of (g scratch)) 0) }
		(* In this one we want to store 4 bytes into our SIMD 64 bits register, so we will need to
		 * expand the values to 16 bits (while still pretending they are 8 bits) *)
		| 4, [| Reg (0, (32, Unsigned)) |], [| 8, sign |] ->
			let scratch0 = make_unique "scratch_read"
			and scratch1 = make_unique "scratch_sign" in
			{ out_banks = [| 1 |] ;
			  helpers =
			  	[| make_scratch 0 scratch0 ;
				   make_scratch 1 scratch1 ;
			  	   clock_var ;
				   make_const 1 0l |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU  proc.buffer (reg_of (g scratch0)) (reg_of (g "clock")) (reg_of (g "<0")) ;
				emit_LW    proc.buffer (reg_of (g scratch0)) (reg_of (g scratch0)) 0 ;
				emit_MTC1  proc.buffer (reg_of (g scratch0)) (reg_of (g ">0")) ;
				(* Expand the 4 low bytes into halfwords (interleaving zeros) *)
				emit_PUNPCKLBH proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g (const_name 1 0l))) ;
				if sign = Signed then (	(* Now sign-extend the values *)
					emit_PCMPGTB   proc.buffer (reg_of (g scratch1)) (reg_of (g (const_name 1 0l))) (reg_of (g ">0")) ;
					emit_PSLL      proc.buffer (reg_of (g scratch1)) (reg_of (g scratch1)) 8 ;
					emit_POR       proc.buffer (reg_of (g ">0")) (reg_of (g ">0")) (reg_of (g scratch1)))) }
		| _ -> raise Not_found

	let stream_write = function
		| 1, [| Reg (0, (32, Unsigned)) ; Reg (0, (64, _)) |], [||] ->
			let scratch = make_unique "scratch_write" in
			{ out_banks = [||] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_SLL  proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) 3 ;
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g "<0")) ;
			  	emit_SD   proc.buffer (reg_of (g "<1")) (reg_of (g scratch)) 0) }
		| 1, [| Reg (0, (32, Unsigned)) ; Reg (0, (32, _)) |], [||] ->
			let scratch = make_unique "scratch_write" in
			{ out_banks = [||] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_SLL  proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) 2 ;
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g "<0")) ;
			  	emit_SW   proc.buffer (reg_of (g "<1")) (reg_of (g scratch)) 0) }
		| 1, [| Reg (0, (32, Unsigned)) ; Reg (0, (16, _)) |], [||] ->
			let scratch = make_unique "scratch_write" in
			{ out_banks = [||] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) (reg_of (g "clock")) ;
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g scratch)) (reg_of (g "<0")) ;
			  	emit_SH   proc.buffer (reg_of (g "<1")) (reg_of (g scratch)) 0) }
		| 1, [| Reg (0, (32, Unsigned)) ; Reg (0, (8, _)) |], [||] ->
			let scratch = make_unique "scratch_write" in
			{ out_banks = [||] ;
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) (reg_of (g "<0")) ;
			  	emit_SB   proc.buffer (reg_of (g "<1")) (reg_of (g scratch)) 0) }
		(* SIMD *)
		| scale, [| Reg (0, (32, Unsigned)) ; Reg (1, (sz, _)) |], [||] when scale * sz = 64 ->
			let scratch = make_unique "scratch_write" in
			{ out_banks = [||] ;
			  helpers = [| make_scratch 1 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	emit_ADDU  proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) (reg_of (g "<0")) ;
			  	emit_SDC 1 proc.buffer (reg_of (g "<1")) (reg_of (g scratch)) 0) }
		| _ -> raise Not_found

	let loop_head = function
		| scale, [| Reg (0, (32, Unsigned)) |], [||] ->
			let scratch = make_unique "scratch_head" in
			{ out_banks = [||] ;
			  (* FIXME: the compiler don't know that we don't need the value of this scratch
			   * register from one run to another, and as a result will keep the register for
			   * the whole loop. *)
			  helpers = [| make_scratch 0 scratch ; clock_var |] ;
			  emitter = (fun proc g ->
			  	let loop_start = Codebuf.offset proc.buffer in
			  	(* First test if clock + scale > width, and if so jump forward to quit label
			  	 * then save loop label *)
			  	emit_ADDIU proc.buffer (reg_of (g scratch)) (reg_of (g "clock")) scale ;
			  	emit_SUBU  proc.buffer (reg_of (g scratch)) (reg_of (g "<0")) (reg_of (g scratch)) ;
			  	proc.loops <- Some
					{ start = loop_start ; blez = Codebuf.offset proc.buffer ; top = proc.loops } ;
				emit_BLTZ  proc.buffer (reg_of (g scratch)) 0; (* actual offset will be patched later *)
				emit_NOP   proc.buffer) }
		| _ -> raise Not_found

	let loop_tail = function
		| scale, [||], [||] ->
			{ out_banks = [||] ;
			  helpers = [| clock_var |] ;
			  emitter = (fun proc g ->
			  	let loop = unopt proc.loops in
			  	let offset = (loop.start - ((Codebuf.offset proc.buffer) + 4)) / 4 in
			  	Printf.printf "BEQ to %d, from %d to %d\n" offset ((Codebuf.offset proc.buffer) + 8) loop.start ;
			  	emit_BEQ   proc.buffer 0 0 offset ;
			  	emit_ADDIU proc.buffer (reg_of (g "clock")) (reg_of (g "clock")) scale ;
			  	patch_imm  proc.buffer loop.blez (((Codebuf.offset proc.buffer) - (loop.blez + 4)) / 4) ;
			  	proc.loops <- loop.top) }
		| _ -> raise Not_found

	let load_param = function
		| _, [| Cst p |], [| sz, _ |] when sz <= 64 ->
			{ out_banks = [| 0 |] ;
			  helpers = [||] ;
			  emitter = (fun proc g ->
			  	emit_LD proc.buffer (reg_of (g ">0")) 29 (8 * (int_of_word p))) }
		| _ -> raise Not_found

	(* Returns the context used by emitters. *)
	let next_seq =
		let seqnum = ref 0 in
		(fun () -> incr seqnum ; !seqnum)

	let make_proc _nb_sources =
		let seq = next_seq () in
		{ buffer = Codebuf.make 1024 ("/tmp/test.code"^(string_of_int seq)) ;
		  loops = None ;
		  params = [||] ;
		  frame_size = 0 ;
		  callee_saved = [] }

	type initer = Param of int | Const of word

	let emit_entry_point proc _inits used_regs =
		(* Callee-saved registers and return address, that we are going to save
		 * if we use them. *)
		let is_callee_saved = function
			| 0, r -> (match reg_of (0, r) with
				| 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 28 | 30 -> true
				(* return address is not really a callee saved reg, but we will treat it the same *)
				| 31 -> true
				| _ -> false)
			| 1, r -> let r = reg_of (1, r) in r >= 20 && r <= 31
			| _ -> failwith "Register is not a Reg" in
		(* These are used to store function arguments.
		 * We save them on the stack since we pretend these registers are available *)
		let arg_regs = [| 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 |] in
		(* We must compute the stack frame size. Reserve as much space for
		 * arguments registers (that we will save inconditionnaly) and for used
		 * callee-saved registers. *)
		proc.frame_size <- (Array.length arg_regs) * 8 ;
		proc.callee_saved <- [] ;
		List.iter (fun r ->
			if is_callee_saved r then (
				(proc.callee_saved <- (proc.frame_size, r) :: proc.callee_saved ;
				proc.frame_size <- proc.frame_size + 8))) used_regs ;
		emit_ADDIU proc.buffer 29 29 (-proc.frame_size) ;
		(* Save all registers used to pass arguments on top of this frame,
		 * so that we can later easily retrieve them. *)
		let save_reg offset r =
			Printf.printf "Saving register %s at offset %d\n" (string_of_reg_id r) proc.frame_size ;
			match r with
				| 0, _ -> emit_SD proc.buffer (reg_of r) 29 offset
				| 1, _ -> emit_SDC 1 proc.buffer (reg_of r) 29 offset
				| _ -> failwith "Saved reg is not a Reg" in
		Array.iteri (fun i r -> save_reg (i*8) (0, r)) arg_regs ;
		(* Save the callee saved registers that we are going to use *)
		List.iter (fun (offset, r) -> save_reg offset r) proc.callee_saved

	let emit_exit proc =
		(* Restore r31 and other callee-saved registers that were saved on the stack. *)
		let restore_reg offset r = match r with
			| 0, _ -> emit_LD proc.buffer (reg_of r) 29 offset
			| 1, _ -> emit_LDC 1 proc.buffer (reg_of r) 29 offset
			| _ -> failwith "Restored reg is not a Reg" in
		List.iter (fun (offset, r) -> restore_reg offset r) proc.callee_saved ;
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
