(*
 * JITon defines a code generator for SIMD processing of data streams.
 * Given an implementation and a loosely specified user program it outputs
 * a machine code procedure ready to be executed that performs the user program
 * on a chunk of data of length "width".
 * 
 * Whenever allowed by the implementation, the generated code will make use of
 * the available SIMD instructions.
 * 
 * This is typically useful when you have to perform some repetitive and simple
 * operations over buffers depending on parameters that are unknown at compile
 * time. For instance for an opengl software rasterizer, an image composition
 * library, image conversion from and to any format, etc...
 *)

(*
 * First we give meaningful names to various basic types we use often :
 *)

(* By "type" what we actually mean is "bit size" *)
type data_type = int

(* We may have distinct "bank" of registers
 * (typically, general purpose registers and (larger) SIMD registers.
 * Function parameters comes in registers of bank 0 *)
type bank_num = int	

(* Registers are identified by number *)
type reg_num = int

(* How many actions are performed by an operation implementation *)
type scale = int

(* Used to specify inputs and outputs of the operations.
 * Auto is for automatic storage (typically, stack) *)
type spec_in =
	| Reg of (bank_num * data_type)
	| Cst of data_type
	| Auto of data_type

(* Used to specify expected outputs *)
type spec_out = data_type

(*
 * An implementer provides code emitters for an architecture
 *)

module type IMPLEMENTER =
sig
	(* Abstract type of a procedure *)
	type proc

	(* Word is the size of the general registers (bank 0),
	 * auto storage and procedure parameters *)
	type word
	val word_of_int : int -> word
	
	(* Size of the various register banks *)
	val register_sets : int array

	(* Returns an empty proc *)
	val make_proc : int (* number of parameters *) -> proc

	(* An operation implementation *)
	type op_impl =
		{ scratch : int array ;	(* number of scratch registers needed per bank *)
		  perm : int array ;	(* number of permanently assigned registers needed *)
		  out_banks : bank_num array ;	(* in which banks the output are assigned *)
		  (* This code will be placed before the main loop and can be used to init perm regs *)
		  preamble_emitter : proc -> int array (* perm regs *) -> unit ;
		  (* This code perform the operation in the main loop *)
		  emitter : proc ->
		  	int array (* input regs *) ->
		  	int array (* scratch regs *) ->
		  	int array (* output regs *) ->
		  	unit }
	
	(* Look for an implementation satisfying given specifications. *)
	type impl_lookup = scale * spec_in array * spec_out array -> op_impl

	(* Operations supported. *)
	val var_read : impl_lookup
	val var_write : impl_lookup
	val stream_read : impl_lookup
	val stream_write : impl_lookup
	val loop_head : impl_lookup
	val loop_tail : impl_lookup
	val load_params : impl_lookup
	val add : impl_lookup
	val mul_rshift : impl_lookup
	val pack565 : impl_lookup
	val unpack565 : impl_lookup

	(* What to initialize auto storage with *)
	type initer = Param of int | Const of word

	(* Emit code to reserve and initialize some storage for extra values
	 * Note that for auto storage of param value, no extra storage may be
	 * needed (if procedure parameters are already on the stack) *)
	val emit_entry_point : proc -> initer array -> unit

	(* Emit code to exit the procedure (and clear auto storage) *)
	val emit_exit : proc -> unit

	(* Run the given procedure with these parameters *)
	val exec : proc -> word array -> unit
end

(*
 * A CODEBUFFER provides a way to poke instructions, execute them, save them on a file...
 *)

module type CODEBUFFER =
sig
	(* You write code into segments. *)
	type segment
	type func_param

	(* Open an existing segment or create a new one *)
	val open_segment : ?size:int -> ?filename:string -> segment
	val poke_byte : segment -> offset:int -> int -> unit
	val close_segment : segment -> unit
	val exec_segment : offset:int -> func_param array -> unit

end


