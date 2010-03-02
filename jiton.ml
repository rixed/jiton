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
 * ALSO USED FOR PASSING ACTUAL VALUES TO CODE EMITTERS !
 * In this case, data_type is reg number, constant value or auto storage location !
 * Auto is for automatic storage (typically, stack) *)
type spec_in =
	| Reg of (bank_num * data_type)
	| Cst of data_type
	| Auto of data_type

(* Used to specify expected outputs *)
type spec_out = data_type

(* When the implementation have no preamble to emit *)
let no_preamble _ _ = ()

(*
 * An implementer provides code emitters for an architecture
 *)

module type IMPLEMENTER =
sig
	(* Abstract type of a procedure *)
	type proc

	(* Word is the size of the general registers (bank 0),
	 * auto storage and procedure parameters. This is not necessarily
	 * nativeint since one may want to cross-compile. *)
	type word
	val word_of_int : int -> word

	(* But then, if you plan to execute it we want to be able to convert
	 * it to nativeint. *)
	val nativeint_of_word : word -> nativeint
	
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
		  preamble_emitter : proc -> spec_in array (* perm regs *) -> unit ;
		  (* This code perform the operation in the main loop *)
		  emitter : proc ->
		  	spec_in array (* input regs *) ->
		  	spec_in array (* scratch regs *) ->
		  	spec_in array (* output regs *) ->
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
	val emit_entry_point : proc -> initer array -> (spec_in, bool) Hashtbl.t -> unit

	(* Emit code to exit the procedure (and clear auto storage) *)
	val emit_exit : proc -> unit

	(* Run the given procedure with these parameters.
	 * If your parameters are bigarray, use the convertion function address_of_bigarray below. *)
	val exec : proc -> nativeint array -> unit
end

(* This works on bigarrays as it would work on any custom tagged object which stores
 * a pointer to its data as the first member of its structure. *)
external address_of_bigarray : 'a -> nativeint = "%identity"

(*
 * A CODEBUFFER provides a way to poke instructions, execute them, save them on a file...
 *)

module type CODEBUFFER =
sig
	type t

	(* Low level interface to a mmaped executable file *)
	val make : int -> string -> t
	val exec : t -> int (* offset *) -> nativeint array -> unit

	(* Higher level functions *)
	val append : t -> int -> unit
	val align : t -> int -> unit
	val offset : t -> int
	val patch_byte : t -> int (* offset *) -> int (* value *) -> int (* mask *) -> unit
end

module type COMPILER =
sig
	module Impl : IMPLEMENTER
	
	type program_step = Impl.impl_lookup * string array * (string * data_type) array
	
	type program = program_step array
	
	type program_param = string * data_type

	val compile : program -> program_param array -> (nativeint array -> unit)

end
