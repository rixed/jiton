open Jiton

module Codebuffer (Impl : IMPLEMENTER)
	: CODEBUFFER =
struct
	type codebuf	(* Remains abstract, actually a C struct codebuf *)
	type t = { codebuf : codebuf ; mutable offset : int }

	external make_codebuf
		: int -> string -> codebuf = "wrap_make_codebuf"
	external poke_byte
		: codebuf -> int -> int -> unit = "wrap_codebuf_poke"
	external peek_byte
		: codebuf -> int -> int = "wrap_codebuf_peek"
	external codebuf_exec
		: codebuf -> int -> nativeint array -> unit = "wrap_codebuf_exec"
	external codebuf_addr
		: codebuf -> int -> int64 = "wrap_codebuf_addr"
	
	let make s f =
		{ codebuf = make_codebuf s f ; offset = 0 }
	
	let exec buf = codebuf_exec buf.codebuf 
	
	let append buf byte =
		poke_byte buf.codebuf buf.offset byte ;
		buf.offset <- succ buf.offset
	
	let align buf boundary =
		while (Int64.to_int (codebuf_addr buf.codebuf buf.offset)) mod boundary <> 0 do
			append buf 0
		done
	
	let offset buf = buf.offset

	let patch_byte buf addr value mask =
		let old_value = peek_byte buf.codebuf addr in
		poke_byte buf.codebuf addr ((old_value land (lnot mask)) lor (value land mask))
			
end

