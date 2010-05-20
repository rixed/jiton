(*
	Attempt at a simple rgb lighten function :
	- read pixel from pixels
	- unpack565 into r, g, b
	- read alpha into a
	- mul r, a into r'
	- mul g, a into g'
	- mul b, a into b'
	- pack565 r', g', b' into pixel'
	- write pixel' into image
*)

open Bigarray
open Jiton

module Test (Impl : IMPLEMENTER) =
struct
	module Compiler = Compiler_impl.Compiler (Impl)

	let () =
		let program = [|
			Impl.stream_read_aligned, [| "pixels" |], [| "pixel", (16, Unsigned) |] ;
			Impl.unpack565, [| "pixel" |], [| "r", (8, Unsigned) ; "g", (8, Unsigned) ; "b", (8, Unsigned) |] ;
			Impl.stream_read_aligned, [| "alpha" |], [| "a", (8, Unsigned) |] ;
			Impl.mul_rshift, [| "r" ; "a" ; "8" |], [| "r'", (8, Unsigned) |] ;
			Impl.mul_rshift, [| "g" ; "a" ; "8" |], [| "g'", (8, Unsigned) |] ;
			Impl.mul_rshift, [| "b" ; "a" ; "8" |], [| "b'", (8, Unsigned) |] ;
			Impl.pack565, [| "r'" ; "g'" ; "b'" |], [| "pixel'", (16, Unsigned) |] ;
			Impl.stream_write_aligned, [| "image" ; "pixel'" |], [||] |] in
		let program_params = [| "width", (32, Unsigned) ; "pixels", (32, Unsigned) ; "alpha", (32, Unsigned) ; "image", (32, Unsigned) |] in
		let procedure = Compiler.compile program program_params in
		Printf.printf "%!" ; (* prepare for the coming segfault :-> *)
		(* Test it on some inputs *)
		let nb_pixels = 200 in
		let pixels = Array1.create int16_unsigned c_layout nb_pixels in
		let alphas = Array1.create int8_unsigned  c_layout nb_pixels in
		let image  = Array1.create int16_unsigned c_layout nb_pixels in
		for p = 0 to nb_pixels - 1 do
			pixels.{p} <- 0xffff ;
			alphas.{p} <- if p land 1 <> 0 then 0x80 else 0x0
		done ;
		procedure
			[| Nativeint.of_int nb_pixels ;
			   address_of_bigarray pixels ;
			   address_of_bigarray alphas ;
			   address_of_bigarray image |] ;
		for p = 0 to nb_pixels - 1 do
			Printf.printf "%04x " image.{p} ;
			if (p + 1) mod 8 = 0 then Printf.printf "\n"
		done ;
		Printf.printf "\n"

end

module Test1 = Test (Impl_virtual.Virtual)
module Test2 = Test (Impl_loongson.Loongson)

