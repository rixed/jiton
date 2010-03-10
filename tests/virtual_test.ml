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

open Bigarray
open Jiton

module Test (Impl : IMPLEMENTER) =
struct
	module Compiler = Compiler_impl.Compiler (Impl)

	let () =
		let program = [|
			Impl.stream_read, [| "pixels" |], [| "pixel", (16, Unsigned) |] ;
			Impl.unpack565, [| "pixel" |], [| "r", (8, Unsigned) ; "g", (8, Unsigned) ; "b", (8, Unsigned) |] ;
			Impl.stream_read, [| "alpha" |], [| "a", (8, Unsigned) |] ;
			Impl.mul_rshift, [| "r" ; "a" ; "8" |], [| "r'", (8, Unsigned) |] ;
			Impl.mul_rshift, [| "g" ; "a" ; "8" |], [| "g'", (8, Unsigned) |] ;
			Impl.mul_rshift, [| "b" ; "a" ; "8" |], [| "b'", (8, Unsigned) |] ;
			Impl.pack565, [| "r'" ; "g'" ; "b'" |], [| "pixel'", (16, Unsigned) |] ;
			Impl.stream_write, [| "image" ; "pixel'" |], [||] |] in
		let program_params = [| "width", (32, Unsigned) ; "pixels", (32, Unsigned) ; "alpha", (32, Unsigned) ; "image", (32, Unsigned) |] in
		let procedure = Compiler.compile program program_params in
		Printf.printf "%!" ; (* prepare for the coming segfault :-> *)
		(* Test it on some inputs *)
		let nb_pixels = 2 in
		let pixels = Array1.create int16_unsigned c_layout 2 in
		let alphas = Array1.create int8_unsigned  c_layout 2 in
		let image  = Array1.create int16_unsigned c_layout 2 in
		pixels.{0} <- 0x5678 ;
		pixels.{1} <- 0x1234 ;
		alphas.{0} <- 0x40 ;
		alphas.{1} <- 0x80 ;
		procedure
			[| Nativeint.of_int nb_pixels ;
			   address_of_bigarray pixels ;
			   address_of_bigarray alphas ;
			   address_of_bigarray image |] ;
		Printf.printf "Image = 0x%x 0x%x\n" image.{0} image.{1}

end

module Test1 = Test (Impl_virtual.Virtual)
module Test2 = Test (Impl_loongson.Loongson)

