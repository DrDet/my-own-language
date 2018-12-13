open Tree;;
open Printf;;
open Buffer;;
open Translation;;


let string_to_tree s =
	let (>>) x f = f x in
	s >> Lexing.from_string >> Parser.main Lexer.main
;;

let rec read_in_string ic buf =
	try
		let s = input_line ic in
		add_string buf s;
		add_string buf " ";
		read_in_string ic buf
	with eof ->
		contents buf
;;

let (ic, oc) = (open_in "input.q", open_out "output.c");;

let s = read_in_string ic (Buffer.create 2000000) in
let t = string_to_tree s in
let c_code = tree_to_c t in
fprintf stdout "%s" c_code;;
(* TODO: Change Eps ctor name in tree *)
