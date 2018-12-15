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

let (ic, oc) = (open_in "sample.q", open_out "output.c");;

let s = read_in_string ic (Buffer.create 2000000) in
let t = string_to_tree s in
let (decls, defs, funs, fun_calls) = collect_globals t ([], [], [], []) in
fprintf oc "#include <stdio.h>\n#include <stdlib.h>\n\n";
List.iter (fun s -> fprintf oc "%s\n" s) decls;
List.iter (fun s -> let end_idx = Str.search_forward (Str.regexp_string " {") s 0 in
					let s' = Str.string_before s end_idx in
					fprintf oc "%s;\n" s') funs;
List.iter (fun s -> fprintf oc "%s\n" s) defs;
List.iter (fun s -> fprintf oc "%s\n" s) funs;
fprintf oc "\nint main() {\n";
List.iter (fun s -> fprintf oc "\t%s\n" s) fun_calls;
fprintf oc "\treturn 0;\n}\n";;
