(* default values *)
(* let somestr = ref ""
let someint = ref 0
let usage = "usage: [-hours int] [-value int] [-tva int] [-rate int] [-exchange-rate int] [-series string] [-nr int]"

(* from http://rosettacode.org/wiki/Command-line_arguments#OCaml *)
let speclist = [
    ("-series", Arg.String (fun s -> somestr := s), ": follows -series sets some string");
    ("-hours", Arg.Int    (fun d -> someint := d), ": some int parameter");
  ]

let parseinput userinp =
  (* Read the arguments *)
  Printf.printf "String:%s\n" (Array.get userinp 2);
  Arg.parse_argv ?current:(Some (ref 0)) userinp
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  Printf.printf "Set stuff to:   %d '%s'\n%!"  !someint !somestr

let parseit line =
  Printf.printf "processing %s%!\n" line
  (* TODO rewrite without Str*)
  (* let listl = (Str.split (Str.regexp " ") line) in *)
  (* parseinput (Array.of_list listl) *)

let _ =
try
  while true do
    let line = input_line stdin in
    parseit line
  done;
  None
with
  End_of_file -> None *)