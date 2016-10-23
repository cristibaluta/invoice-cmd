(* open Yojson.Basic.Util *)
open Str
open Printf
open Arg

type company_details = 	
	| Name of string
    | Orc of string
    | Cui of string
	| Address of string
	| County of string
	| BankAccount of string
	| BankName of string
;;
type commands = 
	| Generate
	| List
	| Help
;;

let command = ref Help
let value = ref 0
let hours = ref 0
let series = ref ""
let nr = ref 0
let tva = ref 0
let rate = ref 0.0
let exchange_rate = ref 0.0
let command_from_string c = match c with
	| "generate" -> Generate
	| "list" -> List
	| "help" | _ -> Help
	
;;
let variables = ["::contractor_name::"]
let read_all_lines file_name =
  let in_channel = open_in file_name in
  let rec read_recursive lines =
    try
      Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines))
    with
      End_of_file ->
        lines in
  let lines = read_recursive [] in
  let _ = close_in_noerr in_channel in
  List.rev (lines)
;;

let rec print_numbers oc = function 
  | [] -> ()
  | e::tl -> Printf.fprintf oc "%d %d\n" (fst e) (snd e); print_numbers oc tl
;;
let rec print_lines file_o = function 
  | [] -> ()
  | e::tl -> 
	  Printf.fprintf file_o "%s\n" (Str.global_replace (Str.regexp "::title::") "title" e);
	  print_lines file_o tl
;;

let main =
begin
let speclist = [
	("-value", Arg.Set_int value, "Value of invoice");
	("-tva", Arg.Set_int tva, "TVA");
	("-hours", Arg.Set_int hours, "Hours");
	("-series", Arg.Set_string series, "Series of invoice");
	("-nr", Arg.Set_int nr, "Number of invoice");
	("-rate", Arg.Set_float rate, "Series of invoice");
	("-exchange-rate", Arg.Set_float exchange_rate, "Series of invoice");
	(* ("-tva", Arg.Tuple ([Arg.Set_int time_hours ; Arg.Set_int time_minutes]), "Sets creation hours and minutes listed files have to match"); *)
	(* ("-s", Arg.Symbol (["alpha"; "chrono"; "owner"], sort_files), " Allows to sort listed files alphabetically, chronologically, or by owner"); *)
	("--", Arg.Rest (fun arg -> print_endline ("The rest contains: " ^ arg)), "Stop interpreting keywords and prints the rest");
]
in let usage_msg = "Usage:"
in Arg.parse speclist (fun anon -> command := command_from_string anon) usage_msg;
match !command with
	| Generate ->
		print_endline ("Generate new invoice");
		(* print_endline ("Value: " ^ string_of_int !value);*)
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		let last_invoice_dir = Array.get children (Array.length children -1) in
		let template = read_all_lines (dir ^ "/0/template.html") in
		(* List.iter (printf "%s ") template; *)
	  	let file_o = open_out (last_invoice_dir ^ "/invoice.html") in
	  	print_lines file_o template;
	  	close_out file_o;
	| List -> 
		print_endline ("List existing invoices");
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		Array.iter print_endline children;
	| Help -> 
		print_endline ("Print help");
print_endline ("Thank you for using invoice cmd!");

end

let () = main
