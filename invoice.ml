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
let amount = ref 0
let hours = ref 0
let email = ref ""
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
let placeholders = [
	"::email::";
	"::contractor_name::";
	"::rate::";
	"::amount::";
	"::tva::";
	"::amount_total::"
]
let rec iterate_placeholders placeholders lineString = match placeholders with
	| [] -> print_endline "This is the end of the string list!"
	| placeholder::body -> 
		begin
			let new_lineString = Str.global_replace (Str.regexp placeholder) "5667,88" lineString in
			iterate_placeholders body new_lineString
		end
;;
(* let rec replace s = function
  | [] -> ""
  | e::tl -> Str.global_replace (Str.regexp "::amount::") "5667,88" e
;; *)
let make_replacements s = match s with
	| ss ->
		iterate_placeholders placeholders ss
		(* Str.global_replace (Str.regexp "::title::") "title" ss;
		Str.global_replace (Str.regexp "::amount::") "5667,88" ss *)
;;
let rec write_line file_o = function 
  | [] -> ()
  | e::tl -> 
	  Printf.fprintf file_o "%s\n" (make_replacements e);
	  write_line file_o tl
;;

let main =
begin
let speclist = [
	("-amount", Arg.Set_int amount, "Amount to be paid");
	("-tva", Arg.Set_int tva, "TVA");
	("-hours", Arg.Set_int hours, "Amount of worked hours");
	("-email", Arg.Set_string email, "Email to be replaced in ::email::");
	("-series", Arg.Set_string series, "Series of invoice");
	("-nr", Arg.Set_int nr, "Number of invoice");
	("-rate", Arg.Set_float rate, "Series of invoice");
	("-exchange-rate", Arg.Set_float exchange_rate, "Series of invoice");
]
in let usage_msg = "Usage:"
in Arg.parse speclist (fun anon -> command := command_from_string anon) usage_msg;
match !command with
	| Generate ->
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		let last_invoice_dir = Array.get children (Array.length children -1) in
		let template = read_all_lines (dir ^ "/0/template.html") in
	  	let file_o = open_out (last_invoice_dir ^ "/invoice.html") in
	  	write_line file_o template;
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
