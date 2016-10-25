open Yojson.Basic
open Yojson.Basic.Util
open Str
open Printf
open Arg
open Unix

type commands = 
	| Generate
	| List
	| Help
;;
type company_details = 	
	| Name of string
    | Orc of string
    | Cui of string
	| Address of string
	| County of string
	| BankAccount of string
	| BankName of string
;;
(* type input_data =
	| InvoiceSeries
	| InvoiceNr
	| InvoiceDate
	| Name
	| Email
	| Web
	| Hours
	| HourlyRate
	| ExchangeRate
	| Amount
	| Tva
	| AmountTotal
	| Client of company_details
	| Contractor of company_details
;; *)

(* Json values. If they are specified in the cmd args, those ones have priority *)
let email = ref ""
let invoice_date = ref ""
let invoice_series = ref ""
let invoice_nr = ref 0

let hourly_rate = ref 0.0
let hours = ref 0.0
let exchange_rate = ref 0.0
let amount = ref 0

let tva = ref 0
let amount_total = ref 0

let command = ref Help
let command_from_string c = match c with
	| "generate" -> Generate
	| "list" -> List
	| "help" | _ -> Help
;;
(* let arg_from_string c = match c with
	| "::email::" -> Email
	| "::contractor_name::" -> Contractor.Name
	| "::rate::" -> HourlyRate
	| "::amount::" -> Amount
	| "::tva::" -> Tva
	| "::amount_total::" -> AmountTotal
	| "::date::" -> InvoiceDate
	| _ -> ()
;; *)
let value_for_placeholder placeholder = match placeholder with
	| "::email::" -> !email
	| "::contractor_name::" -> placeholder
	| "::rate::" -> string_of_float !hourly_rate
	| "::amount::" -> string_of_int !amount
	| "::tva::" -> string_of_int !tva
	| "::amount_total::" -> string_of_int !amount_total
	| "::date::" -> !invoice_date
	| _ -> placeholder
;;
let read_file file_name =
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
	"::amount_total::";
	"::date::"
]
let rec iterate_placeholders placeholders lineString = match placeholders with
	| [] -> lineString
	| placeholder::body -> 
		begin
			let new_lineString = Str.global_replace (Str.regexp placeholder) (value_for_placeholder placeholder) lineString in
			iterate_placeholders body new_lineString
		end
;;
let make_replacements s = match s with
	| ss -> iterate_placeholders placeholders ss
;;
let rec write_file file_o = function 
  | [] -> ()
  | e::tl -> 
	  Printf.fprintf file_o "%s\n" (make_replacements e);
	  write_file file_o tl
;;
let generate_pdf_from_html_in_directory dir =
	let html_o_path = dir ^ "/invoice.html" in
	let pdf_o_path = dir ^ "/invoice.pdf" in
	try (Unix.execvp "wkhtmltopdf" [| "wkhtmltopdf"; html_o_path; pdf_o_path |]) with
	Unix_error(err, _, _) -> printf "Pdf not generated, you can install wkhtmltopdf to generate pdfs"
;;

let main =
begin
let speclist = [
	("-amount", Arg.Set_int amount, "Amount to be paid");
	("-tva", Arg.Set_int tva, "TVA");
	("-hours", Arg.Set_float hours, "Amount of worked hours");
	("-email", Arg.Set_string email, "Email to be replaced in ::email::");
	("-series", Arg.Set_string invoice_series, "Series of invoice");
	("-nr", Arg.Set_int invoice_nr, "Number of invoice");
	("-rate", Arg.Set_float hourly_rate, "Series of invoice");
	("-exchange-rate", Arg.Set_float exchange_rate, "Series of invoice");
	("-date", Arg.Set_string invoice_date, "Date of invoice");
]
in let usage_msg = "Usage:"
in Arg.parse speclist (fun anon -> command := command_from_string anon) usage_msg;

match !command with
	| Generate ->
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		let last_invoice_dir = Array.get children (Array.length children -1) in
		let template = read_file (dir ^ "/0/template.html") in
		let html_o_path = last_invoice_dir ^ "/invoice.html" in
	  	let file_o = open_out html_o_path in
	  	write_file file_o template;
	  	close_out file_o;
		
		let json = Yojson.Basic.from_file (dir ^ "/0/data.json") in
		let open Yojson.Basic.Util in
	 	let date = json |> member "date" |> to_string in
	 	let series = json |> member "series" |> to_string in
	  	print_endline (date ^ series);
		
		(* Write json *)
		let json_o_path = last_invoice_dir ^ "/data.json" in
		let (person : Yojson.Basic.json) = `Assoc [ ("invoice_series", `String !invoice_series) ] in
		Yojson.Basic.to_file json_o_path person;
		
		(* (generate_pdf_from_html_in_directory last_invoice_dir) *)
		print_endline ("Thank you for generating the invoice from command line, you're on a good track for mastering the cmd!")
	| List ->
		print_endline ("List existing invoices");
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		Array.iter print_endline children;
	| Help ->
		print_endline ("Print help");

end

let () = main
