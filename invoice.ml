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
(* type company_details =
	| Name of string
    | Orc of string
    | Cui of string
	| Address of string
	| County of string
	| BankAccount of string
	| BankName of string
;; *)
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
let phone = ref ""
let web = ref ""
let invoice_date = ref ""
let invoice_series = ref ""
let invoice_nr = ref 0

let contractor_name = ref ""
let contractor_orc = ref ""
let contractor_cui = ref ""
let contractor_address = ref ""
let contractor_county = ref ""
let contractor_bank_account = ref ""
let contractor_bank_name = ref ""
let client_name = ref ""
let client_orc = ref ""
let client_cui = ref ""
let client_address = ref ""
let client_county = ref ""
let client_bank_account = ref ""
let client_bank_name = ref ""

let delegate_name = ref ""
let delegate_ci_series = ref ""
let delegate_ci_nr = ref ""
let delegate_ci_released_by = ref ""

let product = ref ""
let rate = ref 0.0
let exchange_rate = ref 0.0
let units = ref 0.0
let amount = ref 0.0
let amount_per_unit = ref 0.0

let tva = ref 0.0
let amount_total = ref 0.0
let currency = ref ""

let command = ref Help
let command_from_string c = match c with
	| "generate" -> Generate
	| "list" -> List
	| "help" | _ -> Help
;;
let value_for_placeholder placeholder = match placeholder with
	| "::email::" -> !email
	| "::phone::" -> !phone
	| "::web::" -> !web
	| "::date::" -> !invoice_date
	| "::invoice_series::" -> !invoice_series
	| "::invoice_nr::" -> string_of_int !invoice_nr
	
	| "::contractor_name::" -> !contractor_name
	| "::contractor_orc::" -> !contractor_orc
	| "::contractor_cui::" -> !contractor_cui
	| "::contractor_address::" -> !contractor_address
	| "::contractor_county::" -> !contractor_county
	| "::contractor_bank_account::" -> !contractor_bank_account
	| "::contractor_bank_name::" -> !contractor_bank_name
	| "::client_name::" -> !client_name
	| "::client_orc::" -> !client_orc
	| "::client_cui::" -> !client_cui
	| "::client_address::" -> !client_address
	| "::client_county::" -> !client_county
	| "::client_bank_account::" -> !client_bank_account
	| "::client_bank_name::" -> !client_bank_name

	| "::delegate_name::" -> !delegate_name
	| "::delegate_ci_series::" -> !delegate_ci_series
	| "::delegate_ci_nr::" -> !delegate_ci_nr
	| "::delegate_ci_released_by::" -> !delegate_ci_released_by
	
	| "::product::" -> !product
	| "::rate::" -> string_of_float !rate
	| "::exchange_rate::" -> string_of_float !exchange_rate
	| "::units::" -> string_of_float !units
	| "::amount::" -> string_of_float !amount
	| "::amount_per_unit::" -> string_of_float !amount_per_unit

	| "::tva::" -> string_of_float !tva
	| "::amount_total::" -> string_of_float !amount_total
	| "::currency::" -> !currency
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
let placeholders = ["::email::"; "::phone::"; "::web::"; "::date::"; "::invoice_series::"; "::invoice_nr::";
	"::contractor_name::"; "::contractor_orc::"; "::contractor_cui::"; "::contractor_address::"; "::contractor_county::"; "::contractor_bank_account::"; "::contractor_bank_name::";
	"::client_name::"; "::client_orc::"; "::client_cui::"; "::client_address::"; "::client_county::"; "::client_bank_account::"; "::client_bank_name::";
	"::delegate_name::"; "::delegate_ci_series::"; "::delegate_ci_nr::"; "::delegate_ci_released_by::";
	"::product::"; "::rate::"; "::exchange_rate::"; "::units::"; "::amount::"; "::amount_per_unit::";
	"::tva::"; "::amount_total::"; "::currency::"
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
let open_and_parse_json path =
	let json = Yojson.Basic.from_file path in
	let open Yojson.Basic.Util in
 	let _invoice_date = json |> member "invoice_date" |> to_string in
 	let _invoice_series = json |> member "invoice_series" |> to_string in
 	let _invoice_nr = json |> member "invoice_nr" |> to_int in
 	let _currency = json |> member "currency" |> to_string in
 	let _tva = json |> member "tva" |> to_float in
	let _client_json = json |> member "client" |> to_assoc in
	let _contractor_json = json |> member "contractor" |> to_assoc in
	let _client_delegate_json = json |> member "client_delegate" |> to_assoc in
	let _contractor_delegate_json = json |> member "contractor_delegate" |> to_assoc in
	let _products = json |> member "products" |> to_list in
	(* print_endline (_contractor_delegate_json |> member "email" _contractor_delegate_json |> to_string); *)
	List.iter (fun str -> print_endline (member "name" str |> to_string)) _products;
	
	(match _client_json with
	  | [] -> []  (* or   failwith "empty"  *)
	  | ab::ris -> printf "Products: %s\n" (member "name" ab |> to_string)
	);
	
	invoice_date := _invoice_date;
	invoice_series := _invoice_series;
	currency := _currency;
	tva := _tva;
	
	(* Do some calculations for the changing fields *)
	invoice_nr := _invoice_nr + 1;
	amount_total := !amount +. !amount *. !tva /. 100.0
;;
let generate_json path =
	let (person : Yojson.Basic.json) = `Assoc [ ("invoice_series", `String !invoice_series) ] in
	Yojson.Basic.to_file path person
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
	("-amount", Arg.Set_float amount, "Amount to be paid");
	("-tva", Arg.Set_float tva, "TVA");
	("-email", Arg.Set_string email, "Email to be replaced in ::email::");
	("-series", Arg.Set_string invoice_series, "Series of invoice");
	("-nr", Arg.Set_int invoice_nr, "Number of invoice");
	("-rate", Arg.Set_float rate, "Series of invoice");
	("-units", Arg.Set_float units, "Amount of worked hours");
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
		let json_i_path = dir ^ "/0/data.json" in
		open_and_parse_json json_i_path;
		
		(* Write new json *)
		let json_o_path = last_invoice_dir ^ "/data.json" in
		generate_json json_o_path;
		
		(* Write html *)
		let html_o_path = last_invoice_dir ^ "/invoice.html" in
	  	let file_o = open_out html_o_path in
	  	write_file file_o template;
	  	close_out file_o;
		
		(* (generate_pdf_from_html_in_directory last_invoice_dir) *)
		print_endline ("Thank you for generating the invoice from command line!")
	| List ->
		print_endline ("List existing invoices");
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		Array.iter print_endline children;
	| Help ->
		print_endline ("Print help");

end

let () = main
