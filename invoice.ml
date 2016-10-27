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

let value_for_placeholder placeholder = match placeholder with
	| "::email::" -> !email
	| "::phone::" -> !phone
	| "::web::" -> !web
	| "::invoice_date::" -> !invoice_date
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
	| "::rate::" -> Printf.sprintf "%.2f" !rate
	| "::exchange_rate::" -> Printf.sprintf "%.2f" !exchange_rate
	| "::units::" -> Printf.sprintf "%.2f" !units
	| "::amount::" -> Printf.sprintf "%.2f" !amount
	| "::amount_per_unit::" -> Printf.sprintf "%.2f" !amount_per_unit

	| "::tva::" -> Printf.sprintf "%.2f" !tva
	| "::amount_total::" -> Printf.sprintf "%.2f" !amount_total
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
let placeholders = ["::email::"; "::phone::"; "::web::"; "::invoice_date::"; "::invoice_series::"; "::invoice_nr::";
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
 	let _email = json |> member "email" |> to_string in
 	let _phone = json |> member "phone" |> to_string in
 	let _web = json |> member "web" |> to_string in
 	let _invoice_date = json |> member "invoice_date" |> to_string in
 	let _invoice_series = json |> member "invoice_series" |> to_string in
 	let _invoice_nr = json |> member "invoice_nr" |> to_int in

 	let _contractor_name = json |> member "contractor_name" |> to_string in
 	let _contractor_orc = json |> member "contractor_orc" |> to_string in
 	let _contractor_cui = json |> member "contractor_cui" |> to_string in
 	let _contractor_address = json |> member "contractor_address" |> to_string in
 	let _contractor_county = json |> member "contractor_county" |> to_string in
 	let _contractor_bank_account = json |> member "contractor_bank_account" |> to_string in
 	let _contractor_bank_name = json |> member "contractor_bank_name" |> to_string in
 	let _client_name = json |> member "client_name" |> to_string in
 	let _client_orc = json |> member "client_orc" |> to_string in
 	let _client_cui = json |> member "client_cui" |> to_string in
 	let _client_address = json |> member "client_address" |> to_string in
 	let _client_county = json |> member "client_county" |> to_string in
 	let _client_bank_account = json |> member "client_bank_account" |> to_string in
 	let _client_bank_name = json |> member "client_bank_name" |> to_string in

 	let _delegate_name = json |> member "delegate_name" |> to_string in
 	let _delegate_ci_series = json |> member "delegate_ci_series" |> to_string in
 	let _delegate_ci_nr = json |> member "delegate_ci_nr" |> to_string in
 	let _delegate_ci_released_by = json |> member "delegate_ci_released_by" |> to_string in

 	let _product = json |> member "product" |> to_string in
 	let _rate = json |> member "rate" |> to_float in
 	let _exchange_rate = json |> member "exchange_rate" |> to_float in
 	let _units = json |> member "units" |> to_float in
 	let _amount = json |> member "amount" |> to_float in
 	let _amount_per_unit = json |> member "amount_per_unit" |> to_float in

 	let _tva = json |> member "tva" |> to_float in
 	let _amount_total = json |> member "amount_total" |> to_float in
 	let _currency = json |> member "currency" |> to_string in
	();
	
	email := _email;
	phone := _phone;
	web := _web;
	invoice_date := if !invoice_date == "" then _invoice_date else !invoice_date;
	invoice_series := _invoice_series;
	invoice_nr := _invoice_nr;

	contractor_name := _contractor_name;
	contractor_orc := _contractor_orc;
	contractor_cui := _contractor_cui;
	contractor_address := _contractor_address;
	contractor_county := _contractor_county;
	contractor_bank_account := _contractor_bank_account;
	contractor_bank_name := _contractor_bank_name;
	client_name := _client_name;
	client_orc := _client_orc;
	client_cui := _client_cui;
	client_address := _client_address;
	client_county := _client_county;
	client_bank_account := _client_bank_account;
	client_bank_name := _client_bank_name;

	delegate_name := _delegate_name;
	delegate_ci_series := _delegate_ci_series;
	delegate_ci_nr := _delegate_ci_nr;
	delegate_ci_released_by := _delegate_ci_released_by;

	product := _product;
	rate := _rate;
	exchange_rate := _exchange_rate;
	units := _units;
	amount := _amount;
	amount_per_unit := _amount_per_unit;

	tva := _tva;
	amount_total := _amount_total;
	currency := _currency;
	
	(* Do some calculations for the changing fields *)
	invoice_nr := _invoice_nr + 1;
	amount_per_unit := !rate *. !exchange_rate;
	amount_total := !amount +. !amount *. !tva /. 100.0
;;
let generate_json path =
	let (person : Yojson.Basic.json) = `Assoc [
		("email", `String !email);
		("phone", `String !phone);
		("web", `String !web);
		("invoice_date", `String !invoice_date);
		("invoice_series", `String !invoice_series);
		("invoice_nr", `Int !invoice_nr);
		
		("contractor_name", `String !contractor_name);
		("contractor_orc", `String !contractor_orc);
		("contractor_cui", `String !contractor_cui);
		("contractor_address", `String !contractor_address);
		("contractor_county", `String !contractor_county);
		("contractor_bank_account", `String !contractor_bank_account);
		("contractor_bank_name", `String !contractor_bank_name);
		("client_name", `String !client_name);
		("client_orc", `String !client_orc);
		("client_cui", `String !client_cui);
		("client_address", `String !client_address);
		("client_county", `String !client_county);
		("client_bank_account", `String !client_bank_account);
		("client_bank_name", `String !client_bank_name);
		
		("delegate_name", `String !delegate_name);
		("delegate_ci_series", `String !delegate_ci_series);
		("delegate_ci_nr", `String !delegate_ci_nr);
		("delegate_ci_released_by", `String !delegate_ci_released_by);
		
		("product", `String !product);
		("rate", `Float !rate);
		("exchange_rate", `Float !exchange_rate);
		("units", `Float !units);
		("amount", `Float !amount);
		("amount_per_unit", `Float !amount_per_unit);
		
		("tva", `Float !tva);
		("amount_total", `Float !amount_total);
		("currency", `String !currency)
	] in
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
	try (Unix.execvp "_wkhtmltopdf" [| "_wkhtmltopdf"; html_o_path; pdf_o_path |]) with
	Unix_error(err, _, _) -> printf "Pdf not generated, you can install wkhtmltopdf to generate pdfs\n"
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
] in
let usage_msg = "Usage:" in
let command = ref Help in
let command_from_string c = (match c with
	| "generate" -> Generate
	| "list" -> List
	| "help" | _ -> Help)
in
Arg.parse speclist (fun anon -> command := command_from_string anon) usage_msg;

match !command with
	| Generate ->
		let dir = Sys.getcwd() in
		let children = Sys.readdir dir in
		let last_invoice_dir = Array.get children (Array.length children -1) in
		let template = read_file (dir ^ "/0/template.html") in
		let json_i_path = dir ^ "/" ^ last_invoice_dir ^ "/data.json" in
		open_and_parse_json json_i_path;
		
		(* Write new json *)
		let new_invoice_dir = !invoice_date in
		if not (Sys.file_exists new_invoice_dir) then
			Unix.mkdir new_invoice_dir 0o755;
		let json_o_path = new_invoice_dir ^ "/data.json" in
		generate_json json_o_path;
		
		(* Write html *)
		let html_o_path = new_invoice_dir ^ "/invoice.html" in
	  	let file_o = open_out html_o_path in
	  	write_file file_o template;
	  	close_out file_o;
		printf "Html generated\n";
		
		generate_pdf_from_html_in_directory last_invoice_dir;
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
