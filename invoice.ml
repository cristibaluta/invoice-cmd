open Yojson.Basic
open Yojson.Basic.Util
open Str
open Printf
open Arg
open Unix

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

let value_for_placeholder placeholder (j : Yojson.Basic.json) = match placeholder with
	| "invoice_nr" -> string_of_int (j |> member "invoice_nr" |> to_int)
	| "rate"
	| "exchange_rate"
	| "units"
	| "amount"
	| "amount_per_unit"
	| "tva"
	| "amount_total" -> Printf.sprintf "%.2f" (j |> member placeholder |> to_float)
	| _ -> j |> member placeholder |> to_string
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
let placeholders = ["email"; "phone"; "web"; "invoice_date"; "invoice_series"; "invoice_nr";
	"contractor_name"; "contractor_orc"; "contractor_cui"; "contractor_address"; "contractor_county"; "contractor_bank_account"; "contractor_bank_name";
	"client_name"; "client_orc"; "client_cui"; "client_address"; "client_county"; "client_bank_account"; "client_bank_name";
	"delegate_name"; "delegate_ci_series"; "delegate_ci_nr"; "delegate_ci_released_by";
	"product"; "rate"; "exchange_rate"; "units"; "amount"; "amount_per_unit";
	"tva"; "amount_total"; "currency"
]
let rec iterate_placeholders placeholders lineString (j : Yojson.Basic.json) = match placeholders with
	| [] -> lineString
	| placeholder::body -> 
		begin
			let new_lineString = Str.global_replace (Str.regexp ("::" ^ placeholder ^ "::")) (value_for_placeholder placeholder j) lineString in
			iterate_placeholders body new_lineString j
		end
;;
let make_replacements s (j : Yojson.Basic.json) = match s with
	| ss -> iterate_placeholders placeholders ss j
;;
(** Update a value for a given key *)
let update ~key ~new_value j : Yojson.Basic.json =
    let as_obj = Yojson.Basic.Util.to_assoc j in
    let g = List.map begin function
        | (this_key, inner) when this_key = key -> (this_key, new_value)
        | otherwise -> otherwise
      end
        as_obj
    in
    (`Assoc g)
;;
let rec write_file file_o (j : Yojson.Basic.json) = function 
  | [] -> ()
  | e::tl -> 
	  Printf.fprintf file_o "%s\n" (make_replacements e j);
	  write_file file_o j tl
;;
let generate_pdf_from_html_in_directory dir =
	let html_o_path = dir ^ "/invoice.html" in
	let pdf_o_path = dir ^ "/invoice.pdf" in
	try (Unix.execvp "_wkhtmltopdf" [| "_wkhtmltopdf"; html_o_path; pdf_o_path |]) with
	Unix_error(err, _, _) -> printf "Pdf not generated, you can install wkhtmltopdf to generate pdfs\n"
;;

let main =
begin
	(* Read the user input *)
	let usage = "invoice-cmd ©2016 Imagin soft\nCommands: \ngenerate Generates a new invoice based on the last invoice and the info from the cmd\nlist - Lists all the invoices dates and amounts\ninstall Run sudo ./invoice install in order to install the app in a proper place" in
	let man = [
		("-amount", Arg.Set_float amount, "<float> Amount to be paid");
		("-tva", Arg.Set_float tva, "<float> VAT");
		("-email", Arg.Set_string email, "<string> Email to be replaced in ::email::");
		("-series", Arg.Set_string invoice_series, "<string> Series of the invoice");
		("-nr", Arg.Set_int invoice_nr, "<int> Number of the invoice. If missing, it will be taken and incremented from last invoice");
		("-rate", Arg.Set_float rate, "<float> Hourly rate");
		("-units", Arg.Set_float units, "<float> Amount of worked hours");
		("-exchange-rate", Arg.Set_float exchange_rate, "<float> Currency conversion rate");
		("-date", Arg.Set_string invoice_date, "<year.month.day> Date of invoice, written with numbers");
	] in
	let usage_msg = "Usage:" in
	let command = ref "help" in
	Arg.parse man (fun anon -> command := anon) usage_msg;

	match !command with
		| "generate" ->
			
			(* Read json from last invoice dir *)
			let dir = Sys.getcwd() in
			let children = Sys.readdir dir in
			let last_invoice_dir = Array.get children (Array.length children -1) in
			let html_template = read_file (dir ^ "/0/template.html") in
			let json_i_path = dir ^ "/" ^ last_invoice_dir ^ "/data.json" in
			(* Open json *)
			let json = ref (Yojson.Basic.from_file json_i_path) in
			let open Yojson.Basic.Util in
		 	let _email = !json |> member "email" |> to_string in
		 	let _invoice_date = !json |> member "invoice_date" |> to_string in
		 	let _invoice_series = !json |> member "invoice_series" |> to_string in
		 	let _invoice_nr = !json |> member "invoice_nr" |> to_int in

		 	let _product = !json |> member "product" |> to_string in
		 	let _rate = !json |> member "rate" |> to_float in
		 	let _exchange_rate = !json |> member "exchange_rate" |> to_float in
		 	let _units = !json |> member "units" |> to_float in
		 	let _amount = !json |> member "amount" |> to_float in
		 	let _amount_per_unit = !json |> member "amount_per_unit" |> to_float in

		 	let _tva = !json |> member "tva" |> to_float in
		 	let _amount_total = !json |> member "amount_total" |> to_float in
		 	let _currency = !json |> member "currency" |> to_string in
			
			(* Do some calculations for the changing fields *)
			invoice_nr := _invoice_nr + 1;
			amount_per_unit := !rate *. !exchange_rate;
			amount_total := !amount +. !amount *. !tva /. 100.0;
			
			(* Write new json *)
			let new_invoice_dir = !invoice_date in
			if not (Sys.file_exists new_invoice_dir) then
				Unix.mkdir new_invoice_dir 0o755;
			let json_o_path = new_invoice_dir ^ "/data.json" in
			json := update "invoice_nr" (`Int !invoice_nr) !json;
			json := update "amount" (`Float !amount) !json;
			json := update "amount_total" (`Float !amount_total) !json;
			json := update "amount_per_unit" (`Float !amount_per_unit) !json;
			json := update "invoice_date" (`String "invoice_date") !json;
			json := update "rate" (`Float !rate) !json;
			json := update "exchange_rate" (`Float !exchange_rate) !json;
			json := update "units" (`Float !units) !json;
			Yojson.Basic.to_file json_o_path !json;

			(* Write html *)
			let html_o_path = new_invoice_dir ^ "/invoice.html" in
		  	let file_o = open_out html_o_path in
		  	write_file file_o !json html_template;
		  	close_out file_o;
			printf "Html generated\n";

			generate_pdf_from_html_in_directory last_invoice_dir;
			print_endline ("Thank you for generating the invoice from command line!")
		| "list" ->
			print_endline ("List existing invoices");
			let dir = Sys.getcwd() in
			let children = Sys.readdir dir in
			Array.iter print_endline children;
		| "help" | "" ->
			print_endline ("Print help");
			Arg.usage man usage
		| "install" ->
			print_endline ("Installing to /usr/local/bin/");
			try (Unix.execvp "mv" [| "mv"; "-i"; "invoice"; "/usr/local/bin/invoice" |]) with
			Unix_error(err, _, _) -> printf "Can't install, please run with sudo\n";
		| _ -> ()
end

let () = main
