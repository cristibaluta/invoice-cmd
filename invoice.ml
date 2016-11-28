open Yojson.Basic
open Yojson.Basic.Util
open Str
open Printf
open Arg
open Unix

let exchange_rate_precision = ref 2
let amount_precision = ref 2
let number_of_decimals value =
	let str_value = string_of_float value in
	let comps = Str.split (Str.regexp "\\.") str_value in
	let decimals = List.nth comps 1 in
	String.length decimals
;;
let value_with_precision value precision = match precision with
	| 0 -> Printf.sprintf "%.0f" value
	| 1 -> Printf.sprintf "%.1f" value
	| 2 -> Printf.sprintf "%.2f" value
	| 3 -> Printf.sprintf "%.3f" value
	| 4 -> Printf.sprintf "%.4f" value
	| _ -> Printf.sprintf "%f" value
;;
let value_for_placeholder placeholder (j : Yojson.Basic.json) = match placeholder with
	| "invoice_nr" -> Printf.sprintf "%03d" (j |> member placeholder |> to_int)
	| "exchange_rate"
	| "amount_per_unit" -> value_with_precision (j |> member placeholder |> to_float) !exchange_rate_precision
	| "amount"
	| "amount_total" -> value_with_precision (j |> member placeholder |> to_float) !amount_precision
	| "units"
	| "tva"
	| "rate" -> value_with_precision (j |> member placeholder |> to_float) 2
	| _ -> j |> member placeholder |> to_string
;;
let load_file file =
	let in_channel = open_in file in
	let rec read_recursive lines =
		try
			let line = input_line in_channel in
			read_recursive (line :: lines)
		with End_of_file -> lines in
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
	try (Unix.execvp "wkhtmltopdf" [| "wkhtmltopdf"; html_o_path; pdf_o_path |]) with
	Unix_error(err, _, _) -> printf "Pdf not generated, you can install wkhtmltopdf to generate pdfs\n"
;;

(* Input vars *)
let invoice_nr = ref 0
let rate = ref 0.0
let exchange_rate = ref 0.0
let units = ref 0.0
let amount = ref 0.0
let tva = ref 0.0
let invoice_date = ref ""
let generate_pdf_enabled = ref false
let is_invoice_folder dir =
	Str.string_match (Str.regexp "[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]") dir 0
;;
let list_of_invoices dir =
	let subdirs = Sys.readdir dir in
	let subdirs_list = Array.to_list subdirs in
	let invoices = List.filter (fun s -> is_invoice_folder s) subdirs_list in
	invoices
;;
let find_last_invoice_dir in_dir new_date =
	let invoices = list_of_invoices in_dir in
	let invoices_reversed = List.rev invoices in
	if List.length invoices_reversed > 0 then begin
		let last_date = ref (List.hd invoices_reversed) in
		if !last_date = new_date && List.length invoices_reversed > 1 then
			last_date := (List.nth invoices_reversed 1)
		else
			last_date := "0";
		!last_date
	end else "0"
;;
let set_pdf b =
	generate_pdf_enabled := b
;;
let print_invoice_details invoice =
	let cwd = Sys.getcwd() in
	let json_path = cwd ^ "/" ^ invoice ^ "/data.json" in
	let json = ref (Yojson.Basic.from_file json_path) in
	let open Yojson.Basic.Util in
	let series = !json |> member "invoice_series" |> to_string in
	let nr = !json |> member "invoice_nr" |> to_int in
	let exchange_rate = !json |> member "exchange_rate" |> to_float in
	let currency = !json |> member "currency" |> to_string in
	let amount = !json |> member "amount" |> to_float in
	let rate = !json |> member "rate" |> to_float in
	let units = !json |> member "units" |> to_float in
	print_endline (invoice ^ " -> " ^ 
		series ^ 
		(Printf.sprintf "%03d" nr) ^ " " ^ 
		(string_of_float exchange_rate) ^ "€/" ^ currency ^ " " ^
		(Printf.sprintf "%.2f" rate) ^ "€/hour " ^ 
		(Printf.sprintf "%.2f" units) ^ "hours " ^ 
		(Printf.sprintf "%.2f" amount) ^ currency)
;;

let main = begin
	(* Read the user input *)
	let usage = "invoice-cmd ©2016 Imagin soft\nUsage : \ninvoice [make|list|install] [options]\nOptions :" in
	let man = [
		("-amount", Arg.Set_float amount, "<float> Amount to be paid");
		("-tva", Arg.Set_float tva, "<float> VAT");
		("-rate", Arg.Set_float rate, "<float> Hourly rate");
		("-units", Arg.Set_float units, "<float> Amount of worked hours");
		("-hours", Arg.Set_float units, "<float> Amount of worked hours");
		("-exchange-rate", Arg.Set_float exchange_rate, "<float> Currency conversion rate");
		("-date", Arg.Set_string invoice_date, "<year.month.day> Date of invoice, ex: 2016.12.25");
		("-pdf", Arg.Bool set_pdf, "<true/false> Generate pdf. You need wkhtmltopdf installed");
	] in
	let command = ref "help" in
	Arg.parse man (fun anon -> command := anon) usage;

	match !command with
		| "make" ->
			
			if !invoice_date = "" then begin
				print_endline ("-date is a mandatory field. Run invoice -help for more info!");
				exit 0
			end else if is_invoice_folder !invoice_date = false then begin
				print_endline ("-date is in the wrong format, use <year.month.day>!");
				exit 0
			end;
			(* Find last invoice dir *)
			let cwd = Sys.getcwd() in
			let prev_invoice_dir = find_last_invoice_dir cwd !invoice_date in
			let html_template = load_file (cwd ^ "/0/template.html") in
			let prev_json_path = cwd ^ "/" ^ prev_invoice_dir ^ "/data.json" in
			(* Open json *)
			let json = ref (Yojson.Basic.from_file prev_json_path) in
			let open Yojson.Basic.Util in
		 	(* let previous_invoice_date = !json |> member "invoice_date" |> to_string in *)
		 	let previous_invoice_nr = !json |> member "invoice_nr" |> to_int in
		 	(* let previous_rate = !json |> member "rate" |> to_float in *)
		 	(* let previous_tva = !json |> member "tva" |> to_float in *)
			let amount_per_unit = ref 0.0 in
			let amount_total = ref 0.0 in
			print_endline ("Input data:");
			if !units <> 0.0 then print_endline ("  units/hours : " ^ (string_of_float !units) );
			if !amount <> 0.0 then print_endline ("  amount : " ^ (string_of_float !amount) );
			if !tva <> 0.0 then print_endline ("  tva : " ^ (string_of_float !tva) );
			if !rate <> 0.0 then print_endline ("  rate : " ^ (string_of_float !rate) );
			if !exchange_rate <> 0.0 then print_endline ("  exchange_rate : " ^ (string_of_float !exchange_rate) );
			if !invoice_date <> "" then print_endline ("  date : " ^ !invoice_date );
			
			print_endline ("Calculated data:");
			(* if !rate = 0.0 then rate := previous_rate; *)
			
			(* Do some calculations *)
			if (!exchange_rate <> 0.0 && !units <> 0.0 && !amount <> 0.0) then begin
				(* When you know the total value, exchange rate, and units *)
				
				amount_per_unit := !amount /. !units;
				rate := !amount_per_unit /. !exchange_rate;
				print_endline ("  amount_per_unit : " ^ (string_of_float !amount_per_unit) );
				print_endline ("  rate : " ^ (string_of_float !rate) )
				
			end else if (!rate <> 0.0 && !exchange_rate <> 0.0 && !amount <> 0.0) then begin
				(* When you know the rate, exchange rate, and amount *)

				amount_per_unit := !rate *. !exchange_rate;
				if !units = 0.0 then begin
					units := !amount /. !amount_per_unit;
					print_endline ("  units/hours : " ^ (string_of_float !units) )
				end;
				print_endline ("  amount_per_unit : " ^ (string_of_float !amount_per_unit) );
				
			end else begin
				
				print_endline ("Err: Not enough info to calculate the missing data! Needed data is:");
				print_endline ("  1) -exchange-rate -units -amount");
				print_endline ("  2) -rate -exchange-rate -units");
				exit 0
				
			end;
			invoice_nr := previous_invoice_nr + 1;
			amount_total := !amount +. !amount *. !tva /. 100.0;
			amount_precision := number_of_decimals !amount;
			exchange_rate_precision := number_of_decimals !exchange_rate;
			
			(* Write new json *)
			let new_invoice_dir = !invoice_date in
			if not (Sys.file_exists new_invoice_dir) then
				Unix.mkdir new_invoice_dir 0o755;
			let json_o_path = new_invoice_dir ^ "/data.json" in
			json := update "invoice_nr" (`Int !invoice_nr) !json;
			json := update "amount" (`Float !amount) !json;
			json := update "amount_total" (`Float !amount_total) !json;
			json := update "amount_per_unit" (`Float !amount_per_unit) !json;
			json := update "invoice_date" (`String !invoice_date) !json;
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

			if !generate_pdf_enabled then generate_pdf_from_html_in_directory new_invoice_dir;
			print_endline ("Thank you for generating the invoice from command line!")
		| "list" ->
			print_endline ("Existing invoices:");
			let cwd = Sys.getcwd() in
			let invoices = list_of_invoices cwd in
			List.iter print_invoice_details invoices;
		| "help" | "" ->
			Arg.usage man usage
		| "install" ->
			let executable_path = (match Sys.os_type with
				| "Unix" -> "/usr/local/bin/"
				| "Win32" | "Cygwin" -> "c://program files/"
				| _ -> "") in
			print_endline ("Installing to " ^ executable_path);
			(try 
				(Unix.execvp "mv" [| "mv"; "-i"; "invoice"; (executable_path ^ "invoice") |]) 
			with
				Unix_error(err, _, _) -> printf "Can't install, please run with sudo\n");
			
			print_endline ("Great, you can run invoice from anywhere on your computer now!")
		| _ ->
			print_endline ("Inexistent command")
end

let () = main
