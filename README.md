# Invoice cmd
Command line app to generate invoices. Data is stored in jsons so it will be easy to move and use them in another places.

This is the html template
![Screenshot](https://s13.postimg.org/ndmkhvfyf/Screen_Shot_2016_10_24_at_09_47_46.png)

You run this command with the minimum information
![Screenshot](https://s15.postimg.org/9sfcggejv/Screen_Shot_2016_10_29_at_21_15_08.png)

And this is your invoice ready to print
![Screenshot](https://s12.postimg.org/3p9489pb1/Screen_Shot_2016_10_29_at_21_15_40.png)

### Motivation
If you're like me, you probably organised your life away from monstruous and useless softwares like Microsoft Word suite. Invoicing websites are a good alternative, but is not probable to pay them to generate one invoice a month. So what i used so far, and i'm not alone, was a word/excel doc that each month needed to be duplicated and updated with the new values. This takes useless time and space. Why not generate the invoices from the command line? We are programmers after all.

### Commands

Go to the folder where the invoices of your company are.

List the invoices you have generated in the past:

	invoice list

Generate a new invoice. Data from the last invoice will be used (and incremented when necessary) in the current invoice. In this way if details changes you don't need to change the first template json

	invoice generate -date 2016.11.16 -hours 184 -amount 1000 -tva 0 -exchange-rate 4.44

### Installing

Option 1: Download the invoice executable (Mac 64 only) then run this to make it available from anywhere:

	sudo ./invoice install

Option 2: Download the sources and compile yourself. To compile you need the Ocaml compiler, Opam libs manager, and Yojson lib with it's dependencies (but opam will take care of this)
 
 To install ocaml get it from https://github.com/ocaml/ocaml and follow their instructions.
 To install opam get it from git at https://github.com/ocaml/opam Then run this commands
 	
	
	./configure
	make
	make install // with sudo if it doesn't work
	
	opam init
	opam config env
	opam install yojson
	opam config env
	

Compile the invoice for byte-code, which means it needs ocaml to execute it

	ocamlfind ocamlc str.cma unix.cma invoice.ml -o invoice -package yojson -linkpkg

Compile standalone/shippable app

	ocamlfind ocamlopt str.cmxa unix.cmxa invoice.ml -o invoice -package yojson -linkpkg

Optionally, if you have Haxe installed you can use this command to compile and run the sample in one shot

	haxe compile.hxml

### Dependencies

Invoice cmd can generate html pages that you can open in a browser and then print, but if you prefer it can generate pdf with the help of http://wkhtmltopdf.org You just need to install it.

### Contribution

- You can help this software grow by submitting invoicing templates as a html (use pull requests for that) or even doc/pdf/screenshot (open an issue for that)
- Need two people willing to make builds for windows and linux from time to time. You need ocaml installed, which is the hardest part.
