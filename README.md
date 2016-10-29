# Invoice cmd
Command line app to generate invoices. Data is stored in jsons so it will be easy to transfer and use in another places.

![Screenshot](https://s13.postimg.org/ndmkhvfyf/Screen_Shot_2016_10_24_at_09_47_46.png)
![Screenshot](https://s15.postimg.org/9sfcggejv/Screen_Shot_2016_10_29_at_21_15_08.png)
![Screenshot](https://s12.postimg.org/3p9489pb1/Screen_Shot_2016_10_29_at_21_15_40.png)

### Motivation
If you're like me, you probably organised your life away from useless softwares like Microsoft Word suite. The hundreds of mega bytes that it takes. Why not generate them from the command line? Ee are programmers after all and we love the command line.

### Commands

Go to the folder where the invoices of your company are.

List the invoices you have generated in the past:

	invoice list

Generate a new invoice. Data from the last invoice will be used (and incremented when necessary) in the current invoice. In this way if details changes you don't need to change the template data

	invoice generate -date 2016.11.16 -hours 184 -value 5776 -tva 0 -rate 9.25 -exchange-rate 4.44 -series IMG -nr 66

### Installing

Download the invoice executable (Mac 64 only) then run

	sudo ./invoice install

Download the sources and compile yourself. To compile you need the Ocaml compiler, opam libs manager, and yojson lib with it's dependencies, but opam will take care of this.
 
 To install ocaml get it from https://github.com/ocaml/ocaml and follow their instructions.
 To install opam get it from git at https://github.com/ocaml/opam Then run this commands
 	
	
	./configure
	make
	make install // with sudo if it doesn't work
	
	opam init
	opam config env
	opam install yojson
	opam config env
	

Compile the invoice app with:
	
	// Quick compile
	ocamlfind ocamlc str.cma unix.cma invoice.ml -o invoice -package yojson
	// Compile standalone/shippable app
	ocamlfind ocamlopt str.cmxa unix.cmxa invoice.ml -o invoice -package yojson -linkpkg
	
	// If you have Haxe installed you can use this command to compile and run a sample
	haxe compile.hxml
	
	sudo ./invoice install
	invoice
	

### Dependencies

Invoice cmd can generate html pages that you can open in a browser and then print, but if you prefer it can generate pdf with the help of http://wkhtmltopdf.org You just need to install it.