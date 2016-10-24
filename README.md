# Invoice cmd
Command line app to generate invoices. Data is stored in jsons so it will be easy to transfer and use in another places.

![Screenshot]()

### Motivation
If you're like me, you probably have a monster like Word suite installed on you computer just to generate invoices. That hundreds of mega bytes are staying on my head knowing they are not used. Why not generate them from the command line, we are programmers after all and we love the command line?

### Commands

Go to the folder where the invoices of your company are.

List the invoices you have generated in the past:

	invoice list

Generate a new invoice. Common data from the last invoice will be used (and incremented when necessary) in the current invoice:

	invoice generate -date 2016.11.16 -hours 184 -value 5776 -tva 0 -rate 9.25 -exchange-rate 4.44 -series IMG -nr 66

### Installing

 1. Download the executable from build directory then run

	sudo ./invoice install

 2. Download the sources and compile yourself. To compile you need the Ocaml compiler.
 	
	haxelib install hxcpp // Installs the hxcpp dependency
	haxe compile.hxml // Compile the application
	cd build
	sudo ./invoice install
	invoice

### Dependencies

Invoice cmd can generate html pages that you can open in a browser and print, but if you prefer it can generate pdf with the help of http://wkhtmltopdf.org You just need to install it.