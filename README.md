# Invoice cmd
If you're like me, you probably have a monster like Word installed on you computer just to generate invoices. Why not generate them from the command line?

![Screenshot](http://s18.postimg.org/vv7c6n8cp/jit.png)

### Commands

Go to the folder where the invoices of your company are.

List the invoices you have generated in the past:

	invoice list

Generate a new invoice. Common data from the last invoice will be used (and incremented when necessary) in the current invoice:

	invoice generate 2016.11.16 -hours 184 -value 5776 -tva 0 -rate 9.25 -exchange-rate 4.44 -series IMG -nr 66

### Installing

 1. Download the executable from build directory then run

	sudo ./invoice install

 2. Download the sources and compile yourself. To compile you need the Haxe compiler (http://haxe.org) and the hxcpp haxelib. Note that the cpp folder is added to .gitignore, please create it in the root before compiling. After you compile you still need to do step 1 if you want to call the app from anywhere
 	
	haxelib install hxcpp // Installs the hxcpp dependency
	haxe compile.hxml // Compile the application
	cd build
	sudo ./invoice install
	invoice

### Dependencies

Invoice cmd can generate html pages that you can open in a browser and print, but if you prefer it can generate pdf with the help of http://wkhtmltopdf.org