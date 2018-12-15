CC=ocamlc
BASE1=str.cma
BASE2=graphics.cma
all : 
	$(CC) $(BASE1) $(BASE2) graphic.ml -o bin/module
