CC = ocamlc
BASE1 = str.cma
all : 
	$(CC) $(BASE1) code.ml naif1.ml naif2.ml  naifc.ml knuth.ml ia.ml  -o bin/module
