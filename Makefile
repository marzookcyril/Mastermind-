<<<<<<< HEAD
CC=ocamlc
BASE1=str.cma
BASE2=graphics.cma
all : 
	$(CC) $(BASE1) $(BASE2) graphic.ml -o bin/module
=======
CC = ocamlc
BASE1 = str.cma
all : 
	$(CC) $(BASE1) code.ml naif1.ml naif2.ml  naifc.ml knuth.ml ia.ml  -o bin/module
>>>>>>> ia
