CC=ocamlc
BASE1=str.cma
BASE2=graphics.cma
EXE=mastermind
all : 
	$(CC) $(BASE1) $(BASE2) code.ml naif.ml niveau3.ml  niveau2.ml knuth.ml ia.ml interface.ml -o $(EXE)

