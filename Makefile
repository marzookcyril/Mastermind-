CC=ocamlc
BASE1=str.cma
BASE2=graphics.cma
EXE=game
all : 
	$(CC) $(BASE1) $(BASE2) code.ml naif1.ml naif2.ml  naifc.ml knuth.ml ia.ml interface.ml -o $(EXE)

