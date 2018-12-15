.PHONY: all run clean
GENERATED=src/parser.mli src/parser.ml src/lexer.ml
OCAMLC=ocamlopt

ifeq ($(OS),Windows_NT)
	DEL=del /f
else
	DEL=rm -f
endif

all: main.exe

run: all
	./main.exe

src/translation.cmi: src/tree.cmi
	cd src && $(OCAMLC) -c translation.ml

src/tree.cmi:
	cd src && $(OCAMLC) -c tree.ml

main.exe: $(GENERATED) src/tree.cmi src/translation.cmi
	cd src && $(OCAMLC) tree.ml translation.ml $(GENERATED:src/%=%) main.ml -o ../main.exe

$(GENERATED): src/lexer.mll src/parser.mly
	ocamllex src/lexer.mll
	menhir -v src/parser.mly

clean:
	cd src && $(DEL) $(GENERATED:src/%=%) *.c* *.o*
	$(DEL) main.exe
