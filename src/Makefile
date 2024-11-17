SOURCES = eval.ml type.ml ./tests/entiers.ml ./tests/let.ml ./tests/listes.ml ./tests/point_fixe.ml ./tests/produit.ml ./tests/somme.ml ./tests/test_types.ml
OBJECTS = $(SOURCES:.ml=.cmo)
EXEC = main

all: $(EXEC)

$(EXEC): $(OBJECTS)
	ocamlc -o $(EXEC) $(OBJECTS)

%.cmo: %.ml
	ocamlc -c $<

clean:
	rm -f *.cmo *.cmi $(EXEC)
	rm -f ./tests/*.cmo ./tests/*.cmi
