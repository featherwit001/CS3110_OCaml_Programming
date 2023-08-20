OCAMLC = ocamlc
OCAMLC_FLAGS = -I +ounit2

how_to_make = dune makefile
SOURCES = lexer.mll parser.mly main.ml ast.ml cal_test.ml

# in utop execuate 
#load "my_lexer.cmo";;
#load "my_parser.cmo";;
#use "main.ml";;
utop : my_lexer.cmo my_parser.cmo main.ml
	utop

all : main cal_test

# in order to avoid the same name module in ocaml lib, 
# add the prefix my_ 
# without which will cause a error (inconsist...) 
# when I execuate these three commamds above in utop
my_parser.mli my_parser.ml : parser.mly 
	menhir parser.mly
	mv parser.mli my_parser.mli
	mv parser.ml my_parser.ml

my_parser.cmi : my_parser.mli ast.cmi
	$(OCAMLC) -c my_parser.mli 

my_parser.cmo : my_parser.cmi
	$(OCAMLC) -c my_parser.ml

my_lexer.ml : lexer.mll
	ocamllex lexer.mll
	mv lexer.ml my_lexer.ml

my_lexer.cmo my_lexer.cmi : my_lexer.ml my_parser.cmo
	$(OCAMLC) -c my_lexer.ml my_parser.cmo

ast.cmo ast.cmi : ast.ml
	$(OCAMLC) -c ast.ml

obj_cmo_cmi = ast.cmo ast.cmi my_parser.cmo my_parser.cmi my_lexer.cmo my_lexer.cmi
main.cmo main.cmi : main.ml $(obj_cmo_cmi)
	$(OCAMLC) -c main.ml

obj_all_cmo = my_parser.cmo my_lexer.cmo ast.cmo main.cmo 
main : $(obj_all_cmo)
	$(OCAMLC) -o main $(obj_all_cmo)

main.byte :  $(obj_all_cmo)
	$(OCAMLC) -o main.byte $(obj_all_cmo)

# this path depends on your own project structure
# but the cal_test.test must in the /_build/defualt ... 
test : cal_test main
	../../_build/default/chapter9/calculator_menhir_ocamllex/cal_test.exe

cal_test : cal_test.ml main
	dune build

.PHONY : clean
clean :
	rm -f my_lexer.ml
	rm -f my_parser.ml my_parser.mli
	rm -f my_parser.cmi my_parser.cmo
	rm -f my_lexer.cmi my_lexer.cmo
	rm -f main.cmi main.cmo
	rm -f ast.cmi ast.cmo
	rm -f main
	dune clean

