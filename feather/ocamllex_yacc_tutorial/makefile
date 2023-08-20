#  ocamllex lexer.mll        
#  ocamlyacc parser.mly     
#  ocamlc -c parser.mli
#  ocamlc -c lexer.ml
#  ocamlc -c parser.ml
#  ocamlc -c calc.ml
#  ocamlc -o calc lexer.cmo parser.cmo calc.cmo

SOURCES = lexer.mll parser.mly calc.ml 

run : calc
	./calc

parser.mli parser.ml : parser.mly
	ocamlyacc parser.mly

parser.cmi : parser.mli
	ocamlc -c parser.mli

parser.cmo : parser.cmi
	ocamlc -c parser.ml

lexer.ml : lexer.mll
	ocamllex lexer.mll

lexer.cmo lexer.cmi : lexer.ml parser.cmo
	ocamlc -c lexer.ml

obj_calc.cmo = parser.cmo parser.cmi lexer.cmo lexer.cmi
calc.cmo calc.cmi : calc.ml $(obj_calc.cmo)
	ocamlc -c calc.ml

obj_calc = lexer.cmo parser.cmo calc.cmo
calc : $(obj_calc)
	ocamlc -o calc $(obj_calc) 

.PHONY : clean
clean :
	rm -f lexer.ml
	rm -f parser.ml parser.mli
	rm -f parser.cmi parser.cmo
	rm -f lexer.cmi lexer.cmo
	rm -f calc.cmi calc.cmo
	rm -f calc

