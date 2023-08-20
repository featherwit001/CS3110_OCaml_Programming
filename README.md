# OCaml_Programming

This is a collection of code that is associated with CS3110,OCaml Programming: Correct + Efficient + Beautiful including **menhir or ocamlyacc and ocamllex** example, AVL tree and **Red-black tree** implement and explanation, code testing using Ounit2, QCheck, detection the coverage of test suite by bisect_ppx and so on.


## feather fold

It includes codes writed by myself, whose origin is the open videos and textbook.
Its subfolder exercises/ includes my solutions for some hard exercises from textbook, which mostly are four stars.

### ocamllex_yacc_tutorial

It is the complete example given by [OCaml Mannul](https://v2.ocaml.org/manual/lexyacc.html).

### chapter9/calculator_menhir_ocamllex

CS3110 show an example calculator to introduce ocamllex and ocamlyacc. 

However, there was no ready-made code available, ~~and the code in the video had errors~~(because I used ocamlyacc initially you could take another fold "chapter9/calculator_ocamlyacc_ocamllex" as a reference)
and lacked a makefile. So, I referred to several projects and wrote a makefile myself, fixed the error, and completed the workflow integration. 

People studying this course can directly refer to all the files in this directory. Pay attention to the commands in the makefile, such as make utop, make all, and make clean, as well as the comments.


## tmp_bisect fold

It is used to ensure the unit testing covering each sentences and conditions.

## ocaml-red-balck-trees

It contains several the red-black tree implememts written by OCaml, from which we could understand the advantages of OCaml's handding of different scenarios.

However, I searched for a lot of information and didn't find a comprehensive explanation for red-black tree deletion. So, I decided to do it myself, and then implement it. 

Successfully, I did it and discovered that this implementation above matched my theoretical deduction results completely. 

Afterward, I will add my own Chinese theoretical deduction -----a comprehensive, detailed, easy-to-understand, and easy-to-implement explanation,including my own implementation,to the repository
- [ ] done 

## formal_vertification
It is formal vertification for mostly proof memtioned in the videos except the first one "even(2*n) = true".

## dune_project
It is the remains after I explore the best practice for building project

