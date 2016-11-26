#!/bin/bash
ocamlc -c main.ml
ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c simp.ml
ocamlc -o simp lexer.cmo parser.cmo main.cmo simp.cmo
