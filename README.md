## Introduction

This project is a OCaml implementation of a fragment of Prolog. This project includes a grammar, a lexer, a parser, a semantic, and an evaluator (supporting back-tracking).

## A list of files

* parser.mly -- Parser
* lexer.mll  -- Lexer
* main.ml    -- Main functionality of unification and backtracking.
* simp.ml    -- Interactive environment interface.

## Installation

* install OCaml 4.03.0.
* compile SimProlog by executing `. mk` in terminal.
* launch interactive interface by executing `./simp` in terminal.

## Examples and more

Readers are encouraged to read report/report.pdf for more information including examples, discussion on design choices and a breif explanation of backtracking.
