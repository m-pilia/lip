Simple Functional Language Interpreter - Programming Languages Class Final Project
==================================================================================
This is an interpreter for a simplified functional language. Two different implementations provide static scope and dynamic scope with deep binding.

The `parser` folder contains a camlp4 parser, useful for testing. The `run` script permits to launch the interpreter easily.

Build and Run
=============
Run `make` to build the parser, then:
* `./run static program.fn` to run the source file `program.fn` (change static with dynamic to use the dinamically scoped interpreter)
* `./run static program.fn out.ml` to generate the OCaml code for the abstract syntax of the program and put it inside `out.ml`


License
=======
The project is licensed under GPL 3. See [LICENSE](./LICENSE)
file for the full license.
