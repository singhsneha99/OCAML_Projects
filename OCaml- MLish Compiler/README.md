# MLish Compiler

This project involves:

1. Type checking and inference for MLish 
2. Compiling MLish to equivalent Scish code

Where MLish is a subset of the ML language.

## MLish Language 

Key aspects:

- No modules, references, pattern matching 
- Minimal type inference 
- Lexer, parser and AST provided

Focused on getting basics of type checking working.

## Implementation

`mlish_type_check.ml`:

- Implement `type_check_exp` to return inferred type or raise exception
- Follow notes from class for Hindley-Milner style inference
- Support let polymorphism as final step

`mlish_compile.ml`: 

- Translate MLish AST to equivalent Scish AST
- Can further compile to Cish and then MIPS

## Testing

`make` generates executable `ps6_mlish` which can be invoked for:

- Type checking: `./ps6_mlish typecheck FILE`
- Compilation: `./ps6_mlish compile FILE` 

Can also test type checker by evaluating in OCaml REPL.