# Types and Programming Languages

Implementations of programming languages and type systems studied in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/).

Each subdirectory implements one of the languages studied in the book. Each such implementation consists of a lexer, parser, interpreter, and type system for the language implemented.

### Directory Structure

Each implementation contains 3 files:
- **interpreter.hpp**: This is the main part containing the actual implementation of the language.
- **interpreter.cpp**: This file contains a simple main() method to invoke the interpreter. For now, it only accepts a single command line argument consisting of the program to be evaluated.
- **test.cpp**: Contains tests for the separate components of an interpreter: lexer, parser, and interpreter.

### Status

Language | Directory | Status
--- | --- | ---
Untyped Arithmetic Expressions | [ch04_arith](ch04_arith) | :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Interpreter + Tests
The Untyped Lmabda Calculus | [ch07_untyped](ch07_untyped) | :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Interpreter + Tests
Typed Arithmetic Expressions | [ch08_tyarith](ch08_tyarith) | :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Interpreter & Type Checker + Tests
Simply Typed Lambda Calculus | [ch10_simplebool](ch10_simplebool) | :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Type Checker + Tests <br> :heavy_check_mark: Interpreter + Tests
Typed Lambda Calculus (with various extensions) | [ch11_fullsimple](ch11_fullsimple) | __Natural numbers (Nat) type support__ <br> :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Type Checker + Tests <br> __Records and Projections__ <br> :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Type Checker + Tests |
Typed Lambda Calculus with Subtyping | [ch17_rcdjoinsub](ch17_rcdjoinsub) | :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Interpreter + Tests |
Typed Lambda Calculus with Imperative Objects | [ch18_fullref](ch18_fullref) | __Let bindings support__ <br> :heavy_check_mark: Lexer + Tests <br> :heavy_check_mark: Parser + Tests <br> :heavy_check_mark: Interpreter + Tests <br> __References__ <br> :construction: |

### Usage

#### Running Tests

```bash
cd ch##_<lang>
clang++ --std=c++17 test.cpp && ./a.out
```

#### Interpreter

```bash
cd ch##_<lang>
clang++ --std=c++17 interpreter.cpp && ./a.out "input program"
```

