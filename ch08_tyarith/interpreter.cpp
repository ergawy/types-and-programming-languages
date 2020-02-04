#include <iostream>

#include "interpreter.hpp"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr
            << "Error: expected input program as a command line argument.\n";
        return 1;
    }

    parser::Parser parser{std::istringstream{argv[1]}};
    type_checker::TypeChecker checker;
    auto program = parser.ParseProgram();
    std::cout << "   " << program << ": " << checker.TypeOf(program) << "\n";

    // This is a Curry-style interperter. It trys to evaluate terms even those
    // that are ill-typed (ref: tapl,§9.6).
    interpreter::Interpreter interpreter;
    auto res = interpreter.Interpret(program);
    std::cout << "=> " << res.first << ": " << res.second << "\n";

    return 0;
}
