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

    interpreter::Interpreter interpreter;
    auto res = interpreter.Interpret(program);
    std::cout << "=> " << res.first << ": " << res.second << "\n";

    return 0;
}

