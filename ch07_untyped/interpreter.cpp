#include <iostream>

#include "interpreter.hpp"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr
            << "Error: expected input program as a command line argument.\n";
        return 1;
    }

    parser::Parser parser{std::istringstream{argv[1]}};
    auto program = parser.ParseProgram();

    std::cout << "   " << program << "\n";

    interpreter::Interpreter interpreter;
    interpreter.Interpret(program);

    std::cout << "=> " << program << "\n";

    return 0;
}
