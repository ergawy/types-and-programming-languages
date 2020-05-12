#include "interpreter.hpp"

#include <array>
#include <iostream>
#include <memory>
#include <sstream>

void EvaluateProgram(char* input) {
    parser::Parser parser{std::istringstream{input}};
    type_checker::TypeChecker checker;
    auto program = parser.ParseStatement();
    std::cout << "   " << program << ": "
              << checker.TypeOf(runtime::NamedStatementStore(), program)
              << "\n";

    interpreter::Interpreter interpreter;
    auto res = interpreter.Interpret(program);
    std::cout << "=> " << res.first << ": " << res.second << "\n";
}

int main(int argc, char* argv[]) {
    if (argc == 1) {
        constexpr int line_size = 120;
        char line[line_size];
        interpreter::Interpreter interpreter;

        while (true) {
            std::cout << ">> ";
            std::cin.getline(&line[0], line_size);
            parser::Parser statement_parser(std::istringstream{line});
            auto statement_ast = statement_parser.ParseStatement();
            auto res = interpreter.Interpret(statement_ast);
            std::cout << "=> " << res.first << ": " << res.second << "\n";
        }
    } else if (argc == 2) {
        EvaluateProgram(argv[1]);
    } else {
        // Print usage.
    }

    return 0;
}
