#include <iostream>
#include <sstream>

#include "interpreter.hpp"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr
            << "Error: expected input program as a command line argument.\n";
        return 1;
    }

    lexer::Lexer lexer{std::istringstream{argv[1]}};

    return 0;
}
