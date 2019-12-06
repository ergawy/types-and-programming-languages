#include "interpreter.hpp"

namespace Test {
using namespace lexer;
using Category = Token::Category;
using TestData = std::pair<std::string, std::vector<Token>>;

std::vector<TestData> kLexerTests = {
    // All valid tokens.
    TestData{"true false if else then 0 succ pred iszero",
             {Token{Category::CONSTANT_TRUE, "true"},
              Token{Category::CONSTANT_FALSE, "false"},
              Token{Category::KEYWORD_IF, "if"},
              Token{Category::KEYWORD_ELSE, "else"},
              Token{Category::KEYWORD_THEN, "then"},
              Token{Category::CONSTANT_ZERO, "0"},
              Token{Category::KEYWORD_SUCC, "succ"},
              Token{Category::KEYWORD_PRED, "pred"},
              Token{Category::KEYWORD_ISZERO, "iszero"}}},
    // Invalid tokens.
    TestData{"x", {Token{Category::MARKER_ERROR, "x"}}},
    TestData{"1", {Token{Category::MARKER_ERROR, "1"}}},
};

void TestLexer() {
    std::cout << "[Lexer] Running " << kLexerTests.size() << " tests...\n";
    int num_failed = 0;

    for (const auto& test : kLexerTests) {
        Lexer lexer{std::istringstream{test.first}};

        bool failed = false;
        auto actual_token = lexer.NextToken();
        auto expected_token_iter = std::begin(test.second);

        for (; actual_token.category != Token::Category::MARKER_END &&
               expected_token_iter != std::end(test.second);
             actual_token = lexer.NextToken(), ++expected_token_iter) {
            if (actual_token != *expected_token_iter) {
                std::cout << "Test failed:\n\tInput program: " << test.first
                          << "\n\tExpected token: "
                          << expected_token_iter->DebugString()
                          << ", actual token: " << actual_token.DebugString()
                          << "\n";
                failed = true;
                break;
            }
        }

        if (!failed && (actual_token.category != Token::Category::MARKER_END ||
                        expected_token_iter != std::end(test.second))) {
            std::cout << "Test failed:\n\tInput program: " << test.first
                      << "\n\tUnexpected number of tokens.\n";
            failed = true;
        }

        if (failed) {
            ++num_failed;
        }
    }

    std::cout << (kLexerTests.size() - num_failed) << " out of "
              << kLexerTests.size() << " tests passed.\n\n";
}
}  // namespace Test

int main() {
    Test::TestLexer();

    return 0;
}
