#include <optional>

#include "interpreter.hpp"

namespace utils {
namespace test {
namespace color {

std::string kRed{"\033[1;31m"};
std::string kGreen{"\033[1;32m"};
std::string kYellow{"\033[1;33m"};
std::string kReset{"\033[0m"};

}  // namespace color
}  // namespace test
}  // namespace utils

namespace lexer {
namespace test {

using Category = Token::Category;
using TestData = std::pair<std::string, std::vector<Token>>;
using namespace utils::test;

std::vector<TestData> kData = {
    // All valid tokens:
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
    // Invalid tokens:
    TestData{"x", {Token{Category::MARKER_ERROR, "x"}}},
    TestData{"1", {Token{Category::MARKER_ERROR, "1"}}},
};

void Run() {
    std::cout << color::kYellow << "[Lexer] Running " << kData.size()
              << " tests...\n"
              << color::kReset;
    int num_failed = 0;

    for (const auto& test : kData) {
        Lexer lexer{std::istringstream{test.first}};

        bool failed = false;
        auto actual_token = lexer.NextToken();
        auto expected_token_iter = std::begin(test.second);

        for (; actual_token.category != Token::Category::MARKER_END &&
               expected_token_iter != std::end(test.second);
             actual_token = lexer.NextToken(), ++expected_token_iter) {
            if (actual_token != *expected_token_iter) {
                std::cout << color::kRed << "Test failed:" << color::kReset
                          << "\n";

                std::cout << "  Input program: " << test.first << "\n";

                std::cout << color::kGreen
                          << "  Expected token: " << color::kReset
                          << expected_token_iter->DebugString() << ", "
                          << color::kRed << "actual token: " << color::kReset
                          << actual_token.DebugString() << "\n";
                failed = true;
                break;
            }
        }

        if (!failed && (actual_token.category != Token::Category::MARKER_END ||
                        expected_token_iter != std::end(test.second))) {
            std::cout << "Test failed:\n  Input program: " << test.first
                      << "\n  Unexpected number of tokens.\n";
            failed = true;
        }

        if (failed) {
            ++num_failed;
        }
    }

    std::cout << color::kYellow << "Results: " << color::kReset
              << (kData.size() - num_failed) << " out of " << kData.size()
              << " tests passed.\n";
}
}  // namespace test
}  // namespace lexer

namespace parser {
namespace test {

using namespace utils::test;
using Category = lexer::Token::Category;

struct TestData {
    std::string input_program_;
    // The absense of an expected AST means that for the test being specified, a
    // parse error is expected.
    std::optional<Term> expected_ast_;
};

std::vector<TestData> kData{
    TestData{"0", Term(Category::CONSTANT_ZERO)},
    TestData{"true", Term(Category::CONSTANT_TRUE)},
    TestData{"false", Term(Category::CONSTANT_FALSE)},
    TestData{"if false then true else 0",
             Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_FALSE),
                                         Term(Category::CONSTANT_TRUE),
                                         Term(Category::CONSTANT_ZERO)})},
    TestData{"if false then true else false",
             Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_FALSE),
                                         Term(Category::CONSTANT_TRUE),
                                         Term(Category::CONSTANT_FALSE)})},
    TestData{
        "if false then true else succ 0",
        Term(Category::KEYWORD_IF,
             {Term(Category::CONSTANT_FALSE), Term(Category::CONSTANT_TRUE),
              Term(Category::KEYWORD_SUCC, {Term(Category::CONSTANT_ZERO)})})},
    TestData{
        "if false then true else succ succ 0",
        Term(Category::KEYWORD_IF,
             {Term(Category::CONSTANT_FALSE), Term(Category::CONSTANT_TRUE),
              Term(Category::KEYWORD_SUCC,
                   {Term(Category::KEYWORD_SUCC,
                         {Term(Category::CONSTANT_ZERO)})})})},
    TestData{
        "if false then true else succ succ succ 0",
        Term(Category::KEYWORD_IF,
             {Term(Category::CONSTANT_FALSE), Term(Category::CONSTANT_TRUE),
              Term(Category::KEYWORD_SUCC,
                   {Term(Category::KEYWORD_SUCC,
                         {Term(Category::KEYWORD_SUCC,
                               {Term(Category::CONSTANT_ZERO)})})})})},
    TestData{
        "if succ 0 then succ 0 else true",
        Term(Category::KEYWORD_IF,
             {Term(Category::KEYWORD_SUCC, {Term(Category::CONSTANT_ZERO)}),
              Term(Category::KEYWORD_SUCC, {Term(Category::CONSTANT_ZERO)}),
              Term(Category::CONSTANT_TRUE)})},
    TestData{"if true then false else true",
             Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_TRUE),
                                         Term(Category::CONSTANT_FALSE),
                                         Term(Category::CONSTANT_TRUE)})},
    TestData{"if true then succ 0 else 0",
             Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_TRUE),
                                         Term(Category::KEYWORD_SUCC,
                                              {Term(Category::CONSTANT_ZERO)}),
                                         Term(Category::CONSTANT_ZERO)})},
    TestData{"if true then succ 0 else true",
             Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_TRUE),
                                         Term(Category::KEYWORD_SUCC,
                                              {Term(Category::CONSTANT_ZERO)}),
                                         Term(Category::CONSTANT_TRUE)})},
    TestData{
        "if true then true else succ 0",
        Term(Category::KEYWORD_IF,
             {Term(Category::CONSTANT_TRUE), Term(Category::CONSTANT_TRUE),
              Term(Category::KEYWORD_SUCC, {Term(Category::CONSTANT_ZERO)})})},

    TestData{
        "if if true then false else true then true else false",
        Term(Category::KEYWORD_IF,
             {Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_TRUE),
                                          Term(Category::CONSTANT_FALSE),
                                          Term(Category::CONSTANT_TRUE)}),
              Term(Category::CONSTANT_TRUE), Term(Category::CONSTANT_FALSE)})},
    TestData{"iszero 0",
             Term(Category::KEYWORD_ISZERO, {Term(Category::CONSTANT_ZERO)})},
    TestData{"iszero pred succ succ 0",
             Term(Category::KEYWORD_ISZERO,
                  {Term(Category::KEYWORD_PRED,
                        {Term(Category::KEYWORD_SUCC,
                              {Term(Category::KEYWORD_SUCC,
                                    {Term(Category::CONSTANT_ZERO)})})})})},
    TestData{"pred pred 0", Term(Category::KEYWORD_PRED,
                                 {Term(Category::KEYWORD_PRED,
                                       {Term(Category::CONSTANT_ZERO)})})},
    TestData{"pred pred if true then true else false",
             Term(Category::KEYWORD_PRED,
                  {Term(Category::KEYWORD_PRED,
                        {Term(Category::KEYWORD_IF,
                              {Term(Category::CONSTANT_TRUE),
                               Term(Category::CONSTANT_TRUE),
                               Term(Category::CONSTANT_FALSE)})})})},
    TestData{"pred succ 0", Term(Category::KEYWORD_PRED,
                                 {Term(Category::KEYWORD_SUCC,
                                       {Term(Category::CONSTANT_ZERO)})})},
    TestData{"pred succ if true then true else false",
             Term(Category::KEYWORD_PRED,
                  {Term(Category::KEYWORD_SUCC,
                        {Term(Category::KEYWORD_IF,
                              {Term(Category::CONSTANT_TRUE),
                               Term(Category::CONSTANT_TRUE),
                               Term(Category::CONSTANT_FALSE)})})})},
    TestData{"pred succ pred 0",
             Term(Category::KEYWORD_PRED,
                  {Term(Category::KEYWORD_SUCC,
                        {Term(Category::KEYWORD_PRED,
                              {Term(Category::CONSTANT_ZERO)})})})},
    TestData{"pred succ pred succ 0",
             Term(Category::KEYWORD_PRED,
                  {Term(Category::KEYWORD_SUCC,
                        {Term(Category::KEYWORD_PRED,
                              {Term(Category::KEYWORD_SUCC,
                                    {Term(Category::CONSTANT_ZERO)})})})})},
    TestData{
        "succ if true then true else false",
        Term(Category::KEYWORD_SUCC,
             {Term(Category::KEYWORD_IF, {Term(Category::CONSTANT_TRUE),
                                          Term(Category::CONSTANT_TRUE),
                                          Term(Category::CONSTANT_FALSE)})})},
    TestData{"succ succ true", Term(Category::KEYWORD_SUCC,
                                    {Term(Category::KEYWORD_SUCC,
                                          {Term(Category::CONSTANT_TRUE)})})},

    // Expected parse errors.
    TestData{"succ"},
    TestData{"if  else false"},
    TestData{"if if true then false else true then true else"},
    TestData{"if if true then false else true then true else false if"},
    TestData{"if then else succ pred"},
    TestData{"if then else succ pred 0 true false"},
    TestData{"if then else succ pred 0 true false test"},
    TestData{"if true"},
    TestData{"if true else false"},
    TestData{"if true then succ 1 else true"},
    TestData{"pred"},
    TestData{"pred pred"},
    TestData{"pred succ"},
    TestData{"pred succ 1"},
    TestData{"pred succ if true then true false"},
    TestData{"succ"},
    TestData{"succ 1"},
    TestData{"succ pred 0 pred"},
    TestData{"succ pred 0 pred 0"},
    TestData{"succ pred 0 presd"},
    TestData{"succ succ 1"},
};

void Run() {
    std::cout << color::kYellow << "[Parser] Running " << kData.size()
              << " tests...\n"
              << color::kReset;
    int num_failed = 0;

    for (const auto& test : kData) {
        Parser parser{std::istringstream{test.input_program_}};
        Term res;

        try {
            res = parser.ParseProgram();
            if (*test.expected_ast_ != res) {
                std::cout << color::kRed << "Test failed:" << color::kReset
                          << "\n";

                std::cout << "  Input program: " << test.input_program_ << "\n";

                std::cout << color::kGreen
                          << "  Expected AST: " << color::kReset << "\n"
                          << test.expected_ast_->ASTString(4) << "\n";

                std::cout << color::kRed << "  Actual AST: " << color::kReset
                          << "\n"
                          << res.ASTString(4) << "\n";

                ++num_failed;
            }

            continue;
        } catch (std::exception& ex) {
            if (test.expected_ast_) {
                // Unexpected parse error.
                std::cout << color::kRed << "Test failed:" << color::kReset
                          << "\n";

                std::cout << "  Input program: " << test.input_program_ << "\n";

                std::cout << color::kGreen
                          << "  Expected AST: " << color::kReset << "\n"
                          << test.expected_ast_->ASTString(4) << "\n";

                std::cout << color::kRed << "  Parsing failed." << color::kReset
                          << "\n";

                ++num_failed;
            }

            continue;
        }

        // Unexpected parse success.
        if (!test.expected_ast_) {
            std::cout << color::kRed << "Test failed:" << color::kReset << "\n";

            std::cout << "  Input program: " << test.input_program_ << "\n";

            std::cout << color::kGreen << "  Expected parsing error"
                      << color::kReset << "\n";

            std::cout << color::kRed << "  Parsed AST: " << color::kReset
                      << "\n"
                      << res.ASTString(4) << "\n";

            ++num_failed;
        }
    }

    std::cout << color::kYellow << "Results: " << color::kReset
              << (kData.size() - num_failed) << " out of " << kData.size()
              << " tests passed.\n";
}

}  // namespace test
}  // namespace parser

int main() {
    lexer::test::Run();
    parser::test::Run();

    return 0;
}
