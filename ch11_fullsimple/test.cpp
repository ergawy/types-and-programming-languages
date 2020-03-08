#include <iostream>
#include <optional>

#include "interpreter.hpp"

namespace lexer {
namespace test {
void Run();
}
}  // namespace lexer

namespace parser {
namespace test {
void Run();
}
}  // namespace parser

namespace type_checker {
namespace test {
void Run();
}
}  // namespace type_checker

namespace interpreter {
namespace test {
void Run();
}
}  // namespace interpreter

int main() {
    lexer::test::Run();
    parser::test::Run();
    type_checker::test::Run();
    interpreter::test::Run();

    return 0;
}

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
    // Valid tokens (non-variables):
    TestData{"l . (  ) : ->",
             {Token{Category::LAMBDA}, Token{Category::LAMBDA_DOT},
              Token{Category::OPEN_PAREN}, Token{Category::CLOSE_PAREN},
              Token{Category::COLON}, Token{Category::ARROW}}},

    // Valid tokens (keywords):
    TestData{"true false if else then",
             {
                 Token{Category::CONSTANT_TRUE, "true"},
                 Token{Category::CONSTANT_FALSE, "false"},
                 Token{Category::KEYWORD_IF, "if"},
                 Token{Category::KEYWORD_ELSE, "else"},
                 Token{Category::KEYWORD_THEN, "then"},
             }},

    // Valid tokens (variables):
    TestData{"x y L test _",
             {Token{Category::VARIABLE, "x"}, Token{Category::VARIABLE, "y"},
              Token{Category::VARIABLE, "L"}, Token{Category::VARIABLE, "test"},
              Token{Category::VARIABLE, "_"}}},
    // Invalid single-character tokens:
    TestData{"! @ # $ % ^ & * - + = ? / < > ' \" \\ | [ ] {  }",
             {Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}}},

    TestData{
        "!@ x*",
        {Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID}}},
};  // namespace test

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

        for (; actual_token.GetCategory() != Token::Category::MARKER_END &&
               expected_token_iter != std::end(test.second);
             actual_token = lexer.NextToken(), ++expected_token_iter) {
            if (actual_token != *expected_token_iter) {
                std::cout << color::kRed << "Test failed:" << color::kReset
                          << "\n";

                std::cout << "  Input program: " << test.first << "\n";

                std::cout << color::kGreen
                          << "  Expected token: " << color::kReset
                          << *expected_token_iter << ", " << color::kRed
                          << "actual token: " << color::kReset << actual_token
                          << "\n";
                failed = true;
                break;
            }
        }

        if (!failed &&
            (actual_token.GetCategory() != Token::Category::MARKER_END ||
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

namespace {
using namespace parser;

std::unique_ptr<Term> VariableUP(std::string name, int de_bruijn_idx) {
    return std::make_unique<Term>(Term::Variable(name, de_bruijn_idx));
}

std::unique_ptr<Term> ApplicationUP(std::unique_ptr<Term> lhs,
                                    std::unique_ptr<Term> rhs) {
    return std::make_unique<Term>(
        Term::Application(std::move(lhs), std::move(rhs)));
}

Term Lambda(std::string arg_name, Type& type, Term&& body) {
    return std::move(Term::Lambda(arg_name, type).Combine(std::move(body)));
}

std::unique_ptr<Term> LambdaUP(std::string arg_name, Type& type, Term&& body) {
    return std::make_unique<Term>(Lambda(arg_name, type, std::move(body)));
}

Term If(Term&& condition, Term&& then_part, Term&& else_part) {
    auto term = Term::If();
    term.Combine(std::move(condition));
    term.MarkIfConditionAsComplete();
    term.Combine(std::move(then_part));
    term.MarkIfThenAsComplete();
    term.Combine(std::move(else_part));
    term.MarkIfElseAsComplete();

    return term;
}

}  // namespace

namespace parser {
namespace test {

using namespace utils::test;
using Category = lexer::Token::Category;

struct TestData {
    std::string input_program_;
    // The absense of an expected AST means that: for the test being specified,
    // a parse error is expected.
    std::optional<Term> expected_ast_;
};

std::vector<TestData> kData{};

void InitData() {
    kData.emplace_back(TestData{"x", Term::Variable("x", 23)});

    kData.emplace_back(TestData{
        "x y", Term::Application(VariableUP("x", 23), VariableUP("y", 24))});

    kData.emplace_back(TestData{
        "(x y)", Term::Application(VariableUP("x", 23), VariableUP("y", 24))});

    kData.emplace_back(
        TestData{"((x y))",
                 Term::Application(VariableUP("x", 23), VariableUP("y", 24))});

    kData.emplace_back(TestData{
        "x y x", Term::Application(
                     ApplicationUP(VariableUP("x", 23), VariableUP("y", 24)),
                     VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "(x y) x", Term::Application(
                       ApplicationUP(VariableUP("x", 23), VariableUP("y", 24)),
                       VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "((x y) x)", Term::Application(ApplicationUP(VariableUP("x", 23),
                                                     VariableUP("y", 24)),
                                       VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "((x y)) (z)", Term::Application(ApplicationUP(VariableUP("x", 23),
                                                       VariableUP("y", 24)),
                                         VariableUP("z", 25))});

    kData.emplace_back(TestData{
        "((x y)) z", Term::Application(ApplicationUP(VariableUP("x", 23),
                                                     VariableUP("y", 24)),
                                       VariableUP("z", 25))});

    kData.emplace_back(TestData{
        "((x y) z)", Term::Application(ApplicationUP(VariableUP("x", 23),
                                                     VariableUP("y", 24)),
                                       VariableUP("z", 25))});

    kData.emplace_back(TestData{
        "(l x:Bool. x a)",
        Lambda("x", Type::Bool(),
               Term::Application(VariableUP("x", 0), VariableUP("a", 1)))});

    kData.emplace_back(TestData{
        "(l x:Bool. x y l y:Bool. y l z:Bool. z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP(
                       "y", Type::Bool(),
                       Term::Application(VariableUP("y", 0),
                                         LambdaUP("z", Type::Bool(),
                                                  Term::Variable("z", 0))))))});

    kData.emplace_back(
        TestData{"(l x:Bool. x) (l y:Bool. y)",
                 Term::Application(
                     LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                     LambdaUP("y", Type::Bool(), Term::Variable("y", 0)))});

    kData.emplace_back(
        TestData{"(l x:Bool. x) l y:Bool. y",
                 Term::Application(
                     LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                     LambdaUP("y", Type::Bool(), Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) (l y:Bool. y) l z:Bool. z",
        Term::Application(
            ApplicationUP(LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                          LambdaUP("y", Type::Bool(), Term::Variable("y", 0))),
            LambdaUP("z", Type::Bool(), Term::Variable("z", 0)))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) l y:Bool. y l z:Bool. z",
        Term::Application(
            LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
            LambdaUP("y", Type::Bool(),
                     Term::Application(VariableUP("y", 0),
                                       LambdaUP("z", Type::Bool(),
                                                Term::Variable("z", 0)))))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) l y:Bool. y a",
        Term::Application(LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                          LambdaUP("y", Type::Bool(),
                                   Term::Application(VariableUP("y", 0),
                                                     VariableUP("a", 1))))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) l y:Bool. y x",
        Term::Application(LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                          LambdaUP("y", Type::Bool(),
                                   Term::Application(VariableUP("y", 0),
                                                     VariableUP("x", 24))))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) l y:Bool. y z",
        Term::Application(LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                          LambdaUP("y", Type::Bool(),
                                   Term::Application(VariableUP("y", 0),
                                                     VariableUP("x", 26))))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) x",
        Term::Application(LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                          VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) y",
        Term::Application(LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                          VariableUP("y", 24))});

    kData.emplace_back(
        TestData{"(x l y:Bool. y)",
                 Term::Application(
                     VariableUP("x", 23),
                     LambdaUP("y", Type::Bool(), Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "(x y)", Term::Application(VariableUP("x", 23), VariableUP("y", 24))});

    kData.emplace_back(TestData{
        "(x y) x", Term::Application(
                       ApplicationUP(VariableUP("x", 23), VariableUP("y", 24)),
                       VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "(x y) z", Term::Application(
                       ApplicationUP(VariableUP("x", 23), VariableUP("y", 24)),
                       VariableUP("z", 25))});

    kData.emplace_back(TestData{"(x)", Term::Variable("x", 23)});

    kData.emplace_back(TestData{
        "l x :Bool. (l y:Bool.((x y) x))",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("x", 1))))});

    kData.emplace_back(
        TestData{"l x:Bool. (l y:Bool. (y x))",
                 Lambda("x", Type::Bool(),
                        Lambda("y", Type::Bool(),
                               Term::Application(VariableUP("y", 0),
                                                 VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x:Bool. (x y)",
        Lambda("x", Type::Bool(),
               Term::Application(VariableUP("x", 0), VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x)", Lambda("x", Type::Bool(), Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:Bool. ((x y) (l z:Bool. z))",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP("z", Type::Bool(), Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. ((x y) (z))",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. ((x y) z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x (y z))",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   ApplicationUP(VariableUP("y", 25), VariableUP("z", 26))))});

    kData.emplace_back(TestData{
        "l x:Bool. (x) (y) (z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x l y:Bool. y) z",
        Lambda(
            "x", Type::Bool(),
            Term::Application(ApplicationUP(VariableUP("x", 0),
                                            LambdaUP("y", Type::Bool(),
                                                     Term::Variable("y", 0))),
                              VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x y l z:Bool. z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP("z", Type::Bool(), Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. (x y z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x y) (z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x y) l z:Bool. z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP("z", Type::Bool(), Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. (x y) z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x) l y:Bool. y",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Type::Bool(), Term::Variable("y", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. (x) y",
        Lambda("x", Type::Bool(),
               Term::Application(VariableUP("x", 0), VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x) y (z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. (x) y z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. l y:Bool. (x y) x",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("x", 1))))});

    kData.emplace_back(
        TestData{"l x:Bool. l y:Bool. x y",
                 Lambda("x", Type::Bool(),
                        Lambda("y", Type::Bool(),
                               Term::Application(VariableUP("x", 1),
                                                 VariableUP("y", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. l y:Bool. x y a",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("a", 2))))});

    kData.emplace_back(TestData{
        "l x:Bool. l y:Bool. x y x",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x:Bool. l y:Bool. x y x y",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(ApplicationUP(VariableUP("x", 1),
                                                      VariableUP("y", 0)),
                                        VariableUP("x", 1)),
                          VariableUP("y", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. l y:Bool. x y y",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("y", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. l y:Bool. x y z",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("z", 27))))});

    kData.emplace_back(
        TestData{"l x:Bool. l y:Bool. y",
                 Lambda("x", Type::Bool(),
                        Lambda("y", Type::Bool(), Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "l x:Bool. x y z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. x (l y:Bool. y)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Type::Bool(), Term::Variable("y", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. x (l y:Bool. y) l z:Bool. z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(
                       VariableUP("x", 0),
                       LambdaUP("y", Type::Bool(), Term::Variable("y", 0))),
                   LambdaUP("z", Type::Bool(), Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. x (l y:Bool. y) l z:Bool. (z w)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(
                       VariableUP("x", 0),
                       LambdaUP("y", Type::Bool(), Term::Variable("y", 0))),
                   LambdaUP("z", Type::Bool(),
                            Term::Application(VariableUP("z", 0),
                                              VariableUP("w", 24)))))});

    kData.emplace_back(TestData{
        "l x:Bool. x (l y:Bool. y) z",
        Lambda(
            "x", Type::Bool(),
            Term::Application(ApplicationUP(VariableUP("x", 0),
                                            LambdaUP("y", Type::Bool(),
                                                     Term::Variable("y", 0))),
                              VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. x (y l z:Bool. z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   ApplicationUP(
                       VariableUP("y", 25),
                       LambdaUP("z", Type::Bool(), Term::Variable("z", 0)))))});

    kData.emplace_back(TestData{
        "l x:Bool. x (y z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   ApplicationUP(VariableUP("y", 25), VariableUP("z", 26))))});

    kData.emplace_back(TestData{
        "l x:Bool. x (y) (z)",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. x (y) z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. x l y:Bool. y",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Type::Bool(), (Term::Variable("y", 0)))))});

    kData.emplace_back(
        TestData{"l x:Bool. x l y:Bool. x y",
                 Lambda("x", Type::Bool(),
                        Term::Application(
                            VariableUP("x", 0),
                            LambdaUP("y", Type::Bool(),
                                     Term::Application(VariableUP("x", 1),
                                                       VariableUP("y", 0)))))});

    kData.emplace_back(
        TestData{"l x:Bool. x l y:Bool. x a",
                 Lambda("x", Type::Bool(),
                        Term::Application(
                            VariableUP("x", 0),
                            LambdaUP("y", Type::Bool(),
                                     Term::Application(VariableUP("x", 1),
                                                       VariableUP("a", 2)))))});

    kData.emplace_back(TestData{
        "l x:Bool. x l y:Bool. y l z:Bool. z w",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Type::Bool(),
                            Term::Application(
                                VariableUP("y", 0),
                                LambdaUP("z", Type::Bool(),
                                         Term::Application(
                                             VariableUP("z", 0),
                                             VariableUP("w", 25)))))))});

    kData.emplace_back(TestData{
        "l x:Bool. x l y:Bool. y z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Type::Bool(),
                            Term::Application(VariableUP("y", 0),
                                              VariableUP("z", 27)))))});

    kData.emplace_back(TestData{
        "l x:Bool. x l y:Bool. y z w",
        Lambda(
            "x", Type::Bool(),
            Term::Application(
                VariableUP("x", 0),
                LambdaUP("y", Type::Bool(),
                         Term::Application(ApplicationUP(VariableUP("y", 0),
                                                         VariableUP("z", 27)),
                                           VariableUP("w", 24)))))});

    kData.emplace_back(TestData{
        "l x:Bool. x x y",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("x", 0)),
                   VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x:Bool. x y",
        Lambda("x", Type::Bool(),
               Term::Application(VariableUP("x", 0), VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x:Bool. x y l y:Bool. y l z:Bool. z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP(
                       "y", Type::Bool(),
                       Term::Application(VariableUP("y", 0),
                                         LambdaUP("z", Type::Bool(),
                                                  Term::Variable("z", 0))))))});

    kData.emplace_back(TestData{
        "l x:Bool. x y l y:Bool. y z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP("y", Type::Bool(),
                            Term::Application(VariableUP("y", 0),
                                              VariableUP("z", 27)))))});

    kData.emplace_back(TestData{
        "l x:Bool. x y l z:Bool. z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP("z", Type::Bool(), Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. x z l y:Bool. y",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("z", 26)),
                   LambdaUP("y", Type::Bool(), Term::Variable("y", 0))))});

    kData.emplace_back(TestData{
        "l x:Bool. x y z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x:Bool. x y z w",
        Lambda(
            "x", Type::Bool(),
            Term::Application(ApplicationUP(ApplicationUP(VariableUP("x", 0),
                                                          VariableUP("y", 25)),
                                            VariableUP("z", 26)),
                              VariableUP("w", 23)))});

    kData.emplace_back(TestData{
        "l x:Bool.(l y:Bool.((x y) x))",
        Lambda("x", Type::Bool(),
               Lambda("y", Type::Bool(),
                      Term::Application(
                          ApplicationUP(VariableUP("x", 1), VariableUP("y", 0)),
                          VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x:Bool.x", Lambda("x", Type::Bool(), Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l y:Bool. (y)", Lambda("x", Type::Bool(), Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l y:Bool. (y) x",
        Lambda("x", Type::Bool(),
               Term::Application(VariableUP("y", 0), VariableUP("x", 24)))});

    kData.emplace_back(TestData{
        "l y:Bool. x l x:Bool. y",
        Lambda("y", Type::Bool(),
               Term::Application(
                   VariableUP("x", 24),
                   LambdaUP("x", Type::Bool(), Term::Variable("y", 1))))});

    kData.emplace_back(TestData{
        "l y:Bool. x y",
        Lambda("y", Type::Bool(),
               Term::Application(VariableUP("x", 24), VariableUP("y", 0)))});

    kData.emplace_back(TestData{
        "l y:Bool. x y z",
        Lambda("y", Type::Bool(),
               Term::Application(
                   ApplicationUP(VariableUP("x", 24), VariableUP("y", 0)),
                   VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l y:Bool. x y z a",
        Lambda(
            "y", Type::Bool(),
            Term::Application(ApplicationUP(ApplicationUP(VariableUP("x", 24),
                                                          VariableUP("y", 0)),
                                            VariableUP("z", 26)),
                              VariableUP("a", 1)))});

    kData.emplace_back(TestData{"x", Term::Variable("x", 23)});

    kData.emplace_back(
        TestData{"x (l y:Bool. y)",
                 Term::Application(
                     VariableUP("x", 23),
                     LambdaUP("y", Type::Bool(), Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "x (y z)", Term::Application(VariableUP("x", 23),
                                     ApplicationUP(VariableUP("y", 24),
                                                   VariableUP("z", 25)))});

    kData.emplace_back(TestData{
        "x (y) z", Term::Application(
                       ApplicationUP(VariableUP("x", 23), VariableUP("y", 24)),
                       VariableUP("z", 25))});

    kData.emplace_back(TestData{
        "x l x:Bool. l y:Bool. x y x y",
        Term::Application(
            VariableUP("x", 23),
            LambdaUP("x", Type::Bool(),
                     Lambda("y", Type::Bool(),
                            Term::Application(
                                ApplicationUP(ApplicationUP(VariableUP("x", 1),
                                                            VariableUP("y", 0)),
                                              VariableUP("x", 1)),
                                VariableUP("y", 0)))))});

    kData.emplace_back(TestData{
        "x l y:Bool. y", Term::Application(VariableUP("x", 23),
                                           LambdaUP("y", Type::Bool(),
                                                    Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "x y", Term::Application(VariableUP("x", 23), VariableUP("y", 24))});

    kData.emplace_back(TestData{
        "x y z x",
        Term::Application(ApplicationUP(ApplicationUP(VariableUP("x", 23),
                                                      VariableUP("y", 24)),
                                        VariableUP("z", 25)),
                          VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "(l z:Bool. l x:Bool. x) (l  y:Bool. y)",
        Term::Application(
            LambdaUP("z", Type::Bool(),
                     Lambda("x", Type::Bool(), Term::Variable("x", 0))),
            LambdaUP("y", Type::Bool(), Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "(l x:Bool. x y l y:Bool. y l z:Bool. z) x",
        Term::Application(
            LambdaUP("x", Type::Bool(),
                     Term::Application(
                         ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                         LambdaUP("y", Type::Bool(),
                                  Term::Application(
                                      VariableUP("y", 0),
                                      LambdaUP("z", Type::Bool(),
                                               Term::Variable("z", 0)))))),
            VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "l x:Bool. x (l y:Bool. y) (l z:Bool. z) w",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(
                       ApplicationUP(
                           VariableUP("x", 0),
                           LambdaUP("y", Type::Bool(), Term::Variable("y", 0))),
                       LambdaUP("z", Type::Bool(), Term::Variable("z", 0))),
                   VariableUP("w", 23)))});

    kData.emplace_back(TestData{
        "l x:Bool. x (x y) l z:Bool. z",
        Lambda("x", Type::Bool(),
               Term::Application(
                   ApplicationUP(
                       VariableUP("x", 0),
                       ApplicationUP(VariableUP("x", 0), VariableUP("y", 25))),
                   LambdaUP("z", Type::Bool(), Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "(l x:Bool. x) ((l x:Bool. x) (l z:Bool. (l x:Bool. x) z))",
        Term::Application(
            LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
            ApplicationUP(
                LambdaUP("x", Type::Bool(), Term::Variable("x", 0)),
                LambdaUP("z", Type::Bool(),
                         Term::Application(LambdaUP("x", Type::Bool(),
                                                    Term::Variable("x", 0)),
                                           VariableUP("z", 0)))))});

    // Some examples from tapl,§5.2
    // true = l t:Bool. l f:Bool. t
    // fals = l t:Bool. l f:Bool. f
    // test = l b:Bool. l m:Bool. l n:Bool. b m n
    // test true v w
    kData.emplace_back(TestData{
        "(l b:Bool. l m:Bool. l n:Bool. b m n) (l t:Bool. l f:Bool. t) v w",
        Term::Application(
            ApplicationUP(
                ApplicationUP(
                    LambdaUP(
                        "b", Type::Bool(),
                        Lambda("m", Type::Bool(),
                               Lambda("n", Type::Bool(),
                                      Term::Application(
                                          ApplicationUP(VariableUP("b", 2),
                                                        VariableUP("m", 1)),
                                          VariableUP("n", 0))))),
                    LambdaUP(
                        "t", Type::Bool(),
                        Lambda("f", Type::Bool(), Term::Variable("t", 1)))),
                VariableUP("v", 21)),
            VariableUP("w", 22))});

    // Test parsing types:
    kData.emplace_back(
        TestData{"l x:Bool->Bool. x",
                 Lambda("x", Type::FunctionType(Type::Bool(), Type::Bool()),
                        Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:Bool->Bool->Bool. x",
        Lambda(
            "x",
            Type::FunctionType(Type::Bool(),
                               Type::FunctionType(Type::Bool(), Type::Bool())),
            Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool. x",
        Lambda(
            "x",
            Type::FunctionType(Type::FunctionType(Type::Bool(), Type::Bool()),
                               Type::Bool()),
            Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool->Bool. x",
        Lambda(
            "x",
            Type::FunctionType(Type::FunctionType(Type::Bool(), Type::Bool()),
                               Type::FunctionType(Type::Bool(), Type::Bool())),
            Term::Variable("x", 0))});

    kData.emplace_back(TestData{"true", Term::True()});

    kData.emplace_back(TestData{"false", Term::False()});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool->(Bool->Bool). x",
        Lambda("x",
               Type::FunctionType(
                   Type::FunctionType(Type::Bool(), Type::Bool()),
                   Type::FunctionType(
                       Type::Bool(),
                       Type::FunctionType(Type::Bool(), Type::Bool()))),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{"if true then true else false",
                                If(Term::True(), Term::True(), Term::False())});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool->Bool. y) "
                 "else (l x:Bool. false)",
                 If(If(Term::True(), Term::True(), Term::False()),
                    Lambda("y", Type::FunctionType(Type::Bool(), Type::Bool()),
                           Term::Variable("y", 0)),
                    Lambda("x", Type::Bool(), Term::False()))});

    kData.emplace_back(
        TestData{"if (l x:Bool. x) then true else false",
                 If(Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                    Term::True(), Term::False())});

    kData.emplace_back(TestData{
        "if (l x:Bool. x) then true else l x:Bool. x",
        If(Lambda("x", Type::Bool(), Term::Variable("x", 0)), Term::True(),
           Lambda("x", Type::Bool(), Term::Variable("x", 0)))});

    kData.emplace_back(
        TestData{"if (l x:Bool. x) then (l x:Bool .x) else l x:Bool. x",
                 If(Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                    Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                    Lambda("x", Type::Bool(), Term::Variable("x", 0)))});

    kData.emplace_back(
        TestData{"l x:Bool. if true then true else false",
                 Lambda("x", Type::Bool(),
                        If(Term::True(), Term::True(), Term::False()))});

    kData.emplace_back(
        TestData{"if l x:Bool. x then true else false",
                 If(Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                    Term::True(), Term::False())});

    kData.emplace_back(TestData{
        "((l x:Bool. x))", Lambda("x", Type::Bool(), Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "if true then l x:Bool. x else false",
        If(Term::True(), Lambda("x", Type::Bool(), Term::Variable("x", 0)),
           Term::False())});

    kData.emplace_back(
        TestData{"if true then false else l x:Bool. x",
                 If(Term::True(), Term::False(),
                    Lambda("x", Type::Bool(), Term::Variable("x", 0)))});

    // Invalid programs:
    kData.emplace_back(TestData{"((x y)) (z"});
    kData.emplace_back(TestData{"(l x. x l y:Bool. y a"});
    kData.emplace_back(TestData{"(x y) x)"});
    kData.emplace_back(TestData{"l . y"});
    kData.emplace_back(TestData{"l x :Bool. (x))"});
    kData.emplace_back(TestData{"l x."});
    kData.emplace_back(TestData{"l x. ((x (y z))"});
    kData.emplace_back(TestData{"l x. x (l y:Bool. y l z:Bool. z"});
    kData.emplace_back(TestData{"l x. x (l y:Bool. y) (l z:Bool. z) w)"});
    kData.emplace_back(TestData{"l x. x'"});
    kData.emplace_back(TestData{"l x. x) (l y:Bool. y)"});
    kData.emplace_back(TestData{"l x. xa"});
    kData.emplace_back(TestData{"l x.l y:Bool. y x'"});
    kData.emplace_back(TestData{"l x:Bool->. x"});
    kData.emplace_back(TestData{"l x:Int->. x"});
    kData.emplace_back(TestData{"if true"});
    kData.emplace_back(TestData{"if true then true"});
    kData.emplace_back(TestData{"if true then true else"});
}

void Run() {
    InitData();
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

namespace type_checker {
namespace test {

using namespace utils::test;

struct TestData {
    std::string input_program_;
    Type& expected_type_;
};

std::vector<TestData> kData{};

void InitData() {
    kData.emplace_back(TestData{"x", Type::IllTyped()});

    kData.emplace_back(TestData{"x y", Type::IllTyped()});

    kData.emplace_back(TestData{
        "(l x:Bool. x)", Type::FunctionType(Type::Bool(), Type::Bool())});

    kData.emplace_back(TestData{
        "(l x:Bool. x x)", Type::FunctionType(Type::Bool(), Type::IllTyped())});

    kData.emplace_back(TestData{
        "(l x:Bool. x a)", Type::FunctionType(Type::Bool(), Type::IllTyped())});

    kData.emplace_back(
        TestData{"(l x:Bool. x y l y:Bool. y l z:Bool. z)",
                 Type::FunctionType(Type::Bool(), Type::IllTyped())});

    kData.emplace_back(TestData{
        "(l x:Bool. l y:Bool. y)",
        Type::FunctionType(Type::Bool(),
                           Type::FunctionType(Type::Bool(), Type::Bool()))});

    kData.emplace_back(
        TestData{"(l x:Bool. x) (l y:Bool. y)", Type::IllTyped()});

    kData.emplace_back(TestData{"(l x:Bool. x) true", Type::Bool()});

    kData.emplace_back(
        TestData{"(l x:Bool->Bool. x) (l y:Bool. y)",
                 Type::FunctionType(Type::Bool(), Type::Bool())});

    kData.emplace_back(TestData{"(l x:Bool. x) x", Type::IllTyped()});

    kData.emplace_back(TestData{
        "l x :Bool. (l y:Bool.((x y) x))",
        Type::FunctionType(
            Type::Bool(), Type::FunctionType(Type::Bool(), Type::IllTyped()))});

    kData.emplace_back(
        TestData{"l x:Bool. (l y:Bool. y) x",
                 Type::FunctionType(Type::Bool(), Type::Bool())});

    kData.emplace_back(TestData{
        "l x:Bool->Bool. l y:Bool. x y",
        Type::FunctionType(Type::FunctionType(Type::Bool(), Type::Bool()),
                           Type::FunctionType(Type::Bool(), Type::Bool()))});

    kData.emplace_back(
        TestData{"(l z:Bool. l x:Bool. x) (l  y:Bool. y)", Type::IllTyped()});

    kData.emplace_back(TestData{"true", Type::Bool()});

    kData.emplace_back(TestData{"false", Type::Bool()});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool->(Bool->Bool). x",
        Type::FunctionType(
            Type::FunctionType(
                Type::FunctionType(Type::Bool(), Type::Bool()),
                Type::FunctionType(
                    Type::Bool(),
                    Type::FunctionType(Type::Bool(), Type::Bool()))),
            Type::FunctionType(
                Type::FunctionType(Type::Bool(), Type::Bool()),
                Type::FunctionType(
                    Type::Bool(),
                    Type::FunctionType(Type::Bool(), Type::Bool()))))});

    kData.emplace_back(TestData{"if true then true else false", Type::Bool()});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool->Bool. y) "
                 "else (l x:Bool. false)",
                 Type::IllTyped()});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool. y) "
                 "else (l x:Bool. x)",
                 Type::FunctionType(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool. y) "
                 "else (l x:Bool. false)",
                 Type::FunctionType(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"if (l x:Bool. x) then true else false", Type::IllTyped()});

    kData.emplace_back(
        TestData{"l x:Bool. if true then true else false",
                 Type::FunctionType(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"if true then (l x:Bool. x) true else false", Type::Bool()});
}

void Run() {
    InitData();
    std::cout << color::kYellow << "[Type Checker] Running " << kData.size()
              << " tests...\n"
              << color::kReset;
    int num_failed = 0;

    for (const auto& test : kData) {
        try {
            Parser parser{std::istringstream{test.input_program_}};
            Term program = parser.ParseProgram();
            TypeChecker type_checker;
            Type& res = type_checker.TypeOf(program);

            if (test.expected_type_ != res) {
                std::cout << color::kRed << "Test failed:" << color::kReset
                          << "\n";

                std::cout << "  Input program: " << test.input_program_ << "\n";

                std::cout << color::kGreen
                          << "  Expected type: " << color::kReset << "\n"
                          << "    " << test.expected_type_ << "\n";

                std::cout << color::kRed << "  Actual type: " << color::kReset
                          << "\n    " << res << "\n";

                ++num_failed;
            }
        } catch (std::exception& ex) {
            std::cout << color::kRed << "Test failed:" << color::kReset << "\n";

            std::cout << "  Input program: " << test.input_program_ << "\n";

            std::cout << color::kGreen << "  Expected type: " << color::kReset
                      << "\n    " << test.expected_type_ << "\n";

            std::cout << color::kRed << "  Parsing failed." << color::kReset
                      << "\n";

            ++num_failed;

            continue;
        }
    }

    std::cout << color::kYellow << "Results: " << color::kReset
              << (kData.size() - num_failed) << " out of " << kData.size()
              << " tests passed.\n";
}

}  // namespace test
}  // namespace type_checker

namespace interpreter {
namespace test {

using namespace utils::test;
using namespace type_checker;

struct TestData {
    std::string input_program_;
    std::pair<std::string, Type&> expected_eval_result_;
};

std::vector<TestData> kData{};

void InitData() {
    kData.emplace_back(TestData{"true", {"true", Type::Bool()}});
    kData.emplace_back(TestData{"false", {"false", Type::Bool()}});
    kData.emplace_back(
        TestData{"if false then true else false", {"false", Type::Bool()}});
    kData.emplace_back(
        TestData{"if true then false else true", {"false", Type::Bool()}});
    kData.emplace_back(
        TestData{"if if true then false else true then true else false",
                 {"false", Type::Bool()}});

    kData.emplace_back(TestData{"(l x:Bool. x) x", {"x", Type::IllTyped()}});

    kData.emplace_back(TestData{"(l x:Bool. x) true", {"true", Type::Bool()}});

    kData.emplace_back(TestData{"(l x:Bool. x) if false then true else false",
                                {"false", Type::Bool()}});

    kData.emplace_back(TestData{
        "(l x:Bool. x) if false then true else l x:Bool. x",
        {"{l x : Bool. x}", Type::FunctionType(Type::Bool(), Type::Bool())}});

    kData.emplace_back(TestData{"(l x:Bool. if x then true else false) true",
                                {"true", Type::Bool()}});

    kData.emplace_back(TestData{"(l x:Bool. if x then true else false) false",
                                {"false", Type::Bool()}});

    kData.emplace_back(TestData{
        "(l x:Bool. if x then l x:Bool. x else l y:Bool->Bool. true) false",
        {"{l y : (Bool -> Bool). true}",
         Type::FunctionType(Type::FunctionType(Type::Bool(), Type::Bool()),
                            Type::Bool())}});
}

void Run() {
    InitData();
    std::cout << color::kYellow << "[Interpreter] Running " << kData.size()
              << " tests...\n"
              << color::kReset;
    int num_failed = 0;

    for (const auto& test : kData) {
        Interpreter interpreter{};

        try {
            Term program =
                parser::Parser{std::istringstream{test.input_program_}}
                    .ParseProgram();
            auto actual_eval_res = interpreter.Interpret(program);

            if (actual_eval_res.first != test.expected_eval_result_.first ||
                actual_eval_res.second != test.expected_eval_result_.second) {
                std::cout << color::kRed << "Test failed:" << color::kReset
                          << "\n";

                std::cout << "  Input program: " << test.input_program_ << "\n";

                std::cout << color::kGreen
                          << "  Expected evaluation result: " << color::kReset
                          << test.expected_eval_result_.first << ": "
                          << test.expected_eval_result_.second << "\n";

                std::cout << color::kRed
                          << "  Actual evaluation result: " << color::kReset
                          << actual_eval_res.first << ": "
                          << actual_eval_res.second << "\n";

                ++num_failed;
            }

        } catch (std::exception& ex) {
            std::cout << color::kRed << "Test failed:" << color::kReset << "\n";

            std::cout << "  Input program: " << test.input_program_ << "\n";

            std::cout << color::kGreen
                      << "  Expected evaluation result: " << color::kReset
                      << test.expected_eval_result_.first << ": "
                      << test.expected_eval_result_.second << "\n";

            std::cout << color::kRed << "  Parsing failed." << color::kReset
                      << "\n";

            ++num_failed;
            continue;
        }
    }

    std::cout << color::kYellow << "Results: " << color::kReset
              << (kData.size() - num_failed) << " out of " << kData.size()
              << " tests passed.\n";
}
}  // namespace test
}  // namespace interpreter

