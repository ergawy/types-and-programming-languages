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

int main() {
    lexer::test::Run();
    parser::test::Run();

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
    TestData{"l . (  )",
             {Token{Category::LAMBDA}, Token{Category::LAMBDA_DOT},
              Token{Category::OPEN_PAREN}, Token{Category::CLOSE_PAREN}}},
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

std::unique_ptr<Term> VariableUP(std::string name, int de_bruijn_idx) {
    return std::make_unique<Term>(Term::Variable(name, de_bruijn_idx));
}

std::unique_ptr<Term> ApplicationUP(std::unique_ptr<Term> lhs,
                                    std::unique_ptr<Term> rhs) {
    return std::make_unique<Term>(
        Term::Application(std::move(lhs), std::move(rhs)));
}

Term Lambda(std::string arg_name, Term&& body) {
    return std::move(Term::Lambda(arg_name).Combine(std::move(body)));
}

std::unique_ptr<Term> LambdaUP(std::string arg_name, Term&& body) {
    return std::make_unique<Term>(Lambda(arg_name, std::move(body)));
}

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

    // DISABLE:
    // kData.emplace_back(TestData{
    //    "(l z. l x. x) (l  y. y)",
    //    Term::Application(LambdaUP("z", Lambda("x", Term::Variable("x", 0))),
    //                      LambdaUP("x", Term::Variable("x", 0)))});

    kData.emplace_back(TestData{
        "(l x. x a)", Lambda("x", Term::Application(VariableUP("x", 0),
                                                    VariableUP("a", 1)))});
    kData.emplace_back(TestData{
        "(l x. x y l y. y l z. z)",
        Lambda(
            "x",
            Term::Application(
                ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                LambdaUP("y", Term::Application(
                                  VariableUP("y", 0),
                                  LambdaUP("z", Term::Variable("z", 0))))))});

    // DISABLE:
    // kData.emplace_back(TestData{
    //    "(l x. x y l y. y l z. z) x",
    //    Term::Application(
    //        LambdaUP(
    //            "x",
    //            Term::Application(
    //                ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
    //                LambdaUP("y", Term::Application(
    //                                  VariableUP("y", 0),
    //                                  LambdaUP("z", Term::Variable("z",
    //                                  0)))))),
    //        VariableUP("x", 1))});

    kData.emplace_back(
        TestData{"(l x. x) (l y. y)",
                 Term::Application(LambdaUP("x", Term::Variable("x", 0)),
                                   LambdaUP("y", Term::Variable("y", 0)))});

    kData.emplace_back(
        TestData{"(l x. x) l y. y",
                 Term::Application(LambdaUP("x", Term::Variable("x", 0)),
                                   LambdaUP("y", Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "(l x. x) (l y. y) l z. z",
        Term::Application(ApplicationUP(LambdaUP("x", Term::Variable("x", 0)),
                                        LambdaUP("y", Term::Variable("y", 0))),
                          LambdaUP("z", Term::Variable("z", 0)))});

    kData.emplace_back(TestData{
        "(l x. x) l y. y l z. z",
        Term::Application(
            LambdaUP("x", Term::Variable("x", 0)),
            LambdaUP("y", Term::Application(
                              VariableUP("y", 0),
                              LambdaUP("z", Term::Variable("z", 0)))))});

    kData.emplace_back(
        TestData{"(l x. x) l y. y a",
                 Term::Application(
                     LambdaUP("x", Term::Variable("x", 0)),
                     LambdaUP("y", Term::Application(VariableUP("y", 0),
                                                     VariableUP("a", 1))))});
    kData.emplace_back(
        TestData{"(l x. x) l y. y x",
                 Term::Application(
                     LambdaUP("x", Term::Variable("x", 0)),
                     LambdaUP("y", Term::Application(VariableUP("y", 0),
                                                     VariableUP("x", 24))))});

    kData.emplace_back(
        TestData{"(l x. x) l y. y z",
                 Term::Application(
                     LambdaUP("x", Term::Variable("x", 0)),
                     LambdaUP("y", Term::Application(VariableUP("y", 0),
                                                     VariableUP("x", 26))))});

    kData.emplace_back(TestData{
        "(l x. x) x", Term::Application(LambdaUP("x", Term::Variable("x", 0)),
                                        VariableUP("x", 23))});

    kData.emplace_back(TestData{
        "(l x. x) y", Term::Application(LambdaUP("x", Term::Variable("x", 0)),
                                        VariableUP("y", 24))});

    kData.emplace_back(
        TestData{"(x l y. y)",
                 Term::Application(VariableUP("x", 23),
                                   LambdaUP("y", Term::Variable("y", 0)))});

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
        "l x . (l y.((x y) x))",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x. (l y. (y x))",
        Lambda("x", Lambda("y", Term::Application(VariableUP("y", 0),
                                                  VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x. (x y)", Lambda("x", Term::Application(VariableUP("x", 0),
                                                    VariableUP("y", 25)))});

    kData.emplace_back(
        TestData{"l x. (x)", Lambda("x", Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x. ((x y) (l z. z))",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        LambdaUP("z", Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x. ((x y) (z))",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. ((x y) z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x (y z))",
        Lambda("x", Term::Application(VariableUP("x", 0),
                                      ApplicationUP(VariableUP("y", 25),
                                                    VariableUP("z", 26))))});

    kData.emplace_back(TestData{
        "l x. (x) (y) (z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x l y. y) z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0),
                                      LambdaUP("y", Term::Variable("y", 0))),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x y l z. z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        LambdaUP("z", Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x. (x y z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x y) (z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x y) l z. z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        LambdaUP("z", Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x. (x y) z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x) l y. y",
        Lambda("x", Term::Application(VariableUP("x", 0),
                                      LambdaUP("y", Term::Variable("y", 0))))});

    kData.emplace_back(TestData{
        "l x. (x) y", Lambda("x", Term::Application(VariableUP("x", 0),
                                                    VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x. (x) y (z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. (x) y z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. l y. (x y) x",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x. l y. x y",
        Lambda("x", Lambda("y", Term::Application(VariableUP("x", 1),
                                                  VariableUP("y", 0))))});

    kData.emplace_back(TestData{
        "l x. l y. x y a",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("a", 2))))});

    kData.emplace_back(TestData{
        "l x. l y. x y x",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("x", 1))))});

    kData.emplace_back(TestData{
        "l x. l y. x y x y",
        Lambda("x",
               Lambda("y", Term::Application(
                               ApplicationUP(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("x", 1)),
                               VariableUP("y", 0))))});

    kData.emplace_back(TestData{
        "l x. l y. x y y",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("y", 0))))});

    kData.emplace_back(TestData{
        "l x. l y. x y z",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("z", 27))))});

    kData.emplace_back(TestData{
        "l x. l y. y", Lambda("x", Lambda("y", Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "l x. x y z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. x (l y. y)",
        Lambda("x", Term::Application(VariableUP("x", 0),
                                      LambdaUP("y", Term::Variable("y", 0))))});

    // DISABLE:
    // kData.emplace_back(TestData{
    //    "l x. x (l y. y) (l z. z) w",
    //    Lambda("x",
    //           Term::Application(
    //               ApplicationUP(
    //                   ApplicationUP(VariableUP("x", 0),
    //                                 LambdaUP("y", Term::Variable("y", 0))),
    //                   LambdaUP("z", Term::Variable("z", 0))),
    //               VariableUP("w", 25)))});

    kData.emplace_back(TestData{
        "l x. x (l y. y) l z. z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0),
                                      LambdaUP("y", Term::Variable("y", 0))),
                        LambdaUP("z", Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x. x (l y. y) l z. (z w)",
        Lambda("x",
               Term::Application(
                   ApplicationUP(VariableUP("x", 0),
                                 LambdaUP("y", Term::Variable("y", 0))),
                   LambdaUP("z", Term::Application(VariableUP("z", 0),
                                                   VariableUP("w", 24)))))});
    kData.emplace_back(TestData{
        "l x. x (l y. y) z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0),
                                      LambdaUP("y", Term::Variable("y", 0))),
                        VariableUP("z", 26)))});

    // DISABLE:
    // kData.emplace_back(TestData{
    //    "l x. x (x y) l z. z",
    //    Lambda("x", Term::Application(
    //                    ApplicationUP(VariableUP("x", 0),
    //                                  ApplicationUP(VariableUP("x", 0),
    //                                                VariableUP("y", 24))),
    //                    LambdaUP("z", Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x. x (y l z. z)",
        Lambda("x",
               Term::Application(
                   VariableUP("x", 0),
                   ApplicationUP(VariableUP("y", 25),
                                 LambdaUP("z", Term::Variable("z", 0)))))});

    kData.emplace_back(TestData{
        "l x. x (y z)",
        Lambda("x", Term::Application(VariableUP("x", 0),
                                      ApplicationUP(VariableUP("y", 25),
                                                    VariableUP("z", 26))))});

    kData.emplace_back(TestData{
        "l x. x (y) (z)",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. x (y) z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. x l y. y",
        Lambda("x",
               Term::Application(VariableUP("x", 0),
                                 LambdaUP("y", (Term::Variable("y", 0)))))});

    kData.emplace_back(TestData{
        "l x. x l y. x y",
        Lambda("x",
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Term::Application(VariableUP("x", 1),
                                                   VariableUP("y", 0)))))});

    kData.emplace_back(TestData{
        "l x. x l y. x a",
        Lambda("x",
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Term::Application(VariableUP("x", 1),
                                                   VariableUP("a", 2)))))});

    kData.emplace_back(TestData{
        "l x. x l y. y l z. z w",
        Lambda(
            "x",
            Term::Application(
                VariableUP("x", 0),
                LambdaUP("y", Term::Application(
                                  VariableUP("y", 0),
                                  LambdaUP("z", Term::Application(
                                                    VariableUP("z", 0),
                                                    VariableUP("w", 25)))))))});

    kData.emplace_back(TestData{
        "l x. x l y. y z",
        Lambda("x",
               Term::Application(
                   VariableUP("x", 0),
                   LambdaUP("y", Term::Application(VariableUP("y", 0),
                                                   VariableUP("z", 27)))))});

    kData.emplace_back(TestData{
        "l x. x l y. y z w",
        Lambda("x", Term::Application(
                        VariableUP("x", 0),
                        LambdaUP("y", Term::Application(
                                          ApplicationUP(VariableUP("y", 0),
                                                        VariableUP("z", 27)),
                                          VariableUP("w", 24)))))});

    kData.emplace_back(TestData{
        "l x. x x y",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("x", 0)),
                        VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x. x y", Lambda("x", Term::Application(VariableUP("x", 0),
                                                  VariableUP("y", 25)))});

    kData.emplace_back(TestData{
        "l x. x y l y. y l z. z",
        Lambda(
            "x",
            Term::Application(
                ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                LambdaUP("y", Term::Application(
                                  VariableUP("y", 0),
                                  LambdaUP("z", Term::Variable("z", 0))))))});

    kData.emplace_back(TestData{
        "l x. x y l y. y z",
        Lambda("x",
               Term::Application(
                   ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                   LambdaUP("y", Term::Application(VariableUP("y", 0),
                                                   VariableUP("z", 27)))))});

    kData.emplace_back(TestData{
        "l x. x y l z. z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        LambdaUP("z", Term::Variable("z", 0))))});

    kData.emplace_back(TestData{
        "l x. x z l y. y",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("z", 26)),
                        LambdaUP("y", Term::Variable("y", 0))))});

    kData.emplace_back(TestData{
        "l x. x y z",
        Lambda("x", Term::Application(
                        ApplicationUP(VariableUP("x", 0), VariableUP("y", 25)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l x. x y z w",
        Lambda("x", Term::Application(
                        ApplicationUP(ApplicationUP(VariableUP("x", 0),
                                                    VariableUP("y", 25)),
                                      VariableUP("z", 26)),
                        VariableUP("w", 23)))});

    kData.emplace_back(TestData{
        "l x.(l y.((x y) x))",
        Lambda("x",
               Lambda("y", Term::Application(ApplicationUP(VariableUP("x", 1),
                                                           VariableUP("y", 0)),
                                             VariableUP("x", 1))))});

    kData.emplace_back(TestData{"l x.x", Lambda("x", Term::Variable("x", 0))});

    kData.emplace_back(
        TestData{"l y. (y)", Lambda("x", Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l y. (y) x", Lambda("x", Term::Application(VariableUP("y", 0),
                                                    VariableUP("x", 24)))});

    kData.emplace_back(TestData{
        "l y. x l x. y",
        Lambda("y", Term::Application(VariableUP("x", 24),
                                      LambdaUP("x", Term::Variable("y", 1))))});

    kData.emplace_back(TestData{
        "l y. x y", Lambda("y", Term::Application(VariableUP("x", 24),
                                                  VariableUP("y", 0)))});

    kData.emplace_back(TestData{
        "l y. x y z",
        Lambda("y", Term::Application(
                        ApplicationUP(VariableUP("x", 24), VariableUP("y", 0)),
                        VariableUP("z", 26)))});

    kData.emplace_back(TestData{
        "l y. x y z a",
        Lambda("y", Term::Application(
                        ApplicationUP(ApplicationUP(VariableUP("x", 24),
                                                    VariableUP("y", 0)),
                                      VariableUP("z", 26)),
                        VariableUP("a", 1)))});

    kData.emplace_back(TestData{"x", Term::Variable("x", 23)});

    kData.emplace_back(
        TestData{"x (l y. y)",
                 Term::Application(VariableUP("x", 23),
                                   LambdaUP("y", Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "x (y z)", Term::Application(VariableUP("x", 23),
                                     ApplicationUP(VariableUP("y", 24),
                                                   VariableUP("z", 25)))});

    kData.emplace_back(TestData{
        "x (y) z", Term::Application(
                       ApplicationUP(VariableUP("x", 23), VariableUP("y", 24)),
                       VariableUP("z", 25))});

    kData.emplace_back(TestData{
        "x l x. l y. x y x y",
        Term::Application(
            VariableUP("x", 23),
            LambdaUP("x", Lambda("y", Term::Application(
                                          ApplicationUP(
                                              ApplicationUP(VariableUP("x", 1),
                                                            VariableUP("y", 0)),
                                              VariableUP("x", 1)),
                                          VariableUP("y", 0)))))});

    kData.emplace_back(TestData{
        "x l y. y", Term::Application(VariableUP("x", 23),
                                      LambdaUP("y", Term::Variable("y", 0)))});

    kData.emplace_back(TestData{
        "x y", Term::Application(VariableUP("x", 23), VariableUP("y", 24))});

    kData.emplace_back(TestData{
        "x y z x",
        Term::Application(ApplicationUP(ApplicationUP(VariableUP("x", 23),
                                                      VariableUP("y", 24)),
                                        VariableUP("z", 25)),
                          VariableUP("x", 23))});

    kData.emplace_back(TestData{"((x y)) (z"});
    kData.emplace_back(TestData{"(l x. x l y. y a"});
    kData.emplace_back(TestData{"(x y) x)"});
    kData.emplace_back(TestData{"l . y"});
    kData.emplace_back(TestData{"l x . (x))"});
    kData.emplace_back(TestData{"l x."});
    kData.emplace_back(TestData{"l x. ((x (y z))"});
    kData.emplace_back(TestData{"l x. x (l y. y l z. z"});
    kData.emplace_back(TestData{"l x. x (l y. y) (l z. z) w)"});
    kData.emplace_back(TestData{"l x. x'"});
    kData.emplace_back(TestData{"l x. x) (l y. y)"});
    kData.emplace_back(TestData{"l x. xa"});
    kData.emplace_back(TestData{"l x.l y. y x'"});
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
