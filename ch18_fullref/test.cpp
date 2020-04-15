#include <cstddef>
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
    TestData{"l.():->{}=:=",
             {Token{Category::LAMBDA}, Token{Category::DOT},
              Token{Category::OPEN_PAREN}, Token{Category::CLOSE_PAREN},
              Token{Category::COLON}, Token{Category::ARROW},
              Token{Category::OPEN_BRACE}, Token{Category::CLOSE_BRACE},
              Token{Category::EQUAL}, Token{Category::ASSIGN}}},

    // Valid tokens (keywords):
    TestData{"true false if else then 0 succ pred iszero Bool Nat let in ref",
             {
                 Token{Category::CONSTANT_TRUE},
                 Token{Category::CONSTANT_FALSE},
                 Token{Category::KEYWORD_IF},
                 Token{Category::KEYWORD_ELSE},
                 Token{Category::KEYWORD_THEN},

                 Token{Category::CONSTANT_ZERO},
                 Token{Category::KEYWORD_SUCC},
                 Token{Category::KEYWORD_PRED},
                 Token{Category::KEYWORD_ISZERO},

                 Token{Category::KEYWORD_BOOL},
                 Token{Category::KEYWORD_NAT},

                 Token{Category::KEYWORD_LET},
                 Token{Category::KEYWORD_IN},

                 Token{Category::KEYWORD_REF},
             }},

    // Valid tokens (variables):
    TestData{
        "x y L test _",
        {Token{Category::IDENTIFIER, "x"}, Token{Category::IDENTIFIER, "y"},
         Token{Category::IDENTIFIER, "L"}, Token{Category::IDENTIFIER, "test"},
         Token{Category::IDENTIFIER, "_"}}},
    // Invalid single-character tokens:
    TestData{"@ # $ % ^ & * - + ? / < > ' \" \\ | [ ]  ",
             {Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}, Token{Category::MARKER_INVALID},
              Token{Category::MARKER_INVALID}}},

    TestData{"x*", {Token{Category::MARKER_INVALID}}},
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
    term.Combine(std::move(then_part));
    term.Combine(std::move(else_part));

    return term;
}

Term Succ(Term&& arg) {
    auto term = Term::Succ();
    term.Combine(std::move(arg));

    return term;
}

Term Pred(Term&& arg) {
    auto term = Term::Pred();
    term.Combine(std::move(arg));

    return term;
}

Term IsZero(Term&& arg) {
    auto term = Term::IsZero();
    term.Combine(std::move(arg));

    return term;
}

Term Let(std::string binding_name, Term&& bound_term, Term&& body_term) {
    auto term = Term::Let(binding_name);
    term.Combine(std::move(bound_term));
    term.Combine(std::move(body_term));

    return term;
}

std::unique_ptr<Term> LetUP(std::string binding_name, Term&& bound_term,
                            Term&& body_term) {
    return std::make_unique<Term>(
        Let(binding_name, std::move(bound_term), std::move(body_term)));
}

Term Ref(Term&& ref_term) {
    Term term = Term::Ref();
    term.Combine(std::move(ref_term));

    return term;
}

std::unique_ptr<Term> RefUP(Term&& ref_term) {
    return std::make_unique<Term>(Ref(std::move(ref_term)));
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
                 Lambda("x", Type::Function(Type::Bool(), Type::Bool()),
                        Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:Bool->Bool->Bool. x",
        Lambda("x",
               Type::Function(Type::Bool(),
                              Type::Function(Type::Bool(), Type::Bool())),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool. x",
        Lambda("x",
               Type::Function(Type::Function(Type::Bool(), Type::Bool()),
                              Type::Bool()),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool->Bool. x",
        Lambda("x",
               Type::Function(Type::Function(Type::Bool(), Type::Bool()),
                              Type::Function(Type::Bool(), Type::Bool())),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:{a:Bool, b:Nat}. x",
        Lambda("x", Type::Record({{"a", Type::Bool()}, {"b", Type::Nat()}}),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:{a:Bool, b:{c:Nat}}. x",
        Lambda("x",
               Type::Record({{"a", Type::Bool()},
                             {"b", Type::Record({{"c", Type::Nat()}})}}),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:{a:Bool->Nat, b:{c:Nat}}. x",
        Lambda("x",
               Type::Record({{"a", Type::Function(Type::Bool(), Type::Nat())},
                             {"b", Type::Record({{"c", Type::Nat()}})}}),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{
        "l x:{a:Bool->Nat->{d:Bool}, b:{c:Nat}}. x",
        Lambda("x",
               Type::Record(
                   {{"a",
                     Type::Function(
                         Type::Bool(),
                         Type::Function(Type::Nat(),
                                        Type::Record({{"d", Type::Bool()}})))},
                    {"b", Type::Record({{"c", Type::Nat()}})}}),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{"true", Term::True()});

    kData.emplace_back(TestData{"false", Term::False()});

    kData.emplace_back(TestData{"0", Term::Zero()});

    kData.emplace_back(TestData{"succ 0", Succ(Term::Zero())});

    kData.emplace_back(TestData{"pred 0", Pred(Term::Zero())});

    kData.emplace_back(TestData{"iszero 0", IsZero(Term::Zero())});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool->(Bool->Bool). x",
        Lambda("x",
               Type::Function(
                   Type::Function(Type::Bool(), Type::Bool()),
                   Type::Function(Type::Bool(),
                                  Type::Function(Type::Bool(), Type::Bool()))),
               Term::Variable("x", 0))});

    kData.emplace_back(TestData{"if true then true else false",
                                If(Term::True(), Term::True(), Term::False())});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool->Bool. y) "
                 "else (l x:Bool. false)",
                 If(If(Term::True(), Term::True(), Term::False()),
                    Lambda("y", Type::Function(Type::Bool(), Type::Bool()),
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

    kData.emplace_back(TestData{"if false then true else 0",
                                If(Term::False(), Term::True(), Term::Zero())});

    kData.emplace_back(
        TestData{"if false then true else succ 0",
                 If(Term::False(), Term::True(), Succ(Term::Zero()))});

    kData.emplace_back(
        TestData{"if false then true else succ succ 0",
                 If(Term::False(), Term::True(), Succ(Succ(Term::Zero())))});

    kData.emplace_back(TestData{
        "if false then true else succ succ succ 0",
        If(Term::False(), Term::True(), Succ(Succ(Succ(Term::Zero()))))});

    kData.emplace_back(
        TestData{"if succ 0 then succ 0 else true",
                 If(Succ(Term::Zero()), Succ(Term::Zero()), Term::True())});

    kData.emplace_back(
        TestData{"if true then succ 0 else 0",
                 If(Term::True(), Succ(Term::Zero()), Term::Zero())});

    kData.emplace_back(TestData{"iszero pred succ succ 0",
                                IsZero(Pred(Succ(Succ(Term::Zero()))))});

    kData.emplace_back(TestData{"pred succ 0", Pred(Succ(Term::Zero()))});

    kData.emplace_back(
        TestData{"l x:Nat. pred pred x",
                 Lambda("x", Type::Nat(), Pred(Pred(Term::Variable("x", 0))))});

    kData.emplace_back(TestData{
        "(l x:Nat. pred pred x) succ succ succ 0",
        Term::Application(
            LambdaUP("x", Type::Nat(), Pred(Pred(Term::Variable("x", 0)))),
            std::make_unique<Term>(Succ(Succ(Succ(Term::Zero())))))});

    auto record = Term::Record();
    record.AddRecordLabel("x");
    record.Combine(Term::Zero());
    kData.emplace_back(TestData{"{x=0}", std::move(record)});

    auto record2 = Term::Record();
    record2.AddRecordLabel("x");
    record2.Combine(Succ(Term::Zero()));
    kData.emplace_back(TestData{"{x=succ 0}", std::move(record2)});

    auto record3 = Term::Record();
    record3.AddRecordLabel("x");
    record3.Combine(Succ(Term::Zero()));
    record3.AddRecordLabel("y");
    record3.Combine(Lambda("z", Type::Bool(), Term::Variable("x", 0)));
    kData.emplace_back(
        TestData{"{x=succ 0, y=l z:Bool. z}", std::move(record3)});

    kData.emplace_back(
        TestData{"x.y", Term::Projection(VariableUP("x", 23), "y")});

    auto record4 = Term::Record();
    record4.AddRecordLabel("x");
    record4.Combine(Succ(Term::Zero()));
    record4.AddRecordLabel("y");
    record4.Combine(Lambda("z", Type::Bool(), Term::Variable("x", 0)));
    kData.emplace_back(TestData{
        "{x=succ 0, y=l z:Bool. z}.x",
        Term::Projection(std::make_unique<Term>(std::move(record4)), "x")});

    auto record5 = Term::Record();
    record5.AddRecordLabel("x");
    record5.Combine(Term::Zero());
    kData.emplace_back(TestData{
        "(l r:{x:Nat}. r.x) {x=succ 0}",
        Term::Application(LambdaUP("r", Type::Record({{"x", Type::Nat()}}),
                                   Term::Projection(VariableUP("r", 0), "x")),
                          std::make_unique<Term>(std::move(record5)))});

    kData.emplace_back(TestData{"let x = true in succ 0",
                                Let("x", Term::True(), Succ(Term::Zero()))});

    kData.emplace_back(TestData{
        "let x = true in x", Let("x", Term::True(), Term::Variable("x", 0))});

    kData.emplace_back(
        TestData{"l x:Bool. l x:Nat. x",
                 Lambda("x", Type::Bool(),
                        Lambda("x", Type::Nat(), Term::Variable("x", 0)))});

    kData.emplace_back(
        TestData{"let x = l x:Bool. x in succ 0",
                 Let("x", Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                     Succ(Term::Zero()))});

    kData.emplace_back(TestData{
        "l y:Nat. let x = l x:Bool. x in succ 0",
        Lambda("y", Type::Nat(),
               Let("x", Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                   Succ(Term::Zero())))});

    kData.emplace_back(TestData{
        "(l y:Nat. let x = l x:Bool. x in succ 0) a",
        Term::Application(
            LambdaUP("y", Type::Nat(),
                     Let("x", Lambda("x", Type::Bool(), Term::Variable("x", 0)),
                         Succ(Term::Zero()))),
            VariableUP("a", 0))});

    kData.emplace_back(TestData{"ref x", Ref(Term::Variable("x", 23))});

    kData.emplace_back(TestData{"ref succ 0", Ref(Succ(Term::Zero()))});

    kData.emplace_back(
        TestData{"ref x y", Term::Application(RefUP(Term::Variable("x", 23)),
                                              VariableUP("y", 24))});

    kData.emplace_back(TestData{
        "ref x let y = succ 0 in iszero y",
        Term::Application(
            RefUP(Term::Variable("x", 23)),
            LetUP("y", Succ(Term::Zero()), IsZero(Term::Variable("y", 0))))});

    kData.emplace_back(TestData{
        "(let y = succ 0 in iszero y) ref x ",
        Term::Application(
            LetUP("y", Succ(Term::Zero()), IsZero(Term::Variable("y", 0))),
            RefUP(Term::Variable("x", 23)))});

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
    kData.emplace_back(TestData{"succ"});
    kData.emplace_back(TestData{"pred"});
    kData.emplace_back(TestData{"pred pred"});
    kData.emplace_back(TestData{"pred succ"});
    kData.emplace_back(TestData{"pred succ 1"});
    kData.emplace_back(TestData{"pred succ if true then true false"});
    kData.emplace_back(TestData{"succ"});
    kData.emplace_back(TestData{"succ 1"});
    kData.emplace_back(TestData{"succ pred 0 pred"});
    kData.emplace_back(TestData{"succ pred 0 pred 0"});
    kData.emplace_back(TestData{"succ pred 0 presd"});
    kData.emplace_back(TestData{"succ succ 1"});
    kData.emplace_back(TestData{"{x=succ 0, y=l z:Bool. z} a:Nat"});
    kData.emplace_back(TestData{"{x=succ 0, y=l z:Bool. z}."});
    kData.emplace_back(TestData{"{x=succ 0, y=}"});
    kData.emplace_back(TestData{"{x=succ 0, true}"});
    kData.emplace_back(TestData{".z"});
    kData.emplace_back(TestData{"ref"});
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

    kData.emplace_back(
        TestData{"(l x:Bool. x)", Type::Function(Type::Bool(), Type::Bool())});

    kData.emplace_back(TestData{
        "(l x:Bool. x x)", Type::Function(Type::Bool(), Type::IllTyped())});

    kData.emplace_back(TestData{
        "(l x:Bool. x a)", Type::Function(Type::Bool(), Type::IllTyped())});

    kData.emplace_back(
        TestData{"(l x:Bool. x y l y:Bool. y l z:Bool. z)",
                 Type::Function(Type::Bool(), Type::IllTyped())});

    kData.emplace_back(
        TestData{"(l x:Bool. l y:Bool. y)",
                 Type::Function(Type::Bool(),
                                Type::Function(Type::Bool(), Type::Bool()))});

    kData.emplace_back(
        TestData{"(l x:Bool. x) (l y:Bool. y)", Type::IllTyped()});

    kData.emplace_back(TestData{"(l x:Bool. x) true", Type::Bool()});

    kData.emplace_back(TestData{"(l x:Bool->Bool. x) (l y:Bool. y)",
                                Type::Function(Type::Bool(), Type::Bool())});

    kData.emplace_back(TestData{"(l x:Bool. x) x", Type::IllTyped()});

    kData.emplace_back(TestData{
        "l x :Bool. (l y:Bool.((x y) x))",
        Type::Function(Type::Bool(),
                       Type::Function(Type::Bool(), Type::IllTyped()))});

    kData.emplace_back(TestData{"l x:Bool. (l y:Bool. y) x",
                                Type::Function(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"l x:Bool->Bool. l y:Bool. x y",
                 Type::Function(Type::Function(Type::Bool(), Type::Bool()),
                                Type::Function(Type::Bool(), Type::Bool()))});

    kData.emplace_back(
        TestData{"(l z:Bool. l x:Bool. x) (l  y:Bool. y)", Type::IllTyped()});

    kData.emplace_back(TestData{"true", Type::Bool()});

    kData.emplace_back(TestData{"false", Type::Bool()});

    kData.emplace_back(TestData{
        "l x:(Bool->Bool)->Bool->(Bool->Bool). x",
        Type::Function(
            Type::Function(
                Type::Function(Type::Bool(), Type::Bool()),
                Type::Function(Type::Bool(),
                               Type::Function(Type::Bool(), Type::Bool()))),
            Type::Function(
                Type::Function(Type::Bool(), Type::Bool()),
                Type::Function(Type::Bool(),
                               Type::Function(Type::Bool(), Type::Bool()))))});

    kData.emplace_back(TestData{"if true then true else false", Type::Bool()});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool->Bool. y) "
                 "else (l x:Bool. false)",
                 Type::IllTyped()});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool. y) "
                 "else (l x:Bool. x)",
                 Type::Function(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"if (if true then true else false) then (l y:Bool. y) "
                 "else (l x:Bool. false)",
                 Type::Function(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"if (l x:Bool. x) then true else false", Type::IllTyped()});

    kData.emplace_back(TestData{"l x:Bool. if true then true else false",
                                Type::Function(Type::Bool(), Type::Bool())});

    kData.emplace_back(
        TestData{"if true then (l x:Bool. x) true else false", Type::Bool()});

    kData.emplace_back(TestData{"0", Type::Nat()});

    kData.emplace_back(TestData{"succ 0", Type::Nat()});

    kData.emplace_back(TestData{"pred 0", Type::Nat()});

    kData.emplace_back(TestData{"iszero 0", Type::Bool()});

    kData.emplace_back(TestData{"iszero pred 0", Type::Bool()});

    kData.emplace_back(TestData{"pred iszero 0", Type::IllTyped()});

    kData.emplace_back(TestData{"l x:Nat. pred pred x",
                                Type::Function(Type::Nat(), Type::Nat())});

    kData.emplace_back(
        TestData{"(l x:Nat. pred pred x) succ succ succ 0", Type::Nat()});

    kData.emplace_back(TestData{"{x=0}", Type::Record({{"x", Type::Nat()}})});

    kData.emplace_back(
        TestData{"{x=0, y=true}",
                 Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}})});

    kData.emplace_back(TestData{
        "{x=0, y=true, z=l x:Bool. x}",
        Type::Record({{"x", Type::Nat()},
                      {"y", Type::Bool()},
                      {"z", Type::Function(Type::Bool(), Type::Bool())}})});

    kData.emplace_back(TestData{"{x=if true then 0 else pred (succ succ 0)}",
                                Type::Record({{"x", Type::Nat()}})});

    kData.emplace_back(TestData{"{x=if true then 0 else iszero 0}",
                                Type::Record({{"x", Type::Top()}})});

    kData.emplace_back(TestData{"{x=0}.x", Type::Nat()});

    kData.emplace_back(TestData{"{x=0}.y", Type::IllTyped()});

    kData.emplace_back(TestData{"{x=0, y=true}.y", Type::Bool()});

    kData.emplace_back(TestData{"let x = true in l y:Nat. x",
                                Type::Function(Type::Nat(), Type::Bool())});

    kData.emplace_back(
        TestData{"let x = l x:Bool. x in l y:Nat. x",
                 Type::Function(Type::Nat(),
                                Type::Function(Type::Bool(), Type::Bool()))});

    kData.emplace_back(TestData{"let x = true in l x:Nat. x",
                                Type::Function(Type::Nat(), Type::Nat())});

    kData.emplace_back(TestData{"(l y:Nat. (let x = y in x)) 0", Type::Nat()});

    kData.emplace_back(
        TestData{"(l y:Nat. (let x = succ y in x)) 0", Type::Nat()});

    kData.emplace_back(
        TestData{"(l y:Nat. (let x = succ y in succ x)) 0", Type::Nat()});

    kData.emplace_back(TestData{"(l y:Nat. (let x = succ false in succ x)) 0",
                                Type::IllTyped()});
}

struct SubtypingTestData {
    Type& s_;
    Type& t_;
    bool expected_is_subtype_;
};

std::vector<SubtypingTestData> kSubtypingData{};

void InitSubtypingData() {
    kSubtypingData.emplace_back(
        SubtypingTestData{Type::Bool(), Type::Bool(), true});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Record({{"a", Type::Bool()}, {"b", Type::Nat()}}),
        Type::Record({{"a", Type::Bool()}, {"b", Type::Nat()}}), true});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Record({{"b", Type::Nat()}, {"a", Type::Bool()}}),
        Type::Record({{"a", Type::Bool()}, {"b", Type::Nat()}}), true});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Record({{"a", Type::Nat()}, {"b", Type::Bool()}}),
        Type::Record({{"a", Type::Bool()}, {"b", Type::Nat()}}), false});

    kSubtypingData.emplace_back(
        SubtypingTestData{Type::Record({{"a", Type::Nat()}}),
                          Type::Record({{"a", Type::Bool()}}), false});

    kSubtypingData.emplace_back(
        SubtypingTestData{Type::Function(Type::Bool(), Type::Nat()),
                          Type::Function(Type::Bool(), Type::Nat()), true});

    kSubtypingData.emplace_back(
        SubtypingTestData{Type::Function(Type::Nat(), Type::Bool()),
                          Type::Function(Type::Bool(), Type::Nat()), false});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Function(Type::Record({{"a", Type::Nat()}}), Type::Bool()),
        Type::Function(Type::Record({{"a", Type::Nat()}, {"b", Type::Nat()}}),
                       Type::Bool()),
        true});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Function(Type::Record({{"a", Type::Nat()}, {"b", Type::Nat()}}),
                       Type::Bool()),
        Type::Function(Type::Record({{"a", Type::Nat()}}), Type::Bool()),
        false});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Function(Type::Bool(),
                       Type::Record({{"a", Type::Nat()}, {"b", Type::Nat()}})),
        Type::Function(Type::Bool(), Type::Record({{"a", Type::Nat()}})),
        true});

    kSubtypingData.emplace_back(SubtypingTestData{
        Type::Function(Type::Bool(), Type::Record({{"a", Type::Nat()}})),
        Type::Function(Type::Bool(),
                       Type::Record({{"a", Type::Nat()}, {"b", Type::Nat()}})),
        false});
}

struct JoinTestData {
    Type& s_;
    Type& t_;
    Type& expected_join_type_;
};

std::vector<JoinTestData> kJoinData{};

void InitJoinData() {
    kJoinData.emplace_back(
        JoinTestData{Type::Bool(), Type::Bool(), Type::Bool()});

    kJoinData.emplace_back(
        JoinTestData{Type::Bool(), Type::Nat(), Type::Top()});

    {
        Type& s = Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}});
        Type& t = Type::Record({{"x", Type::Nat()}});
        Type& j = t;
        kJoinData.emplace_back(JoinTestData{s, t, j});
    }

    {
        Type& s = Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}});
        Type& t = Type::Record({{"x", Type::Nat()}, {"z", Type::Nat()}});
        Type& j = Type::Record({{"x", Type::Nat()}});
        kJoinData.emplace_back(JoinTestData{s, t, j});
    }

    {
        Type& s1 = Type::Bool();
        Type& s2 = Type::Bool();

        Type& t1 = Type::Bool();
        Type& t2 = Type::Bool();

        Type& s = Type::Function(s1, s2);
        Type& t = Type::Function(t1, t2);
        Type& j = Type::Function(t1, t2);

        kJoinData.emplace_back(JoinTestData{s, t, j});
    }

    {
        Type& s1 = Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}});
        Type& s2 = Type::Bool();

        Type& t1 = Type::Bool();
        Type& t2 = Type::Bool();

        Type& s = Type::Function(s1, s2);
        Type& t = Type::Function(t1, t2);
        Type& j = Type::IllTyped();

        kJoinData.emplace_back(JoinTestData{s, t, j});
    }

    {
        Type& s1 = Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}});
        Type& s2 = Type::Bool();

        Type& t1 = Type::Record({{"x", Type::Nat()}, {"z", Type::Bool()}});
        Type& t2 = Type::Bool();

        Type& j1 = Type::Record(
            {{"x", Type::Nat()}, {"y", Type::Bool()}, {"z", Type::Bool()}});

        Type& s = Type::Function(s1, s2);
        Type& t = Type::Function(t1, t2);
        Type& j = Type::Function(j1, t2);

        kJoinData.emplace_back(JoinTestData{s, t, j});
    }

    {
        Type& s1 = Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}});
        Type& s2 = Type::Bool();

        Type& t1 = Type::Record({{"x", Type::Nat()}, {"z", Type::Bool()}});
        Type& t2 = Type::Nat();

        Type& j1 = Type::Record(
            {{"x", Type::Nat()}, {"y", Type::Bool()}, {"z", Type::Bool()}});

        Type& s = Type::Function(s1, s2);
        Type& t = Type::Function(t1, t2);
        Type& j = Type::Function(j1, Type::Top());

        kJoinData.emplace_back(JoinTestData{s, t, j});
    }

    {
        Type& s1 = Type::Record({{"x", Type::Nat()}, {"y", Type::Bool()}});
        Type& s2 = s1;

        Type& t1 = Type::Record({{"x", Type::Nat()}, {"z", Type::Bool()}});
        Type& t2 = t1;

        Type& j1 = Type::Record(
            {{"x", Type::Nat()}, {"y", Type::Bool()}, {"z", Type::Bool()}});
        Type& j2 = Type::Record({{"x", Type::Nat()}});

        Type& s = Type::Function(s1, s2);
        Type& t = Type::Function(t1, t2);
        Type& j = Type::Function(j1, j2);

        kJoinData.emplace_back(JoinTestData{s, t, j});
    }
}

void Run() {
    InitData();
    InitSubtypingData();
    InitJoinData();

    size_t total_num_tests =
        kData.size() + kSubtypingData.size() + kJoinData.size();

    // Test type checking.
    std::cout << color::kYellow << "[Type Checker] Running " << total_num_tests
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

    TypeChecker type_checker;

    // Test subtyping.
    for (const auto& test : kSubtypingData) {
        if (type_checker.IsSubtype(test.s_, test.t_) !=
            test.expected_is_subtype_) {
            std::cout << color::kRed << "Test failed:" << color::kReset << "\n";

            std::cout << "  S: " << test.s_ << ", T: " << test.t_ << "\n";

            std::cout << color::kGreen << "  Exptected S<:T : " << color::kReset
                      << test.expected_is_subtype_ << "\n";

            std::cout << color::kRed << "  Actual S<:T : " << color::kReset
                      << !test.expected_is_subtype_ << "\n";
            ++num_failed;
        }
    }

    // Test join (least common supertype) calculation.
    for (const auto& test : kJoinData) {
        Type& actual_join_type = type_checker.Join(test.s_, test.t_);

        if (actual_join_type != test.expected_join_type_) {
            std::cout << color::kRed << "Test failed:" << color::kReset << "\n";

            std::cout << "  S: " << test.s_ << ", T: " << test.t_ << "\n";

            std::cout << color::kGreen
                      << "  Exptected S v T : " << color::kReset
                      << test.expected_join_type_ << "\n";

            std::cout << color::kRed << "  Actual S v T : " << color::kReset
                      << actual_join_type << "\n";
            ++num_failed;
        }
    }

    std::cout << color::kYellow << "Results: " << color::kReset
              << (total_num_tests - num_failed) << " out of " << total_num_tests
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

    kData.emplace_back(TestData{"0", {"0", Type::Nat()}});

    kData.emplace_back(
        TestData{"if false then true else 0", {"0", Type::Top()}});

    kData.emplace_back(
        TestData{"if false then true else succ 0", {"1", Type::Top()}});

    kData.emplace_back(
        TestData{"if false then true else succ succ 0", {"2", Type::Top()}});

    kData.emplace_back(TestData{"(l x:Nat. x) succ 0", {"1", Type::Nat()}});

    kData.emplace_back(
        TestData{"(l x:Nat. succ x) succ 0", {"2", Type::Nat()}});

    kData.emplace_back(TestData{"(l x:Bool. x) true", {"true", Type::Bool()}});

    kData.emplace_back(TestData{"(l x:Bool. x) if false then true else false",
                                {"false", Type::Bool()}});

    kData.emplace_back(TestData{
        "(l x:Bool. x) if false then true else l x:Bool. x",
        {"({l x : Bool. x} <- if (false) then (true) else ({l x : Bool. x}))",
         Type::IllTyped()}});

    kData.emplace_back(TestData{"(l x:Bool. if x then true else false) true",
                                {"true", Type::Bool()}});

    kData.emplace_back(TestData{"(l x:Bool. if x then true else false) false",
                                {"false", Type::Bool()}});

    kData.emplace_back(
        TestData{"(l x:Nat. succ succ x) 0", {"2", Type::Nat()}});

    kData.emplace_back(
        TestData{"(l x:Nat. succ succ x) succ 0", {"3", Type::Nat()}});

    kData.emplace_back(TestData{"{x=0}.x", {"0", Type::Nat()}});

    kData.emplace_back(TestData{"{x=0, y=true}.y", {"true", Type::Bool()}});

    kData.emplace_back(
        TestData{"{x=0, y=l x:Nat. x}.y",
                 {"{l x : Nat. x}", Type::Function(Type::Nat(), Type::Nat())}});

    kData.emplace_back(TestData{"pred succ 0", {"0", Type::Nat()}});

    kData.emplace_back(
        TestData{"((l r:{x:Nat}. r) {x=succ 0}).x", {"1", Type::Nat()}});

    kData.emplace_back(
        TestData{"{x=pred succ 0, y=if true then false else true}.y",
                 {"false", Type::Bool()}});

    kData.emplace_back(
        TestData{"(l r:{x:Nat}. r.x) {x=succ 0}", {"1", Type::Nat()}});

    kData.emplace_back(TestData{"(l r:{x:Nat}. succ r.x) {x=succ 0, y=true}",
                                {"2", Type::Nat()}});

    kData.emplace_back(
        TestData{"(l r:{a:{x:Nat}}. r.a.x) {a={x=succ 0, y=true}, b=false}",
                 {"1", Type::Nat()}});

    kData.emplace_back(TestData{"let x = true in x", {"true", Type::Bool()}});

    kData.emplace_back(TestData{
        "let x = true in l y:Nat. x",
        {"{l y : Nat. true}", Type::Function(Type::Nat(), Type::Bool())}});

    kData.emplace_back(TestData{"(l y:Nat. (let x = succ y in succ x)) 0",
                                {"2", Type::Nat()}});

    kData.emplace_back(TestData{
        "(l y:Nat. (let x = succ y in if iszero y then succ x else y)) 0",
        {"2", Type::Nat()}});

    kData.emplace_back(TestData{
        "(l y:Nat. (let x = succ y in if iszero y then succ x else y)) succ 0",
        {"1", Type::Nat()}});

    kData.emplace_back(TestData{
        "{x=true}", {"{x=true}", Type::Record({{"x", Type::Bool()}})}});
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
