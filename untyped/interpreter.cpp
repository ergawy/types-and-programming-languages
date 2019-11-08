#include <iostream>
#include <sstream>
#include <stack>
#include <vector>

namespace lexer {
struct Token {
    enum class Category {
        VARIABLE,
        LAMBDA,
        LAMBDA_DOT,
        OPEN_PAREN,
        CLOSE_PAREN,
        MARKER_END,
        MARKER_INVALID
    };

    Category category = Category::MARKER_INVALID;
    std::string text;
};
std::ostream& operator<<(std::ostream& out, Token token);

class Lexer {
    static const std::string kLambdaInputSymbol;

   public:
    Lexer(std::istringstream&& in) : in_(std::move(in)) {}

    Token NextToken() {
        if (is_cached_token_valid) {
            is_cached_token_valid = false;
            return cached_token;
        }

        Token token;
        char next_char = 0;
        std::ostringstream token_text_out;
        while (in_.get(next_char) && !IsSeparator(next_char)) {
            token_text_out << next_char;
        }

        auto token_text = token_text_out.str();

        if (token_text == kLambdaInputSymbol) {
            token.category = Token::Category::LAMBDA;
        } else if (!token_text.empty()) {
            token.category = Token::Category::VARIABLE;
            token.text = token_text;
        } else if (next_char == '(') {
            // There is no token before next_char, then create a token of it and
            // clear next_char.
            token.category = Token::Category::OPEN_PAREN;
            next_char = 0;
        } else if (next_char == ')') {
            // There is no token before next_char, then create a token of it and
            // clear next_char.
            token.category = Token::Category::CLOSE_PAREN;
            next_char = 0;
        } else if (next_char == '.') {
            // There is no token before next_char, then create a token of it and
            // clear next_char.
            token.category = Token::Category::LAMBDA_DOT;
            next_char = 0;
        } else if (!in_) {
            token.category = Token::Category::MARKER_END;
        } else {
            token = NextToken();
        }

        // There is a token before next_char, then return that token and cache
        // next_char's token for next call to NextToken().
        if (next_char == '.') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::LAMBDA_DOT;
        } else if (next_char == '(') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::OPEN_PAREN;
        } else if (next_char == ')') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::CLOSE_PAREN;
        }

        if (token.category == Token::Category::MARKER_INVALID) {
            throw std::invalid_argument("Error: invalide token: " + token.text);
        }

        return token;
    }

   private:
    bool IsSeparator(char c) {
        return std::string{" .()"}.find(c) != std::string::npos;
    }

   private:
    std::istringstream in_;
    bool is_cached_token_valid = false;
    Token cached_token;
};

const std::string Lexer::kLambdaInputSymbol = "l";

std::ostream& operator<<(std::ostream& out, Token token) {
    switch (token.category) {
        case Token::Category::LAMBDA:
            out << "lambda";
            break;
        case Token::Category::VARIABLE:
            out << token.text;
            break;
        case Token::Category::LAMBDA_DOT:
            out << ".";
            break;
        case Token::Category::OPEN_PAREN:
            out << "(";
            break;
        case Token::Category::CLOSE_PAREN:
            out << ")";
            break;
        case Token::Category::MARKER_END:
            out << "<END>";
            break;

        case Token::Category::MARKER_INVALID:
            out << "<INVALID>";
            break;

        default:
            out << "<ILLEGAL_TOKEN>";
    }

    return out;
}
}  // namespace lexer

namespace parser {

class Term {
    friend std::ostream& operator<<(std::ostream&, const Term&);

   public:
    static Term Lambda(std::string arg_name) {
        Term result;
        result.lambda_arg_name_ = arg_name;
        result.is_lambda_ = true;

        return result;
    }

    static Term Variable(std::string var_name) {
        Term result;
        result.variable_name_ = var_name;
        result.is_variable_ = true;

        return result;
    }

    static Term Application(std::unique_ptr<Term> lhs,
                            std::unique_ptr<Term> rhs) {
        Term result;
        result.is_application_ = true;
        result.application_lhs_ = std::move(lhs);
        result.application_rhs_ = std::move(rhs);

        return result;
    }

    Term() = default;

    Term(const Term&) = delete;
    Term& operator=(const Term&) = delete;

    Term(Term&&) = default;
    Term& operator=(Term&&) = default;

    bool IsLambda() const { return is_lambda_; }

    bool IsVariable() const { return is_variable_; }

    bool IsApplication() const { return is_application_; }
    bool IsInvalid() const {
        if (IsLambda()) {
            return lambda_arg_name_.empty() || !lambda_body_;
        } else if (IsVariable()) {
            return variable_name_.empty();
        } else if (IsApplication()) {
            return !application_lhs_ || !application_rhs_;
        }

        return true;
    }

    void Combine(Term&& term) {
        if (term.IsInvalid()) {
            throw std::invalid_argument(
                "Term::Combine() received an invalid Term.");
        }

        if (IsLambda()) {
            if (lambda_body_) {
                lambda_body_->Combine(std::move(term));
            } else {
                lambda_body_ = std::make_unique<Term>(std::move(term));
            }
        } else if (IsVariable()) {
            *this = Application(std::make_unique<Term>(std::move(*this)),
                                std::make_unique<Term>(std::move(term)));

            is_variable_ = false;
            variable_name_ = "";
        } else if (IsApplication()) {
            *this = Application(std::make_unique<Term>(std::move(*this)),
                                std::make_unique<Term>(std::move(term)));
        } else {
            *this = std::move(term);
        }
    }

   private:
    bool is_lambda_ = false;
    std::string lambda_arg_name_ = "";
    std::unique_ptr<Term> lambda_body_{};

    bool is_variable_ = false;
    std::string variable_name_ = "";

    bool is_application_ = false;
    std::unique_ptr<Term> application_lhs_{};
    std::unique_ptr<Term> application_rhs_{};
};

std::ostream& operator<<(std::ostream& out, const Term& term) {
    if (term.IsVariable()) {
        out << term.variable_name_;
    } else if (term.IsLambda()) {
        out << "{λ " << term.lambda_arg_name_ << ". " << *term.lambda_body_
            << "}";
    } else if (term.IsApplication()) {
        out << "(" << *term.application_lhs_ << " <- " << *term.application_rhs_
            << ")";
    } else {
        out << "<ERROR>";
    }

    return out;
}

class Parser {
    using Token = lexer::Token;

   public:
    Parser(std::istringstream&& in) : lexer_(std::move(in)) {}

    Term ParseProgram() {
        Token next_token;
        std::stack<Term> term_stack;
        term_stack.emplace(Term());
        int balance_parens = 0;

        while ((next_token = lexer_.NextToken()).category !=
               Token::Category::MARKER_END) {
            if (next_token.category == Token::Category::LAMBDA) {
                auto lambda_arg = ParseVariable();
                ParseDot();
                term_stack.emplace(Term::Lambda(lambda_arg.text));
            } else if (next_token.category == Token::Category::VARIABLE) {
                term_stack.top().Combine(Term::Variable(next_token.text));
            } else if (next_token.category == Token::Category::OPEN_PAREN) {
                term_stack.emplace(Term());
                ++balance_parens;
            } else if (next_token.category == Token::Category::CLOSE_PAREN) {
                CombineStackTop(term_stack);

                // A prenthesized λ-abstration is equivalent to a term
                // double parenthesized term since we push a new term on the
                // stack for each lambda.
                if (term_stack.top().IsLambda()) {
                    CombineStackTop(term_stack);
                }

                --balance_parens;
            } else {
                throw std::invalid_argument(
                    "Unexpected token: " +
                    (std::ostringstream() << next_token).str());
            }
        }

        if (balance_parens != 0) {
            throw std::invalid_argument(
                "Invalid term: probably because a ( is not matched by a )");
        }

        while (term_stack.size() > 1) {
            CombineStackTop(term_stack);
        }

        return std::move(term_stack.top());
    }

    void CombineStackTop(std::stack<Term>& term_stack) {
        if (term_stack.size() < 2) {
            throw std::invalid_argument(
                "Invalid term: probably because a ( is not matched by a )");
        }

        Term top = std::move(term_stack.top());
        term_stack.pop();
        term_stack.top().Combine(std::move(top));
    }

    Token ParseVariable() {
        auto token = lexer_.NextToken();

        return (token.category == Token::Category::VARIABLE)
                   ? token
                   : throw std::logic_error("Expected to parse a variable.");
    }

    Token ParseDot() {
        auto token = lexer_.NextToken();

        return (token.category == Token::Category::LAMBDA_DOT)
                   ? token
                   : throw std::logic_error("Expected to parse a dot.");
    }

    Token ParseCloseParen() {
        auto token = lexer_.NextToken();

        return (token.category == Token::Category::CLOSE_PAREN)
                   ? token
                   : throw std::logic_error("Expected to parse a ')'.");
    }

   private:
    lexer::Lexer lexer_;
};
}  // namespace parser

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr
            << "Error: expected input program as a command line argument.\n";
        return 1;
    }

    // lexer::Lexer lexer{std::istringstream{argv[1]}};

    // for (auto token = lexer.NextToken();
    //     token.category != lexer::Token::Category::MARKER_END;
    //     token = lexer.NextToken()) {
    //    std::cout << token << "_";
    //}

    // std::cout << "\n";

    parser::Parser parser{std::istringstream{argv[1]}};

    std::cout << parser.ParseProgram() << "\n";

    return 0;
}
