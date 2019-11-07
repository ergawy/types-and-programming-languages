#include <iostream>
#include <sstream>
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
            // std::cout << cached_token << " <--$\n";
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
            // std::cout << "====\n";
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
            // std::cout << "--> " << next_char << "|\n";
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

        // std::cout << token << " <--\n";
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
    Term() = default;

    explicit Term(std::string name) : var_name_(name) {}

    explicit Term(std::vector<Term>&& sub_terms)
        : sub_terms_(std::move(sub_terms)) {
        if (sub_terms_.empty() || sub_terms.size() > 2) {
            throw std::invalid_argument("Invalid number of sub-terms.");
        }
    }

    bool IsVariable() const { return !var_name_.empty() && sub_terms_.empty(); }

    bool IsLambda() const { return sub_terms_.size() == 1; }

    bool IsApplication() const { return sub_terms_.size() == 2; }

    const Term& operator[](int idx) const {
        if (IsVariable()) {
            throw std::logic_error("Trying to access sub-terms of a variable.");
        }

        if (IsLambda() && idx > 0) {
            throw std::logic_error(
                "A λ-abstraction contains a single sub-term.");
        }

        if (IsApplication() && idx > 1) {
            throw std::logic_error("An application contains two sub-term.");
        }

        return sub_terms_[idx];
    }

   private:
    std::string var_name_ = "";
    // If this Term represents a variable, this field stores its assigned de
    // Bruijn index in the nameless representation of the program.
    // int de_bruijn_idx_;

    // This either contains 0, 1 or 2 sub-terms based on whether this Term is a
    // variable, a λ-abstraction or an application.
    std::vector<Term> sub_terms_{};
    bool is_parenthesized_ = false;
};

std::ostream& operator<<(std::ostream& out, const Term& term) {
    if (term.IsVariable()) {
        out << term.var_name_;
    } else if (term.IsLambda()) {
        out << "lambda. {" << term[0] << "}";
    } else if (term.IsApplication()) {
        out << "(" << term[0] << ")<-"
            << "(" << term[1] << ")";
    } else {
        out << "<ERROR>";
    }

    return out;
}

class Parser {
    using Token = lexer::Token;

   public:
    Parser(std::istringstream&& in) : lexer_(std::move(in)) {}

    Term NextTerm() {
        auto token = (is_cached_token_valid)
                         ? (is_cached_token_valid = false, cached_token)
                         : lexer_.NextToken();
        Term result;

        if (token.category == Token::Category::LAMBDA) {
            ParseVariable();
            std::cout << "Parsed a variable.\n";
            ParseDot();
            std::cout << "Parsed a dot.\n";
            result = Term(std::vector<Term>{NextTerm()});
        } else {
            if (token.category != Token::Category::VARIABLE) {
                std::cout << "Oops: " << token << "\n";
                throw std::logic_error("Expected to parse a variable.");
            }

            result = Term(token.text);
            std::cout << "Exepcting a varible: " << token.text << "\n";
            auto token2 = lexer_.NextToken();

            if (token2.category != Token::Category::MARKER_END) {
                is_cached_token_valid = true;
                cached_token = token2;
                auto term2 = NextTerm();

                // If term2 is itself an application, then switch order of
                // application to make left-associative.
                if (term2.IsApplication()) {
                    result = Term(std::vector<Term>{
                        Term(std::vector<Term>{result, term2[0]}), term2[1]});
                } else {
                    result = Term(std::vector<Term>{result, term2});
                }

                token2 = lexer_.NextToken();
            }
        }

        return result;
    }

    Token ParseVariable() {
        auto token = (is_cached_token_valid)
                         ? (is_cached_token_valid = false, cached_token)
                         : lexer_.NextToken();

        return (token.category == Token::Category::VARIABLE)
                   ? token
                   : throw std::logic_error("Expected to parse a variable.");
    }

    Token ParseDot() {
        auto token = (is_cached_token_valid)
                         ? (is_cached_token_valid = false, cached_token)
                         : lexer_.NextToken();

        return (token.category == Token::Category::LAMBDA_DOT)
                   ? token
                   : throw std::logic_error("Expected to parse a dot.");
    }

    Token ParseCloseParen() {
        auto token = (is_cached_token_valid)
                         ? (is_cached_token_valid = false, cached_token)
                         : lexer_.NextToken();

        return (token.category == Token::Category::CLOSE_PAREN)
                   ? token
                   : throw std::logic_error("Expected to parse a ')'.");
    }

   private:
    lexer::Lexer lexer_;
    bool is_cached_token_valid = false;
    Token cached_token;
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

    std::cout << parser.NextTerm() << "\n";

    return 0;
}
