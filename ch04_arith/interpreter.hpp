#pragma once

#include <iostream>
#include <sstream>
#include <vector>

namespace lexer {
struct Token {
    enum class Category {
        CONSTANT_TRUE,
        CONSTANT_FALSE,
        CONSTANT_ZERO,

        KEYWORD_IF,
        KEYWORD_THEN,
        KEYWORD_ELSE,
        KEYWORD_SUCC,
        KEYWORD_PRED,
        KEYWORD_ISZERO,

        MARKER_ERROR,
        MARKER_END
    };

    bool operator==(const Token& other) const {
        return category == other.category && text == other.text;
    }

    bool operator!=(const Token& other) const { return !(*this == other); }

    std::string DebugString() const;

    Category category;
    std::string text;
};

class Lexer {
   public:
    Lexer(std::istringstream&& in) : in_(std::move(in)) {}

    Token NextToken() {
        Token token;

        if (in_ >> token.text) {
            if (token.text == "true") {
                token.category = Token::Category::CONSTANT_TRUE;
            } else if (token.text == "false") {
                token.category = Token::Category::CONSTANT_FALSE;
            } else if (token.text == "0") {
                token.category = Token::Category::CONSTANT_ZERO;
            } else if (token.text == "if") {
                token.category = Token::Category::KEYWORD_IF;
            } else if (token.text == "then") {
                token.category = Token::Category::KEYWORD_THEN;
            } else if (token.text == "else") {
                token.category = Token::Category::KEYWORD_ELSE;
            } else if (token.text == "succ") {
                token.category = Token::Category::KEYWORD_SUCC;
            } else if (token.text == "pred") {
                token.category = Token::Category::KEYWORD_PRED;
            } else if (token.text == "iszero") {
                token.category = Token::Category::KEYWORD_ISZERO;
            } else {
                token.category = Token::Category::MARKER_ERROR;
            }
        } else {
            token.category = Token::Category::MARKER_END;
        }

        return token;
    }

   private:
    std::istringstream in_;
};

std::ostream& operator<<(std::ostream& out, Token::Category token_category) {
    switch (token_category) {
        case Token::Category::CONSTANT_TRUE:
            out << "true";
            break;
        case Token::Category::CONSTANT_FALSE:
            out << "false";
            break;
        case Token::Category::CONSTANT_ZERO:
            out << "0";
            break;

        case Token::Category::KEYWORD_IF:
            out << "if";
            break;
        case Token::Category::KEYWORD_THEN:
            out << "then";
            break;
        case Token::Category::KEYWORD_ELSE:
            out << "else";
            break;
        case Token::Category::KEYWORD_SUCC:
            out << "succ";
            break;
        case Token::Category::KEYWORD_PRED:
            out << "pred";
            break;
        case Token::Category::KEYWORD_ISZERO:
            out << "iszero";
            break;

        case Token::Category::MARKER_ERROR:
            out << "<ERROR>";
            break;
        case Token::Category::MARKER_END:
            out << "<END>";
            break;

        default:
            out << "<ILLEGAL_TOKEN>";
    }

    return out;
}

std::string Token::DebugString() const {
    return (std::ostringstream{} << "{text: " << text
                                 << ", category: " << category << "}")
        .str();
}

}  // namespace lexer

namespace parser {

class Term {
    using Cat = lexer::Token::Category;
    friend std::ostream& operator<<(std::ostream& out,
                                    const Term& token_category);

   public:
    Term(Cat first_token_category, std::vector<Term> sub_terms = {})
        : first_token_category_(first_token_category), sub_terms_(sub_terms) {}

    Term() = default;
    Term(const Term&) = default;
    Term(Term&&) = default;
    Term& operator=(const Term&) = default;
    Term& operator=(Term&&) = default;

    Cat Category() const { return first_token_category_; }
    Term& SubTerm(int i) { return sub_terms_[i]; }

    std::string ASTString(int indentation = 0) const {
        std::ostringstream out;
        std::string prefix = std::string(indentation, ' ');

        switch (Category()) {
            case Cat::CONSTANT_ZERO:
            case Cat::CONSTANT_TRUE:
            case Cat::CONSTANT_FALSE:
                out << prefix << Category();
                break;

            case Cat::KEYWORD_IF:
                out << prefix << Category() << "\n";
                out << sub_terms_[0].ASTString(indentation + 2) << "\n";
                out << prefix << Cat::KEYWORD_ELSE << "\n";
                out << sub_terms_[1].ASTString(indentation + 2) << "\n";
                out << prefix << Cat::KEYWORD_THEN << "\n";
                out << sub_terms_[2].ASTString(indentation + 2);
                break;

            case Cat::KEYWORD_SUCC:
            case Cat::KEYWORD_PRED:
            case Cat::KEYWORD_ISZERO:
                out << prefix << Category() << "\n";
                out << sub_terms_[0].ASTString(indentation + 2);
                break;

            default:
                break;
        }

        return out.str();
    }

    bool operator==(const Term& other) const {
        if (Category() != other.Category()) {
            return false;
        }

        bool res = true;

        switch (Category()) {
            case Cat::CONSTANT_ZERO:
            case Cat::CONSTANT_TRUE:
            case Cat::CONSTANT_FALSE:
                break;

            case Cat::KEYWORD_IF:
                for (int i = 0; i < 3; ++i) {
                    res = res && (sub_terms_[i] == other.sub_terms_[i]);

                    if (!res) {
                        break;
                    }
                }

                break;

            case Cat::KEYWORD_SUCC:
            case Cat::KEYWORD_PRED:
            case Cat::KEYWORD_ISZERO:
                res = res && (sub_terms_[0] == other.sub_terms_[0]);

                break;

            default:
                break;
        }

        return res;
    }

    bool operator!=(const Term& other) const { return !(*this == other); }

   private:
    // Category of the first token in a term speicifies the type of the term.
    Cat first_token_category_;
    std::vector<Term> sub_terms_;
};

std::ostream& operator<<(std::ostream& out, const Term& term) {
    using Category = lexer::Token::Category;
    out << term.first_token_category_;

    if (term.first_token_category_ == Category::KEYWORD_IF) {
        out << " (" << term.sub_terms_[0] << ") " << Category::KEYWORD_THEN
            << " (" << term.sub_terms_[1] << ") " << Category::KEYWORD_ELSE
            << " (" << term.sub_terms_[2] << ")";
    } else if (term.first_token_category_ == Category::KEYWORD_SUCC ||
               term.first_token_category_ == Category::KEYWORD_PRED ||
               term.first_token_category_ == Category::KEYWORD_ISZERO) {
        out << " (" << term.sub_terms_[0] << ")";
    }

    return out;
}

class Parser {
   public:
    Parser(std::istringstream&& in) : lexer_(std::move(in)) {}

    Term ParseProgram() {
        auto program = NextTerm();

        if (lexer_.NextToken().category != lexer::Token::Category::MARKER_END) {
            throw std::invalid_argument(
                "Error: extra input after program end.");
        }

        return program;
    }

   private:
    Term NextTerm() {
        using Category = lexer::Token::Category;
        auto token = lexer_.NextToken();

        std::vector<Term> sub_terms;

        switch (token.category) {
                // Possible terms:
            case Category::CONSTANT_TRUE:
            case Category::CONSTANT_FALSE:
                break;

            case Category::CONSTANT_ZERO:
                break;

            case Category::KEYWORD_IF: {
                // Add condition sub-term.
                sub_terms.emplace_back(NextTerm());

                if (lexer_.NextToken().category != Category::KEYWORD_THEN) {
                    // Parsing error:
                    throw std::invalid_argument(
                        "Error: invalid if-then-else term.");
                }

                // Add then branch sub-term.
                sub_terms.emplace_back(NextTerm());

                if (lexer_.NextToken().category != Category::KEYWORD_ELSE) {
                    // Parsing error:
                    throw std::invalid_argument(
                        "Error: invalid if-then-else term.");
                }

                // Add then else sub-term.
                sub_terms.emplace_back(NextTerm());
            } break;

            case Category::KEYWORD_SUCC: {
                auto sub_term = NextTerm();
                sub_terms.emplace_back(sub_term);
            } break;

            case Category::KEYWORD_PRED:
            case Category::KEYWORD_ISZERO: {
                auto sub_term = NextTerm();
                sub_terms.emplace_back(sub_term);
            } break;

                // End of input (parse error):
            case Category::MARKER_END:
                throw std::invalid_argument("Error: reached end of input.");

                // Lexing errors:
            case Category::MARKER_ERROR:
                throw std::invalid_argument(
                    "Error: invalid token: " + token.text + ".");

                // Parsing errors:
            case Category::KEYWORD_THEN:
            case Category::KEYWORD_ELSE:
                throw std::invalid_argument("Error: invalid term start " +
                                            token.text + ".");
        }

        return Term(token.category, sub_terms);
    }

   private:
    lexer::Lexer lexer_;
};
}  // namespace parser

namespace interpreter {
using lexer::Token;
using parser::Term;

class Interpreter {
   public:
    std::string Interpret(Term program) {
        Term res = Eval(program);
        return AsString(IsValue(res) ? res
                                     : Term(Token::Category::MARKER_ERROR));
    }

   private:
    std::string AsString(Term value) {
        std::ostringstream ss;
        ss << value;
        auto term_str = ss.str();

        if (IsNumericValue(value)) {
            std::size_t start_pos = 0;
            int num = 0;

            while ((start_pos = term_str.find("succ", start_pos)) !=
                   std::string::npos) {
                ++num;
                ++start_pos;
            }

            return std::to_string(num);
        }

        return term_str;
    }

    Term Eval(Term term) {
        try {
            Term res = Eval1(term);
            return Eval(res);
        } catch (std::invalid_argument&) {
            return term;
        }
    }

    Term Eval1(Term term) {
        switch (term.Category()) {
            case Token::Category::KEYWORD_IF: {
                return Eval1If(term);
            }

            case Token::Category::KEYWORD_SUCC: {
                term.SubTerm(0) = Eval1(term.SubTerm(0));
                return term;
            }

            case Token::Category::KEYWORD_PRED: {
                return Eval1Pred(term);
            }

            case Token::Category::KEYWORD_ISZERO: {
                return Eval1IsZero(term);
            }

            default:
                throw std::invalid_argument("No applicable rule.");
        }
    }

    Term Eval1If(Term term) {
        switch (term.SubTerm(0).Category()) {
            case Token::Category::CONSTANT_TRUE: {
                return term.SubTerm(1);
            }

            case Token::Category::CONSTANT_FALSE: {
                return term.SubTerm(2);
            }

            default:
                term.SubTerm(0) = Eval1(term.SubTerm(0));
                return term;
        }
    }

    Term Eval1Pred(Term term) {
        switch (term.SubTerm(0).Category()) {
            case Token::Category::CONSTANT_ZERO: {
                return Term(Token::Category::CONSTANT_ZERO);
            }

            case Token::Category::KEYWORD_SUCC: {
                if (IsNumericValue(term.SubTerm(0))) {
                    return term.SubTerm(0).SubTerm(0);
                } else {
                    term.SubTerm(0) = Eval1(term.SubTerm(0));
                    return term;
                }
            }

            default:
                term.SubTerm(0) = Eval1(term.SubTerm(0));
                return term;
        }
    }

    Term Eval1IsZero(Term term) {
        switch (term.SubTerm(0).Category()) {
            case Token::Category::CONSTANT_ZERO: {
                return Term(Token::Category::CONSTANT_TRUE);
            }

            case Token::Category::KEYWORD_SUCC: {
                if (IsNumericValue(term.SubTerm(0))) {
                    return Term(Token::Category::CONSTANT_FALSE);
                } else {
                    term.SubTerm(0) = Eval1(term.SubTerm(0));
                    return term;
                }
            }

            default:
                term.SubTerm(0) = Eval1(term.SubTerm(0));
                return term;
        }
    }

    bool IsNumericValue(Term term) {
        return term.Category() == Token::Category::CONSTANT_ZERO ||
               (term.Category() == Token::Category::KEYWORD_SUCC &&
                IsNumericValue(term.SubTerm(0)));
    }

    bool IsValue(Term term) {
        return term.Category() == Token::Category::CONSTANT_TRUE ||
               term.Category() == Token::Category::CONSTANT_FALSE ||
               IsNumericValue(term);
    }
};
}  // namespace interpreter
