#pragma once

#include <iostream>
#include <iterator>
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
        COLON,
        ARROW,

        CONSTANT_TRUE,
        CONSTANT_FALSE,

        KEYWORD_BOOL,
        KEYWORD_IF,
        KEYWORD_THEN,
        KEYWORD_ELSE,

        MARKER_END,
        MARKER_INVALID
    };

    Token(Category category = Category::MARKER_INVALID, std::string text = "")
        : category_(category),
          text_(category == Category::VARIABLE ? text : "") {}

    bool operator==(const Token& other) const {
        return category_ == other.category_ && text_ == other.text_;
    }

    bool operator!=(const Token& other) const { return !(*this == other); }

    Category GetCategory() const { return category_; }

    std::string GetText() const { return text_; }

   private:
    Category category_ = Category::MARKER_INVALID;
    std::string text_;
};

std::ostream& operator<<(std::ostream& out, Token token);

namespace {
const std::string kLambdaInputSymbol = "l";
const std::string kKeywordBool = "Bool";
}  // namespace

class Lexer {
   public:
    Lexer(std::istringstream&& in) {
        std::istringstream iss(SurroundTokensBySpaces(std::move(in)));
        token_strings_ =
            std::vector<std::string>(std::istream_iterator<std::string>{iss},
                                     std::istream_iterator<std::string>());
    }

    Token NextToken() {
        Token token;

        if (current_token_ == token_strings_.size()) {
            token = Token(Token::Category::MARKER_END);
            return token;
        }

        if (token_strings_[current_token_] == "true") {
            token = Token(Token::Category::CONSTANT_TRUE);
        } else if (token_strings_[current_token_] == "false") {
            token = Token(Token::Category::CONSTANT_FALSE);
        } else if (token_strings_[current_token_] == "if") {
            token = Token(Token::Category::KEYWORD_IF);
        } else if (token_strings_[current_token_] == "then") {
            token = Token(Token::Category::KEYWORD_THEN);
        } else if (token_strings_[current_token_] == "else") {
            token = Token(Token::Category::KEYWORD_ELSE);
        } else if (token_strings_[current_token_] == kLambdaInputSymbol) {
            token = Token(Token::Category::LAMBDA);
        } else if (token_strings_[current_token_] == kKeywordBool) {
            token = Token(Token::Category::KEYWORD_BOOL);
        } else if (token_strings_[current_token_] == "(") {
            token = Token(Token::Category::OPEN_PAREN);
        } else if (token_strings_[current_token_] == ")") {
            token = Token(Token::Category::CLOSE_PAREN);
        } else if (token_strings_[current_token_] == ".") {
            token = Token(Token::Category::LAMBDA_DOT);
        } else if (token_strings_[current_token_] == ":") {
            token = Token(Token::Category::COLON);
        } else if (token_strings_[current_token_] == "->") {
            token = Token(Token::Category::ARROW);
        } else if (IsVariableName(token_strings_[current_token_])) {
            token = Token(Token::Category::VARIABLE,
                          token_strings_[current_token_]);
        }

        ++current_token_;

        return token;
    }

   private:
    std::string SurroundTokensBySpaces(std::istringstream&& in) {
        std::ostringstream processed_stream;
        char c;

        while (in.get(c)) {
            // Check for one-character separators and surround them with spaces.
            if (c == ':' || c == '.' || c == '(' || c == ')') {
                processed_stream << " " << c << " ";
            } else if (c == '-') {
                // Check for the only two-character serparator '->' and surround
                // it with spaces.
                if (in.peek() == '>') {
                    in.get(c);
                    processed_stream << " -> ";
                } else {
                    // Just write '-' and let the lexing error be
                    // discovered later.
                    processed_stream << " - ";
                }
            } else {
                processed_stream << c;
            }
        }

        return processed_stream.str();
    }

    bool IsVariableName(std::string token_text) {
        for (auto c : token_text) {
            if (!std::isalpha(c) && c != '_') {
                return false;
            }
        }

        return true;
    }

   private:
    std::vector<std::string> token_strings_;
    int current_token_ = 0;
};

std::ostream& operator<<(std::ostream& out, Token token) {
    switch (token.GetCategory()) {
        case Token::Category::LAMBDA:
            out << "λ";
            break;
        case Token::Category::KEYWORD_BOOL:
            out << "<Bool>";
            break;
        case Token::Category::VARIABLE:
            out << token.GetText();
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
        case Token::Category::COLON:
            out << ":";
            break;
        case Token::Category::ARROW:
            out << "→";
            break;

        case Token::Category::CONSTANT_TRUE:
            out << "<true>";
            break;
        case Token::Category::CONSTANT_FALSE:
            out << "<false>";
            break;
        case Token::Category::KEYWORD_IF:
            out << "<if>";
            break;
        case Token::Category::KEYWORD_THEN:
            out << "<then>";
            break;
        case Token::Category::KEYWORD_ELSE:
            out << "<else>";
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
