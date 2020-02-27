#pragma once

#include <deque>
#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <stdexcept>
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

    void PutBackToken() {
        if (current_token_ > 0) {
            --current_token_;
        }
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

        return !token_text.empty();
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
            out << "->";
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

namespace parser {

class Type {
    friend std::ostream& operator<<(std::ostream&, const Type&);

   public:
    // TODO: For now, types are created over and over again. Instead, create
    // each type once and use its reference. For one thing, a Type shouldn't be
    // owned by a term of that Type.

    static Type IllTyped() {
        Type type;
        type.ill_typed_ = true;
        return type;
    }

    static Type SimpleBool() {
        Type type;
        type.simple_bool_ = true;
        return type;
    }

    static Type FunctionType(std::unique_ptr<Type> lhs,
                             std::unique_ptr<Type> rhs) {
        Type type;
        type.lhs_ = std::move(lhs);
        type.rhs_ = std::move(rhs);

        return type;
    }

    Type() = default;

    Type(const Type&) = delete;
    Type& operator=(const Type&) = delete;

    Type(Type&&) = default;
    Type& operator=(Type&&) = default;

    ~Type() = default;

    Type Clone() const {
        if (IsIllTyped()) {
            return Type::IllTyped();
        }

        if (IsSimpleBool()) {
            return Type::SimpleBool();
        }

        return Type::FunctionType(std::make_unique<Type>(lhs_->Clone()),
                                  std::make_unique<Type>(rhs_->Clone()));
    }

    bool operator==(const Type& other) const {
        if (ill_typed_) {
            return other.ill_typed_;
        }

        if (simple_bool_) {
            return other.simple_bool_;
        }

        return lhs_ && rhs_ && other.lhs_ && other.rhs_ &&
               (*lhs_ == *other.lhs_) && (*rhs_ == *other.rhs_);
    }

    bool operator!=(const Type& other) const { return !(*this == other); }

    bool IsIllTyped() const { return ill_typed_; }

    bool IsSimpleBool() const { return simple_bool_; }

    bool IsFunction() const { return !ill_typed_ && !simple_bool_; }

    Type& FunctionLHS() const {
        if (!IsFunction()) {
            throw std::invalid_argument("Invalid function type.");
        }

        return *lhs_;
    }

    Type& FunctionRHS() const {
        if (!IsFunction()) {
            throw std::invalid_argument("Invalid function type.");
        }

        return *rhs_;
    }

   private:
    bool ill_typed_ = false;

    bool simple_bool_ = false;

    std::unique_ptr<Type> lhs_;
    std::unique_ptr<Type> rhs_;
};

std::ostream& operator<<(std::ostream& out, const Type& type) {
    if (type.IsSimpleBool()) {
        out << lexer::kKeywordBool;
    } else if (type.IsFunction()) {
        out << "(" << *type.lhs_ << " "
            << lexer::Token(lexer::Token::Category::ARROW) << " " << *type.rhs_
            << ")";
    } else {
        out << "Ⱦ";
    }

    return out;
}

class Term {
    friend std::ostream& operator<<(std::ostream&, const Term&);

   public:
    static Term Lambda(std::string arg_name, std::unique_ptr<Type> arg_type) {
        Term result;
        result.lambda_arg_name_ = arg_name;
        result.lambda_arg_type_ = std::move(arg_type);
        result.is_lambda_ = true;

        return result;
    }

    static Term Variable(std::string var_name, int de_bruijn_idx) {
        Term result;
        result.variable_name_ = var_name;
        result.de_bruijn_idx_ = de_bruijn_idx;
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

    static Term If() {
        Term result;
        result.is_if_ = true;

        return result;
    }

    static Term True() {
        Term result;
        result.is_true_ = true;

        return result;
    }

    static Term False() {
        Term result;
        result.is_false_ = true;

        return result;
    }

    Term() = default;

    Term(const Term&) = delete;
    Term& operator=(const Term&) = delete;

    Term(Term&&) = default;
    Term& operator=(Term&&) = default;

    ~Term() = default;

    bool IsLambda() const { return is_lambda_; }

    void MarkLambdaAsComplete() { is_complete_lambda_ = true; }

    bool IsVariable() const { return is_variable_; }

    bool IsApplication() const { return is_application_; }

    bool IsIf() const { return is_if_; }

    bool IsTrue() const { return is_true_; }

    bool IsFalse() const { return is_false_; }

    void MarkIfConditionAsComplete() { is_complete_if_condition_ = true; }

    void MarkIfThenAsComplete() { is_complete_if_then_ = true; }

    void MarkIfElseAsComplete() { is_complete_if_else_ = true; }

    bool IsInvalid() const {
        if (IsLambda()) {
            return lambda_arg_name_.empty() || !lambda_arg_type_ ||
                   !lambda_body_;
        } else if (IsVariable()) {
            return variable_name_.empty();
        } else if (IsApplication()) {
            return !application_lhs_ || !application_rhs_;
        } else if (IsIf()) {
            return !if_condition_ || !if_then_ || !if_else_;
        } else if (IsTrue() || IsFalse()) {
            return false;
        }

        return true;
    }

    bool IsEmpty() const {
        return !IsLambda() && !IsVariable() && !IsApplication() && !IsIf();
    }

    Term& Combine(Term&& term) {
        if (term.IsInvalid()) {
            throw std::invalid_argument(
                "Term::Combine() received an invalid Term.");
        }

        if (IsLambda()) {
            if (lambda_body_) {
                // If the lambda body was completely parsed, then combining this
                // term and the argument term means applying this lambda to the
                // argument.
                if (is_complete_lambda_) {
                    *this =
                        Application(std::make_unique<Term>(std::move(*this)),
                                    std::make_unique<Term>(std::move(term)));

                    is_lambda_ = false;
                    lambda_body_ = nullptr;
                    lambda_arg_name_ = "";
                    is_complete_lambda_ = false;
                } else {
                    lambda_body_->Combine(std::move(term));
                }
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
        } else if (IsIf()) {
            if (!is_complete_if_condition_) {
                if (if_condition_) {
                    if_condition_->Combine(std::move(term));
                } else {
                    if_condition_ = std::make_unique<Term>(std::move(term));
                }
            } else if (!is_complete_if_then_) {
                if (if_then_) {
                    if_then_->Combine(std::move(term));
                } else {
                    if_then_ = std::make_unique<Term>(std::move(term));
                }
            } else {
                if (if_else_) {
                    // If the lambda body was completely parsed, then combining
                    // this term and the argument term means applying this
                    // lambda to the argument.
                    if (is_complete_if_else_) {
                        *this = Application(
                            std::make_unique<Term>(std::move(*this)),
                            std::make_unique<Term>(std::move(term)));

                        is_if_ = false;
                        if_condition_ = nullptr;
                        if_then_ = nullptr;
                        if_else_ = nullptr;
                        is_complete_if_condition_ = false;
                        is_complete_if_then_ = false;
                        is_complete_if_else_ = false;
                    } else {
                        if_else_->Combine(std::move(term));
                    }
                } else {
                    if_else_ = std::make_unique<Term>(std::move(term));
                }
            }
        } else if (IsTrue() || IsFalse()) {
            throw std::invalid_argument("Trying to combine with a constant.");
        } else {
            *this = std::move(term);
        }

        return *this;
    }

    /*
     * Shifts the de Bruijn indices of all free variables inside this Term up by
     * distance amount. For an example use, see Term::Substitute(int, Term&).
     */
    void Shift(int distance) {
        std::function<void(int, Term&)> walk = [&distance, &walk](
                                                   int binding_context_size,
                                                   Term& term) {
            if (term.IsInvalid()) {
                throw std::invalid_argument("Trying to shift an invalid term.");
            }

            if (term.IsVariable()) {
                if (term.de_bruijn_idx_ >= binding_context_size) {
                    term.de_bruijn_idx_ += distance;
                }
            } else if (term.IsLambda()) {
                walk(binding_context_size + 1, *term.lambda_body_);
            } else if (term.IsApplication()) {
                walk(binding_context_size, *term.application_lhs_);
                walk(binding_context_size, *term.application_rhs_);
            }
        };

        walk(0, *this);
    }

    /**
     * Substitutes variable (that is, the de Brijun idex of a variable) with the
     * term sub.
     */
    void Substitute(int variable, Term& sub) {
        if (IsInvalid() || sub.IsInvalid()) {
            throw std::invalid_argument(
                "Trying to substitute using invalid terms.");
        }

        std::function<void(int, Term&)> walk = [&variable, &sub, &walk](
                                                   int binding_context_size,
                                                   Term& term) {
            if (term.IsVariable()) {
                // Adjust variable according to the current binding
                // depth before comparing term's index.
                if (term.de_bruijn_idx_ == variable + binding_context_size) {
                    // Shift sub up by binding_context_size distance since sub
                    // is now substituted in binding_context_size deep context.
                    auto clone = sub.Clone();
                    clone.Shift(binding_context_size);
                    std::swap(term, clone);
                }
            } else if (term.IsLambda()) {
                walk(binding_context_size + 1, *term.lambda_body_);
            } else if (term.IsApplication()) {
                walk(binding_context_size, *term.application_lhs_);
                walk(binding_context_size, *term.application_rhs_);
            } else if (term.IsIf()) {
                walk(binding_context_size, *term.if_condition_);
                walk(binding_context_size, *term.if_then_);
                walk(binding_context_size, *term.if_else_);
            }
        };

        walk(0, *this);
    }

    Term& LambdaBody() const {
        if (!IsLambda()) {
            throw std::invalid_argument("Invalid Lambda term.");
        }

        return *lambda_body_;
    }

    std::string LambdaArgName() const {
        if (!IsLambda()) {
            throw std::invalid_argument("Invalid Lambda term.");
        }

        return lambda_arg_name_;
    }

    Type& LambdaArgType() const {
        if (!IsLambda()) {
            throw std::invalid_argument("Invalid Lambda term.");
        }

        return *lambda_arg_type_;
    }

    std::string VariableName() const {
        if (!IsVariable()) {
            throw std::invalid_argument("Invalid variable term.");
        }

        return variable_name_;
    }

    int VariableDeBruijnIdx() const {
        if (!IsVariable()) {
            throw std::invalid_argument("Invalid variable term.");
        }

        return de_bruijn_idx_;
    }

    Term& ApplicationLHS() const {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalid application term.");
        }

        return *application_lhs_;
    }

    Term& ApplicationRHS() const {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalid application term.");
        }

        return *application_rhs_;
    }

    Term& IfCondition() const {
        if (!IsIf()) {
            throw std::invalid_argument("Invalid if term.");
        }

        return *if_condition_;
    }

    Term& IfThen() const {
        if (!IsIf()) {
            throw std::invalid_argument("Invalid if term.");
        }

        return *if_then_;
    }

    Term& IfElse() const {
        if (!IsIf()) {
            throw std::invalid_argument("Invalid if term.");
        }

        return *if_else_;
    }

    bool operator==(const Term& other) const {
        if (IsLambda() && other.IsLambda()) {
            return LambdaArgType() == other.LambdaArgType() &&
                   LambdaBody() == other.LambdaBody();
        }

        if (IsVariable() && other.IsVariable()) {
            return de_bruijn_idx_ == other.de_bruijn_idx_;
        }

        if (IsApplication() && other.IsApplication()) {
            return (ApplicationLHS() == other.ApplicationLHS()) &&
                   (ApplicationRHS() == other.ApplicationRHS());
        }

        if (IsIf() && other.IsIf()) {
            return (IfCondition() == other.IfCondition()) &&
                   (IfThen() == other.IfThen()) && (IfElse() == other.IfElse());
        }

        if (IsTrue() && other.IsTrue()) {
            return true;
        }

        if (IsFalse() && other.IsFalse()) {
            return true;
        }

        return false;
    }

    bool operator!=(const Term& other) const { return !(*this == other); }

    std::string ASTString(int indentation = 0) const {
        std::ostringstream out;
        std::string prefix = std::string(indentation, '-');

        if (IsLambda()) {
            out << prefix << "λ " << lambda_arg_name_ << ":"
                << *lambda_arg_type_ << "\n";
            out << lambda_body_->ASTString(indentation + 2);
        } else if (IsVariable()) {
            out << prefix << variable_name_ << "[" << de_bruijn_idx_ << "]";
        } else if (IsApplication()) {
            out << prefix << "<-\n";
            out << application_lhs_->ASTString(indentation + 2) << "\n";
            out << application_rhs_->ASTString(indentation + 2);
        } else if (IsIf()) {
            out << prefix << "if\n";
            out << if_condition_->ASTString(indentation + 2) << "\n";
            out << prefix << "then\n";
            out << if_then_->ASTString(indentation + 2) << "\n";
            out << prefix << "else\n";
            out << if_else_->ASTString(indentation + 2);
        } else if (IsTrue()) {
            out << prefix << "true";
        } else if (IsFalse()) {
            out << prefix << "false";
        }

        return out.str();
    }

    Term Clone() const {
        if (IsInvalid()) {
            throw std::logic_error("Trying to clone an invalid term.");
        }

        if (IsLambda()) {
            return std::move(
                Lambda(lambda_arg_name_,
                       std::make_unique<Type>(lambda_arg_type_->Clone()))
                    .Combine(lambda_body_->Clone()));
        } else if (IsVariable()) {
            return Variable(variable_name_, de_bruijn_idx_);
        } else if (IsApplication()) {
            return Application(
                std::make_unique<Term>(application_lhs_->Clone()),
                std::make_unique<Term>(application_rhs_->Clone()));
        } else if (IsTrue()) {
            return Term::True();
        } else if (IsFalse()) {
            return Term::False();
        }

        std::ostringstream error_ss;
        error_ss << "Couldn't clone term: " << *this;
        throw std::logic_error(error_ss.str());
    }

    bool is_complete_lambda_ = false;

   private:
    bool is_lambda_ = false;
    std::string lambda_arg_name_ = "";
    std::unique_ptr<Type> lambda_arg_type_{};
    std::unique_ptr<Term> lambda_body_{};
    // Marks whether parsing for the body of the lambda term is finished or not.

    bool is_variable_ = false;
    std::string variable_name_ = "";
    int de_bruijn_idx_ = -1;

    bool is_application_ = false;
    std::unique_ptr<Term> application_lhs_{};
    std::unique_ptr<Term> application_rhs_{};

    bool is_if_ = false;
    std::unique_ptr<Term> if_condition_{};
    std::unique_ptr<Term> if_then_{};
    std::unique_ptr<Term> if_else_{};
    bool is_complete_if_condition_ = false;
    bool is_complete_if_then_ = false;
    bool is_complete_if_else_ = false;

    bool is_true_ = false;

    bool is_false_ = false;
};

std::ostream& operator<<(std::ostream& out, const Term& term) {
    if (term.IsInvalid()) {
        out << "<INVALID>";
    } else if (term.IsVariable()) {
        out << term.variable_name_;
    } else if (term.IsLambda()) {
        out << "{l " << term.lambda_arg_name_ << " : " << *term.lambda_arg_type_
            << ". " << *term.lambda_body_ << "}";
    } else if (term.IsApplication()) {
        out << "(" << *term.application_lhs_ << " <- " << *term.application_rhs_
            << ")";
    } else if (term.IsIf()) {
        out << "if (" << *term.if_condition_ << ") then (" << *term.if_then_
            << ") else (" << *term.if_else_ << ")";
    } else if (term.IsTrue()) {
        out << "true";
    } else if (term.IsFalse()) {
        out << "false";
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
        std::vector<Term> term_stack;
        term_stack.emplace_back(Term());
        int balance_parens = 0;
        // For each '(', records the size of term_stack when the '(' was parsed.
        // This is used later when the corresponding ')' is parsed to know how
        // many Terms from term_stack should be popped (i.e. their parsing is
        // know to be complete).
        std::vector<int> stack_size_on_open_paren;
        // Contains a list of bound variables in order of binding. For example,
        // for a term λ x. λ y. x y, this list would eventually contains {"x" ,
        // "y"} in that order. This is used to assign de Bruijn indices/static
        // distances to bound variables (ref: tapl,§6.1).
        std::vector<std::string> bound_variables;

        while ((next_token = lexer_.NextToken()).GetCategory() !=
               Token::Category::MARKER_END) {
            if (next_token.GetCategory() == Token::Category::LAMBDA) {
                auto lambda_arg = ParseLambdaArg();
                auto lambda_arg_name = lambda_arg.first;
                bound_variables.push_back(lambda_arg_name);

                // If the current stack top is empty, use its slot for the
                // lambda.
                if (term_stack.back().IsEmpty()) {
                    term_stack.back() = Term::Lambda(
                        lambda_arg_name,
                        std::make_unique<Type>(std::move(lambda_arg.second)));
                } else {
                    // Else, push a new term on the stack to start building the
                    // lambda term.
                    term_stack.emplace_back(Term::Lambda(
                        lambda_arg_name,
                        std::make_unique<Type>(std::move(lambda_arg.second))));
                }
            } else if (next_token.GetCategory() == Token::Category::VARIABLE) {
                auto bound_variable_it =
                    std::find(std::begin(bound_variables),
                              std::end(bound_variables), next_token.GetText());
                int de_bruijn_idx = -1;

                if (bound_variable_it != std::end(bound_variables)) {
                    de_bruijn_idx = std::distance(bound_variable_it,
                                                  std::end(bound_variables)) -
                                    1;
                } else {
                    // The naming context for free variables (ref: tapl,§6.1.2)
                    // is chosen to be the ASCII code of a variable's name.
                    //
                    // NOTE: Only single-character variable names are currecntly
                    // supported as free variables.
                    if (next_token.GetText().length() != 1) {
                        std::ostringstream error_ss;
                        error_ss << "Unexpected token: " << next_token;
                        throw std::invalid_argument(error_ss.str());
                    }

                    de_bruijn_idx =
                        bound_variables.size() +
                        (std::tolower(next_token.GetText()[0]) - 'a');
                }

                term_stack.back().Combine(
                    Term::Variable(next_token.GetText(), de_bruijn_idx));
            } else if (next_token.GetCategory() ==
                       Token::Category::KEYWORD_IF) {
                // If the current stack top is empty, use its slot for the
                // if condition.
                if (term_stack.back().IsEmpty()) {
                    term_stack.back() = Term::If();
                } else {
                    // Else, push a new term on the stack to start building the
                    // if condition.
                    term_stack.emplace_back(Term::If());
                }

                stack_size_on_open_paren.emplace_back(term_stack.size());
                term_stack.emplace_back(Term());
                ++balance_parens;
            } else if (next_token.GetCategory() ==
                       Token::Category::KEYWORD_THEN) {
                UnwindStack(term_stack, stack_size_on_open_paren,
                            bound_variables);

                --balance_parens;

                if (!term_stack.back().IsIf()) {
                    throw std::invalid_argument("Unexpected 'then'");
                }

                term_stack.back().MarkIfConditionAsComplete();

                stack_size_on_open_paren.emplace_back(term_stack.size());
                term_stack.emplace_back(Term());
                ++balance_parens;
            } else if (next_token.GetCategory() ==
                       Token::Category::KEYWORD_ELSE) {
                UnwindStack(term_stack, stack_size_on_open_paren,
                            bound_variables);

                --balance_parens;

                if (!term_stack.back().IsIf()) {
                    throw std::invalid_argument("Unexpected 'else'");
                }

                term_stack.back().MarkIfThenAsComplete();
            } else if (next_token.GetCategory() ==
                       Token::Category::OPEN_PAREN) {
                stack_size_on_open_paren.emplace_back(term_stack.size());
                term_stack.emplace_back(Term());
                ++balance_parens;
            } else if (next_token.GetCategory() ==
                       Token::Category::CLOSE_PAREN) {
                UnwindStack(term_stack, stack_size_on_open_paren,
                            bound_variables);

                --balance_parens;
            } else if (next_token.GetCategory() ==
                       Token::Category::CONSTANT_TRUE) {
                term_stack.back().Combine(Term::True());

            } else if (next_token.GetCategory() ==
                       Token::Category::CONSTANT_FALSE) {
                term_stack.back().Combine(Term::False());
            } else {
                std::ostringstream error_ss;
                error_ss << "Unexpected token: " << next_token;
                throw std::invalid_argument(error_ss.str());
            }
        }

        if (balance_parens != 0) {
            throw std::invalid_argument(
                "Invalid term: probably because a ( is not matched by a )");
        }

        while (term_stack.size() > 1) {
            CombineStackTop(term_stack);
        }

        if (term_stack.back().IsInvalid()) {
            throw std::invalid_argument("Invalid term.");
        }

        return std::move(term_stack.back());
    }

    void UnwindStack(std::vector<Term>& term_stack,
                     std::vector<int>& stack_size_on_open_paren,
                     std::vector<std::string>& bound_variables) {
        while (!term_stack.empty() && !stack_size_on_open_paren.empty() &&
               term_stack.size() > stack_size_on_open_paren.back()) {
            if (term_stack.back().IsLambda() &&
                !term_stack.back().is_complete_lambda_) {
                // Mark the λ as complete so that terms to its right
                // won't be combined to its body.
                term_stack.back().MarkLambdaAsComplete();
                // λ's variable is no longer part of the current binding
                // context, therefore pop it.
                bound_variables.pop_back();
            }

            if (term_stack.back().IsIf()) {
                term_stack.back().MarkIfElseAsComplete();
            }

            CombineStackTop(term_stack);
        }

        if (!stack_size_on_open_paren.empty()) {
            stack_size_on_open_paren.pop_back();
        }
    }

    void CombineStackTop(std::vector<Term>& term_stack) {
        if (term_stack.size() < 2) {
            throw std::invalid_argument(
                "Invalid term: probably because a ( is not matched by a )");
        }

        Term top = std::move(term_stack.back());
        term_stack.pop_back();
        term_stack.back().Combine(std::move(top));
    }

    std::pair<std::string, Type> ParseLambdaArg() {
        auto token = lexer_.NextToken();

        if (token.GetCategory() != Token::Category::VARIABLE) {
            throw std::logic_error("Expected to parse a variable.");
        }

        auto arg_name = token.GetText();
        token = lexer_.NextToken();

        if (token.GetCategory() != Token::Category::COLON) {
            throw std::logic_error("Expected to parse a ':'.");
        }

        return {arg_name, std::move(ParseType())};
    }

    Type ParseType() {
        std::vector<Type> parts;
        while (true) {
            auto token = lexer_.NextToken();

            if (token.GetCategory() == Token::Category::KEYWORD_BOOL) {
                parts.emplace_back(Type::SimpleBool());
            } else if (token.GetCategory() == Token::Category::OPEN_PAREN) {
                parts.emplace_back(ParseType());

                if (lexer_.NextToken().GetCategory() !=
                    Token::Category::CLOSE_PAREN) {
                    std::ostringstream error_ss;
                    error_ss << __LINE__ << ": Unexpected token: " << token;
                    throw std::logic_error(error_ss.str());
                }
            } else {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::logic_error(error_ss.str());
            }

            token = lexer_.NextToken();

            if (token.GetCategory() == Token::Category::LAMBDA_DOT) {
                break;
            } else if (token.GetCategory() == Token::Category::CLOSE_PAREN) {
                lexer_.PutBackToken();
                break;
            } else if (token.GetCategory() != Token::Category::ARROW) {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::logic_error(error_ss.str());
            }
        }

        for (int i = parts.size() - 2; i >= 0; --i) {
            parts[i] = Type::FunctionType(
                std::make_unique<Type>(std::move(parts[i])),
                std::make_unique<Type>(std::move(parts[i + 1])));
        }

        return std::move(parts[0]);
    }

    Token ParseDot() {
        auto token = lexer_.NextToken();

        return (token.GetCategory() == Token::Category::LAMBDA_DOT)
                   ? token
                   : throw std::logic_error("Expected to parse a dot.");
    }

   private:
    lexer::Lexer lexer_;
};  // namespace parser
}  // namespace parser

namespace type_checker {
using parser::Term;
using parser::Type;

class TypeChecker {
    using Context = std::deque<std::pair<std::string, Type*>>;

   public:
    Type TypeOf(const Term& term) {
        Context ctx;
        return TypeOf(ctx, term);
    }

    Type TypeOf(const Context& ctx, const Term& term) {
        Type res = Type::IllTyped();

        if (term.IsTrue() || term.IsFalse()) {
            res = Type::SimpleBool();
        } else if (term.IsIf()) {
            if (TypeOf(ctx, term.IfCondition()) == Type::SimpleBool()) {
                Type then_type = TypeOf(ctx, term.IfThen());

                if (then_type == TypeOf(ctx, term.IfElse())) {
                    res = then_type.Clone();
                }
            }
        } else if (term.IsLambda()) {
            Context new_ctx =
                AddBinding(ctx, term.LambdaArgName(), term.LambdaArgType());
            Type return_type = TypeOf(new_ctx, term.LambdaBody());
            res = Type::FunctionType(
                std::make_unique<Type>(term.LambdaArgType().Clone()),
                std::make_unique<Type>(return_type.Clone()));
        } else if (term.IsApplication()) {
            Type lhs_type = TypeOf(ctx, term.ApplicationLHS());
            Type rhs_type = TypeOf(ctx, term.ApplicationRHS());

            if (lhs_type.IsFunction() && lhs_type.FunctionLHS() == rhs_type) {
                res = lhs_type.FunctionRHS().Clone();
            }
        } else if (term.IsVariable()) {
            int idx = term.VariableDeBruijnIdx();

            if (idx >= 0 && idx < ctx.size() &&
                ctx[idx].first == term.VariableName()) {
                res = ctx[idx].second->Clone();
            }
        }

        return res;
    }

   private:
    Context AddBinding(const Context& current_ctx, std::string var_name,
                       Type& type) {
        Context new_ctx = current_ctx;
        new_ctx.push_front({var_name, &type});

        return new_ctx;
    }
};
}  // namespace type_checker

namespace interpreter {
class Interpreter {
    using Term = parser::Term;

   public:
    std::pair<std::string, type_checker::Type> Interpret(Term& program) {
        Eval(program);
        type_checker::Type type = type_checker::TypeChecker().TypeOf(program);

        std::ostringstream ss;
        ss << program;

        return {ss.str(), std::move(type)};
    }

   private:
    void Eval(Term& term) {
        try {
            Eval1(term);
            Eval(term);
        } catch (std::invalid_argument&) {
        }
    }

    void Eval1(Term& term) {
        auto term_subst_top = [](Term& s, Term& t) {
            // Adjust the free variables in s by increasing their static
            // distances by 1. That's because s will now be embedded one level
            // deeper in t (i.e. t's bound variable will be replaced by s).
            s.Shift(1);
            t.Substitute(0, s);
            // Because of the substitution, one level of abstraction was peeled
            // off. Account for that by decreasing the static distances of the
            // free variables in t by 1.
            t.Shift(-1);
            // NOTE: For more details see: tapl,§6.3.
        };

        if (term.IsApplication() && term.ApplicationLHS().IsLambda() &&
            IsValue(term.ApplicationRHS())) {
            term_subst_top(term.ApplicationRHS(),
                           term.ApplicationLHS().LambdaBody());
            std::swap(term, term.ApplicationLHS().LambdaBody());
        } else if (term.IsApplication() && IsValue(term.ApplicationLHS())) {
            Eval1(term.ApplicationRHS());
        } else if (term.IsApplication()) {
            Eval1(term.ApplicationLHS());
        } else if (term.IsIf()) {
            if (term.IfCondition() == Term::True()) {
                std::swap(term, term.IfThen());
            } else if (term.IfCondition() == Term::False()) {
                std::swap(term, term.IfElse());
            } else {
                Eval1(term.IfCondition());
            }
        } else {
            throw std::invalid_argument("No applicable rule.");
        }
    }

    bool IsValue(const Term& term) {
        return term.IsLambda() || term.IsVariable() || term.IsTrue() ||
               term.IsFalse();
    }
};
}  // namespace interpreter
