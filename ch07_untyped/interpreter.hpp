#include <functional>
#include <memory>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <vector>

namespace lexer {
class Token {
   public:
    enum class Category {
        VARIABLE,
        LAMBDA,
        LAMBDA_DOT,
        OPEN_PAREN,
        CLOSE_PAREN,
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

class Lexer {
    static const std::string kLambdaInputSymbol;

   public:
    Lexer(std::istringstream&& in) : in_(std::move(in)) {}

    // TODO I think this can be significantly simplified by:
    //
    // 1. Adding a pre-processing step that inserts a space before and after
    // separators (., (, and )).
    //
    // 2. Separating the code of reading the next token from the input character
    // buffer from the code of managing the output token buffer.
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

        Token::Category token_category;
        auto token_text = token_text_out.str();

        if (token_text == kLambdaInputSymbol) {
            token = Token(Token::Category::LAMBDA);
        } else if (IsVariableName(token_text)) {
            token = Token(Token::Category::VARIABLE, token_text);
        } else if (next_char == '(') {
            // There is no token before next_char, then create a token of it and
            // clear next_char.
            token = Token(Token::Category::OPEN_PAREN);
            next_char = 0;
        } else if (next_char == ')') {
            // There is no token before next_char, then create a token of it and
            // clear next_char.
            token = Token(Token::Category::CLOSE_PAREN);
            next_char = 0;
        } else if (next_char == '.') {
            // There is no token before next_char, then create a token of it and
            // clear next_char.
            token = Token(Token::Category::LAMBDA_DOT);
            next_char = 0;
        } else if (!token_text.empty()) {
            token = Token(Token::Category::MARKER_INVALID);
        } else if (!in_) {
            token = Token(Token::Category::MARKER_END);
        } else {
            // Must be whitespace, eat it.
            token = NextToken();
        }

        // There is a token before next_char, then return that token and cache
        // next_char's token for next call to NextToken().
        if (next_char == '.') {
            is_cached_token_valid = true;
            cached_token = Token(Token::Category::LAMBDA_DOT);
        } else if (next_char == '(') {
            is_cached_token_valid = true;
            cached_token = Token(Token::Category::OPEN_PAREN);
        } else if (next_char == ')') {
            is_cached_token_valid = true;
            cached_token = Token(Token::Category::CLOSE_PAREN);
        }

        return token;
    }

   private:
    bool IsSeparator(char c) const {
        return std::string{" .()"}.find(c) != std::string::npos;
    }

    bool IsVariableName(const std::string& text) const {
        if (text.empty()) {
            return false;
        }

        for (char c : text) {
            if (!std::islower(c) && !std::isupper(c) && (c != '_')) {
                return false;
            }
        }

        return true;
    }

   private:
    std::istringstream in_;
    bool is_cached_token_valid = false;
    Token cached_token;
};

const std::string Lexer::kLambdaInputSymbol = "l";

std::ostream& operator<<(std::ostream& out, Token token) {
    switch (token.GetCategory()) {
        case Token::Category::LAMBDA:
            out << "lambda";
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

    bool IsEmpty() const {
        return !IsLambda() && !IsVariable() && !IsApplication();
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
            if (term.IsVariable()) {
                if (term.de_bruijn_idx_ >= binding_context_size) {
                    term.de_bruijn_idx_ += distance;
                }
            } else if (term.IsLambda()) {
                walk(binding_context_size + 1, *term.lambda_body_);
            } else if (term.IsApplication()) {
                walk(binding_context_size, *term.application_lhs_);
                walk(binding_context_size, *term.application_rhs_);
            } else {
                throw std::invalid_argument("Trying to shift an invalid term.");
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

    Term& ApplicationLHS() const {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalide application term.");
        }

        return *application_lhs_;
    }

    Term& ApplicationRHS() const {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalide application term.");
        }

        return *application_rhs_;
    }

    bool operator==(const Term& other) const {
        if (IsLambda() && other.IsLambda()) {
            return LambdaBody() == other.LambdaBody();
        }

        if (IsVariable() && other.IsVariable()) {
            return de_bruijn_idx_ == other.de_bruijn_idx_;
        }

        if (IsApplication() && other.IsApplication()) {
            return (ApplicationLHS() == other.ApplicationLHS()) &&
                   (ApplicationRHS() == other.ApplicationRHS());
        }

        return false;
    }

    bool operator!=(const Term& other) const { return !(*this == other); }

    std::string ASTString(int indentation = 0) const {
        std::ostringstream out;
        std::string prefix = std::string(indentation, ' ');

        if (IsLambda()) {
            out << prefix << "λ " << lambda_arg_name_ << "\n";
            out << lambda_body_->ASTString(indentation + 2);
        } else if (IsVariable()) {
            out << prefix << variable_name_ << "[" << de_bruijn_idx_ << "]";
        } else if (IsApplication()) {
            out << prefix << "<-\n";
            out << application_lhs_->ASTString(indentation + 2) << "\n";
            out << application_rhs_->ASTString(indentation + 2);
        }

        return out.str();
    }

    Term Clone() const {
        if (IsInvalid()) {
            throw std::logic_error("Trying to clone an invalid term.");
        }

        if (IsLambda()) {
            return std::move(
                Lambda(lambda_arg_name_).Combine(lambda_body_->Clone()));
        } else if (IsVariable()) {
            return Variable(variable_name_, de_bruijn_idx_);
        } else if (IsApplication()) {
            return Application(
                std::make_unique<Term>(application_lhs_->Clone()),
                std::make_unique<Term>(application_rhs_->Clone()));
        }

        std::ostringstream error_ss;
        error_ss << "Couldn't clone term: " << *this;
        throw std::logic_error(error_ss.str());
    }

   private:
    bool is_lambda_ = false;
    std::string lambda_arg_name_ = "";
    std::unique_ptr<Term> lambda_body_{};
    // Marks whether parsing for the body of the lambda term is finished or not.
    bool is_complete_lambda_ = false;

    bool is_variable_ = false;
    std::string variable_name_ = "";
    int de_bruijn_idx_ = -1;

    bool is_application_ = false;
    std::unique_ptr<Term> application_lhs_{};
    std::unique_ptr<Term> application_rhs_{};
};

std::ostream& operator<<(std::ostream& out, const Term& term) {
    if (term.IsInvalid()) {
        out << "<INVALID>";
    } else if (term.IsVariable()) {
        out << "[" << term.variable_name_ << "=" << term.de_bruijn_idx_ << "]";
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
                auto lambda_arg = ParseVariable();
                bound_variables.push_back(lambda_arg.GetText());
                ParseDot();

                // If the current stack top is empty, use its slot for the
                // lambda.
                if (term_stack.back().IsEmpty()) {
                    term_stack.back() = Term::Lambda(lambda_arg.GetText());
                } else {
                    // Else, push a new term on the stack to start building the
                    // lambda term.
                    term_stack.emplace_back(Term::Lambda(lambda_arg.GetText()));
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
                       Token::Category::OPEN_PAREN) {
                stack_size_on_open_paren.emplace_back(term_stack.size());
                term_stack.emplace_back(Term());
                ++balance_parens;
            } else if (next_token.GetCategory() ==
                       Token::Category::CLOSE_PAREN) {
                while (!term_stack.empty() &&
                       !stack_size_on_open_paren.empty() &&
                       term_stack.size() > stack_size_on_open_paren.back()) {
                    if (term_stack.back().IsLambda()) {
                        // Mark the λ as complete so that terms to its right
                        // won't be combined to its body.
                        term_stack.back().MarkLambdaAsComplete();
                        // λ's variable is no longer part of the current binding
                        // context, therefore pop it.
                        bound_variables.pop_back();
                    }

                    CombineStackTop(term_stack);
                }

                --balance_parens;

                if (!stack_size_on_open_paren.empty()) {
                    stack_size_on_open_paren.pop_back();
                }
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

    void CombineStackTop(std::vector<Term>& term_stack) {
        if (term_stack.size() < 2) {
            throw std::invalid_argument(
                "Invalid term: probably because a ( is not matched by a )");
        }

        Term top = std::move(term_stack.back());
        term_stack.pop_back();
        term_stack.back().Combine(std::move(top));
    }

    Token ParseVariable() {
        auto token = lexer_.NextToken();

        return (token.GetCategory() == Token::Category::VARIABLE)
                   ? token
                   : throw std::logic_error("Expected to parse a variable.");
    }

    Token ParseDot() {
        auto token = lexer_.NextToken();

        return (token.GetCategory() == Token::Category::LAMBDA_DOT)
                   ? token
                   : throw std::logic_error("Expected to parse a dot.");
    }

   private:
    lexer::Lexer lexer_;
};
}  // namespace parser

namespace interpreter {
class Interpreter {
    using Term = parser::Term;

   public:
    void Interpret(Term& program) { Eval(program); }

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
        } else {
            throw std::invalid_argument("No applicable rule.");
        }
    }

    bool IsValue(const Term& term) {
        return term.IsLambda() || term.IsVariable();
    }
};
}  // namespace interpreter
