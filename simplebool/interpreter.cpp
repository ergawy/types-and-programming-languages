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

    Category category = Category::MARKER_INVALID;
    std::string text;
};

std::ostream& operator<<(std::ostream& out, Token token);

class Lexer {
    static const std::string kLambdaInputSymbol;
    static const std::string kKeywordBool;

   public:
    Lexer(std::istringstream&& in) : in_(std::move(in)) {}

    Token NextToken() {
        if (is_cached_token_valid) {
            if (cached_token.category == Token::Category::MARKER_INVALID) {
                throw std::invalid_argument("Error: invalid token: " +
                                            cached_token.text);
            }

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

        if (token_text == "true") {
            token.category = Token::Category::CONSTANT_TRUE;
        } else if (token_text == "false") {
            token.category = Token::Category::CONSTANT_FALSE;
        } else if (token_text == "if") {
            token.category = Token::Category::KEYWORD_IF;
        } else if (token_text == "then") {
            token.category = Token::Category::KEYWORD_THEN;
        } else if (token_text == "else") {
            token.category = Token::Category::KEYWORD_ELSE;
        } else if (token_text == kLambdaInputSymbol) {
            token.category = Token::Category::LAMBDA;
        } else if (token_text == kKeywordBool) {
            token.category = Token::Category::KEYWORD_BOOL;
        } else if (!token_text.empty()) {
            token.category = Token::Category::VARIABLE;
            token.text = token_text;
        } else if (next_char == '(') {
            // There is no token before next_char, then create a token of it
            // and clear next_char.
            token.category = Token::Category::OPEN_PAREN;
            next_char = 0;
        } else if (next_char == ')') {
            // There is no token before next_char, then create a token of it
            // and clear next_char.
            token.category = Token::Category::CLOSE_PAREN;
            next_char = 0;
        } else if (next_char == '.') {
            // There is no token before next_char, then create a token of it
            // and clear next_char.
            token.category = Token::Category::LAMBDA_DOT;
            next_char = 0;
        } else if (next_char == ':') {
            // There is no token before next_char, then create a token of it
            // and clear next_char.
            token.category = Token::Category::COLON;
            next_char = 0;
        } else if (next_char == '-') {
            if (in_.get(next_char) && next_char == '>') {
                token.category = Token::Category::ARROW;
            } else {
                token.text = '-';
                token.category = Token::Category::MARKER_INVALID;
            }

            next_char = 0;
        } else if (!in_) {
            token.category = Token::Category::MARKER_END;
        } else {
            // Must be whitespace, eat it.
            token = NextToken();
        }

        // There is a token before next_char, then return that token and
        // cache next_char's token for next call to NextToken().
        if (next_char == '.') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::LAMBDA_DOT;
        } else if (next_char == '(') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::OPEN_PAREN;
        } else if (next_char == ')') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::CLOSE_PAREN;
        } else if (next_char == ':') {
            is_cached_token_valid = true;
            cached_token.category = Token::Category::COLON;
        } else if (next_char == '-') {
            is_cached_token_valid = true;

            if (in_.get(next_char) && next_char == '>') {
                cached_token.category = Token::Category::ARROW;
            } else {
                cached_token.text = '-';
                cached_token.category = Token::Category::MARKER_INVALID;
            }
        }

        if (token.category == Token::Category::MARKER_INVALID) {
            throw std::invalid_argument("Error: invalid token: " + token.text);
        }

        return token;
    }

   private:
    bool IsSeparator(char c) {
        return std::string{" .():-"}.find(c) != std::string::npos;
    }

   private:
    std::istringstream in_;
    bool is_cached_token_valid = false;
    Token cached_token;
};

const std::string Lexer::kLambdaInputSymbol = "l";
const std::string Lexer::kKeywordBool = "Bool";

std::ostream& operator<<(std::ostream& out, Token token) {
    switch (token.category) {
        case Token::Category::LAMBDA:
            out << "λ";
            break;
        case Token::Category::KEYWORD_BOOL:
            out << "<Bool>";
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

namespace parser {
struct Binding {
    std::string name;
    std::vector<lexer::Token> type;
};

class Term {
    friend std::ostream& operator<<(std::ostream&, const Term&);

   public:
    static Term Lambda(Binding binding) {
        Term result;
        result.lambda_arg_name_ = binding.name;
        result.is_lambda_ = true;
        result.lambda_type_ = binding.type;

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

    static Term ConstantTrue() {
        Term result;
        result.is_true_constant_ = true;

        return result;
    }

    static Term ConstantFalse() {
        Term result;
        result.is_false_constant_ = true;

        return result;
    }

    static Term Conditional() {
        Term result;
        result.is_conditional_ = true;

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

    bool IsBoolConst() const { return is_true_constant_ || is_false_constant_; }

    bool IsConditional() const { return is_conditional_; }

    bool IsInvalid() const {
        // NOTE: Should this be recursive instead of simply checking the
        // pointers contents?
        if (IsLambda()) {
            return lambda_arg_name_.empty() || !lambda_body_;
        } else if (IsVariable()) {
            return variable_name_.empty();
        } else if (IsApplication()) {
            return !application_lhs_ || !application_rhs_;
        } else if (IsBoolConst()) {
            return false;
        } else if (IsConditional()) {
            return !conditional_cond_ || !conditional_then_branch_ ||
                   !conditional_else_branch_;
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
                // If this is a completely parsed lambda, then the combination's
                // result is an application. In that case, term is the argument
                // to this lambda.
                if (is_complete_lambda_) {
                    *this =
                        Application(std::make_unique<Term>(std::move(*this)),
                                    std::make_unique<Term>(std::move(term)));

                    is_lambda_ = false;
                    lambda_body_ = nullptr;
                    lambda_arg_name_ = "";
                    is_complete_lambda_ = false;
                } else {
                    // If not, term is part of the lambda's body.
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
        } else if (IsBoolConst()) {
            throw std::logic_error("Cannot combine term with a Bool constatn.");
        } else if (IsConditional()) {
            if (!cond_built_) {
                if (conditional_cond_) {
                    conditional_cond_->Combine(std::move(term));
                } else {
                    conditional_cond_ = std::make_unique<Term>(std::move(term));
                }
            } else if (!then_branch_built_) {
                if (conditional_then_branch_) {
                    conditional_then_branch_->Combine(std::move(term));
                } else {
                    conditional_then_branch_ =
                        std::make_unique<Term>(std::move(term));
                }
            } else if (!else_branch_built_) {
                if (conditional_else_branch_) {
                    conditional_else_branch_->Combine(std::move(term));
                } else {
                    conditional_else_branch_ =
                        std::make_unique<Term>(std::move(term));
                }
            } else {
                // A complete conditional, then term is an argument to result of
                // evaluating the conditional.
                // For example: (if true then (l x. true) else (l x. x)) y.
                *this = Application(std::make_unique<Term>(std::move(*this)),
                                    std::make_unique<Term>(std::move(term)));

                is_conditional_ = false;
                conditional_cond_ = nullptr;
                conditional_then_branch_ = nullptr;
                conditional_else_branch_ = nullptr;
                cond_built_ = false;
                then_branch_built_ = false;
                else_branch_built_ = false;
            }
        } else {
            *this = std::move(term);
        }
    }

    /*
     * Shifts the de Bruijn indices of all free variables inside this Term
     * up by distance amount. For an example use, see Term::Substitute(int,
     * Term&).
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
     * Substitutes variable (that is, the de Brijun idex of a variable) with
     * the term sub.
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
                    // Shift sub up by binding_context_size distance
                    // since sub is now substituted in
                    // binding_context_size deep context.
                    sub.Shift(binding_context_size);
                    std::swap(term, sub);
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

    Term& LambdaBody() {
        if (!IsLambda()) {
            throw std::invalid_argument("Invalid Lambda term.");
        }

        return *lambda_body_;
    }

    Term& ApplicationLHS() {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalide application term.");
        }

        return *application_lhs_;
    }

    Term& ApplicationRHS() {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalide application term.");
        }

        return *application_rhs_;
    }

    bool is_complete_lambda_ = false;
    bool cond_built_ = false;
    bool then_branch_built_ = false;
    bool else_branch_built_ = false;

   private:
    bool is_lambda_ = false;
    std::string lambda_arg_name_ = "";
    std::unique_ptr<Term> lambda_body_{};
    std::vector<lexer::Token> lambda_type_;

    bool is_variable_ = false;
    std::string variable_name_ = "";
    int de_bruijn_idx_ = -1;

    bool is_application_ = false;
    std::unique_ptr<Term> application_lhs_{};
    std::unique_ptr<Term> application_rhs_{};

    bool is_true_constant_ = false;

    bool is_false_constant_ = false;

    bool is_conditional_ = false;
    std::unique_ptr<Term> conditional_cond_{};
    std::unique_ptr<Term> conditional_then_branch_{};
    std::unique_ptr<Term> conditional_else_branch_{};
};

std::ostream& operator<<(std::ostream& out, const Term& term) {
    if (term.IsVariable()) {
        out << "[" << term.variable_name_ << "=" << term.de_bruijn_idx_ << "]";
    } else if (term.IsLambda()) {
        out << "{λ " << term.lambda_arg_name_ << ":";

        for (const auto& t : term.lambda_type_) {
            out << t;
        }

        out << ". " << *term.lambda_body_ << "}";
    } else if (term.IsApplication()) {
        out << "(" << *term.application_lhs_ << " <- " << *term.application_rhs_
            << ")";
    } else if (term.is_true_constant_) {
        out << "<true>";
    } else if (term.is_false_constant_) {
        out << "<false>";
    } else if (term.IsConditional()) {
        out << "<if> " << *term.conditional_cond_ << " <then> "
            << *term.conditional_then_branch_ << " <else> "
            << *term.conditional_else_branch_;
    } else {
        out << "<ERROR>";
    }

    return out;
}

// Known issue:
// ./a.out "(l z:Bool. if true then (l y:Bool. z y) else (l x:Bool. z x)) w"
// Probably can be solved by attaching to each ( the type of the token that came
// right after.
class Parser {
    using Token = lexer::Token;

   public:
    Parser(std::istringstream&& in) : lexer_(std::move(in)) {}

    void AddBinding(std::vector<Binding>& context, Binding binding) {
        context.push_back(binding);
    }

    Term ParseProgram() {
        Token next_token;
        std::stack<Term> term_stack;
        term_stack.emplace(Term());
        int balance_parens = 0;
        // Contains a list of bound variables in order of binding. For example,
        // for a term λ x. λ y. x y, this list would eventually contains {"x" ,
        // "y"} in that order. This is used to assign de Bruijn indices/static
        // distances to bound variables (ref: tapl,§6.1).
        std::vector<Binding> context;

        while ((next_token = lexer_.NextToken()).category !=
               Token::Category::MARKER_END) {
            if (next_token.category == Token::Category::LAMBDA) {
                auto lambda_arg = ParseVariable();
                ParseColon();
                Binding binding{lambda_arg.text, ParseType()};
                AddBinding(context, binding);
                term_stack.emplace(Term::Lambda(binding));
            } else if (next_token.category == Token::Category::VARIABLE) {
                auto bound_variable_it =
                    std::find_if(std::begin(context), std::end(context),
                                 [&next_token](const Binding& b) {
                                     return b.name == next_token.text;
                                 });
                int de_bruijn_idx = -1;

                if (bound_variable_it != std::end(context)) {
                    de_bruijn_idx =
                        std::distance(bound_variable_it, std::end(context)) - 1;
                } else {
                    // The naming context for free variables (ref: tapl,§6.1.2)
                    // is chosen to be the ASCII code of a variable's name.
                    //
                    // NOTE: Only single-character variable names are currecntly
                    // supported as free variables.
                    de_bruijn_idx = context.size() +
                                    (std::tolower(next_token.text[0]) - 'a');
                }

                term_stack.top().Combine(
                    Term::Variable(next_token.text, de_bruijn_idx));
            } else if (next_token.category == Token::Category::OPEN_PAREN) {
                term_stack.emplace(Term());
                ++balance_parens;
            } else if (next_token.category == Token::Category::CLOSE_PAREN) {
                CombineStackTop(term_stack);

                // A prenthesized λ-abstration is equivalent to a double
                // parenthesized term since we push a new term on the stack
                // for each lambda.
                if (term_stack.top().IsLambda()) {
                    std::cout << "Closing a lambda.\n";
                    // Mark the λ as complete so that terms to its right
                    // won't be combined to its body.
                    term_stack.top().is_complete_lambda_ = true;
                    // λ's variable is no longer part of the current binding
                    // context, therefore pop it.
                    context.pop_back();
                    CombineStackTop(term_stack);
                } else if (term_stack.top().IsConditional()) {
                    std::cout << "Closing an if.\n";
                    term_stack.top().else_branch_built_ = true;
                    CombineStackTop(term_stack);
                }

                --balance_parens;
            } else if (next_token.category == Token::Category::CONSTANT_TRUE) {
                term_stack.top().Combine(Term::ConstantTrue());
            } else if (next_token.category == Token::Category::CONSTANT_FALSE) {
                term_stack.top().Combine(Term::ConstantFalse());
            } else if (next_token.category == Token::Category::KEYWORD_IF) {
                term_stack.push(Term::Conditional());
                // Push an empty term for to build the condition.
                term_stack.push(Term());
            } else if (next_token.category == Token::Category::KEYWORD_THEN) {
                if (term_stack.size() < 2) {
                    throw std::invalid_argument("Syntax error.");
                }

                CombineStackTop(term_stack);

                if (!term_stack.top().IsConditional()) {
                    throw std::invalid_argument("Syntax error.");
                }

                term_stack.top().cond_built_ = true;
                // Push an empty term for to build the then branch.
                term_stack.push(Term());
            } else if (next_token.category == Token::Category::KEYWORD_ELSE) {
                if (term_stack.size() < 2) {
                    throw std::invalid_argument("Syntax error.");
                }

                CombineStackTop(term_stack);

                if (!term_stack.top().IsConditional()) {
                    throw std::invalid_argument("Syntax error.");
                }

                term_stack.top().then_branch_built_ = true;
                // Push an empty term for to build the else branch.
                term_stack.push(Term());
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

    std::vector<Token> ParseType() {
        std::vector<Token> type;

        while (true) {
            Token next = lexer_.NextToken();
            (next.category == Token::Category::KEYWORD_BOOL)
                ? type.push_back(next)
                : throw std::logic_error("Expected to parse Bool.");

            next = lexer_.NextToken();

            // So far, types can only exist in abstraction definitions and
            // is always expected to be followed by a dot. Therefore, ther
            // is no need to implement buffering or the ability to look
            // ahead tokens from the lexer.
            if (next.category == Token::Category::LAMBDA_DOT) {
                break;
            } else {
                (next.category == Token::Category::ARROW)
                    ? type.push_back(next)
                    : throw std::logic_error("Expected to parse an arrow.");
            }
        }

        return type;
    }

    Token ParseColon() {
        auto token = lexer_.NextToken();

        return (token.category == Token::Category::COLON)
                   ? token
                   : throw std::logic_error("Expected to parse a dot.");
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
            // distances by 1. That's because s will now be embedded one lever
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

    bool IsValue(const Term& term) { return term.IsLambda(); }
};
}  // namespace interpreter

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
    //    std::cout << token << " ";
    //}

    // std::cout << "\n";

    parser::Parser parser{std::istringstream{argv[1]}};
    auto program = parser.ParseProgram();

    std::cout << "   " << program << "\n";

    // interpreter::Interpreter interpreter;
    // interpreter.Interpret(program);

    // std::cout << "=> " << program << "\n";

    return 0;
}
