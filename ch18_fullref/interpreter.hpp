#pragma once

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <utility>
#include <vector>

namespace lexer {
struct Token {
    enum class Category {
        IDENTIFIER,

        LAMBDA,

        DOT,
        COMMA,
        EQUAL,
        ASSIGN,
        OPEN_PAREN,
        CLOSE_PAREN,
        OPEN_BRACE,
        CLOSE_BRACE,
        COLON,
        ARROW,
        EXCLAMATION,
        SEMICOLON,

        CONSTANT_TRUE,
        CONSTANT_FALSE,

        KEYWORD_BOOL,
        KEYWORD_IF,
        KEYWORD_THEN,
        KEYWORD_ELSE,

        CONSTANT_ZERO,

        KEYWORD_NAT,
        KEYWORD_SUCC,
        KEYWORD_PRED,
        KEYWORD_ISZERO,

        KEYWORD_LET,
        KEYWORD_IN,

        KEYWORD_REF,
        KEYWORD_REF_TYPE,

        CONSTANT_UNIT,
        KEYWORD_UNIT_TYPE,

        KEYWORD_FIX,

        MARKER_END,
        MARKER_INVALID,
    };

    enum class IdentitySubCategory {
        VARIABLE,
        RECORD_LABEL,
    };

    explicit Token(Category category = Category::MARKER_INVALID,
                   std::string text = "")
        : category_(category),
          text_(category == Category::IDENTIFIER ? text : "") {}

    bool operator==(const Token &other) const {
        return category_ == other.category_ && text_ == other.text_;
    }

    bool operator!=(const Token &other) const { return !(*this == other); }

    Category GetCategory() const { return category_; }

    std::string GetText() const { return text_; }

   private:
    Category category_ = Category::MARKER_INVALID;
    std::string text_;
};

std::ostream &operator<<(std::ostream &out, Token token);

namespace {
const std::string kLambdaInputSymbol = "l";
const std::string kKeywordTop = "Top";
const std::string kKeywordBool = "Bool";
const std::string kKeywordNat = "Nat";
const std::string kKeywordLet = "let";
const std::string kKeywordIn = "in";
const std::string kKeywordRef = "ref";
const std::string kKeywordRefType = "Ref";
const std::string kKeywordUnit = "unit";
const std::string kKeywordUnitType = "Unit";
const std::string kKeywordFix = "fix";
}  // namespace

class Lexer {
   public:
    explicit Lexer(std::istringstream &&in) {
        std::istringstream iss(SurroundTokensBySpaces(std::move(in)));
        token_strings_ =
            std::vector<std::string>(std::istream_iterator<std::string>{iss},
                                     std::istream_iterator<std::string>());
    }

    Token NextToken() {
        Token token;

        if (current_token_ == token_strings_.size()) {
            current_token_ = token_strings_.size() + 1;
            token = Token(Token::Category::MARKER_END);
            return token;
        }

        static std::unordered_map<std::string, Token::Category>
            token_str_to_cat = {
                {kLambdaInputSymbol, Token::Category::LAMBDA},

                {".", Token::Category::DOT},
                {",", Token::Category::COMMA},
                {"=", Token::Category::EQUAL},
                {"(", Token::Category::OPEN_PAREN},
                {")", Token::Category::CLOSE_PAREN},
                {"{", Token::Category::OPEN_BRACE},
                {"}", Token::Category::CLOSE_BRACE},
                {":", Token::Category::COLON},
                {"->", Token::Category::ARROW},
                {":=", Token::Category::ASSIGN},
                {"!", Token::Category::EXCLAMATION},
                {";", Token::Category::SEMICOLON},

                {"true", Token::Category::CONSTANT_TRUE},
                {"false", Token::Category::CONSTANT_FALSE},

                {kKeywordBool, Token::Category::KEYWORD_BOOL},
                {"if", Token::Category::KEYWORD_IF},
                {"then", Token::Category::KEYWORD_THEN},
                {"else", Token::Category::KEYWORD_ELSE},

                {"0", Token::Category::CONSTANT_ZERO},

                {kKeywordNat, Token::Category::KEYWORD_NAT},
                {"succ", Token::Category::KEYWORD_SUCC},
                {"pred", Token::Category::KEYWORD_PRED},
                {"iszero", Token::Category::KEYWORD_ISZERO},

                {kKeywordLet, Token::Category::KEYWORD_LET},
                {kKeywordIn, Token::Category::KEYWORD_IN},

                {kKeywordRef, Token::Category::KEYWORD_REF},
                {kKeywordRefType, Token::Category::KEYWORD_REF_TYPE},

                {kKeywordUnit, Token::Category::CONSTANT_UNIT},
                {kKeywordUnitType, Token::Category::KEYWORD_UNIT_TYPE},

                {kKeywordFix, Token::Category::KEYWORD_FIX},
            };

        auto token_string = token_strings_[current_token_];

        if (token_str_to_cat.find(token_string) != std::end(token_str_to_cat)) {
            token = Token(token_str_to_cat[token_string]);
        } else if (IsIdentifierName(token_string)) {
            token = Token(Token::Category::IDENTIFIER, token_string);
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
    std::string SurroundTokensBySpaces(std::istringstream &&in) {
        std::ostringstream processed_stream;
        char c;

        while (in.get(c)) {
            // Check for one-character separators and surround them with spaces.
            if (c == ',' || c == '.' || c == '=' || c == '(' || c == ')' ||
                c == '{' || c == '}' || c == '!' || c == ';') {
                processed_stream << " " << c << " ";
            } else if (c == '-') {
                // Check for the two-character serparator '->' and surround it
                // with spaces.
                if (in.peek() == '>') {
                    in.get(c);
                    processed_stream << " -> ";
                } else {
                    // Just write '-' and let the lexing error be
                    // discovered later.
                    processed_stream << " - ";
                }
            } else if (c == ':') {
                // Check for the two-character serparator ':=' and surround it
                // with spaces.
                if (in.peek() == '=') {
                    in.get(c);
                    processed_stream << " := ";
                } else {
                    processed_stream << " : ";
                }
            } else {
                processed_stream << c;
            }
        }

        return processed_stream.str();
    }

    bool IsIdentifierName(std::string token_text) {
        static const std::regex id_regex("[_[:alpha:]][_[:alnum:]]*");
        return std::regex_match(token_text, id_regex);
    }

   private:
    std::vector<std::string> token_strings_;
    int current_token_ = 0;
};

std::ostream &operator<<(std::ostream &out, Token token) {
    static std::unordered_map<Token::Category, std::string> token_to_str = {
        {Token::Category::LAMBDA, "λ"},

        {Token::Category::DOT, "."},
        {Token::Category::COMMA, ","},
        {Token::Category::EQUAL, "="},
        {Token::Category::OPEN_PAREN, "("},
        {Token::Category::CLOSE_PAREN, ")"},
        {Token::Category::OPEN_BRACE, "{"},
        {Token::Category::CLOSE_BRACE, "}"},
        {Token::Category::COLON, ":"},
        {Token::Category::ARROW, "->"},
        {Token::Category::ASSIGN, ":="},
        {Token::Category::EXCLAMATION, "!"},
        {Token::Category::SEMICOLON, ";"},

        {Token::Category::CONSTANT_TRUE, "<true>"},
        {Token::Category::CONSTANT_FALSE, "<false>"},

        {Token::Category::KEYWORD_BOOL, "<Bool>"},
        {Token::Category::KEYWORD_IF, "<if>"},
        {Token::Category::KEYWORD_THEN, "<then>"},
        {Token::Category::KEYWORD_ELSE, "<else>"},

        {Token::Category::CONSTANT_ZERO, "0"},

        {Token::Category::KEYWORD_NAT, "<Nat>"},
        {Token::Category::KEYWORD_SUCC, "succ"},
        {Token::Category::KEYWORD_PRED, "pred"},
        {Token::Category::KEYWORD_ISZERO, "iszero"},

        {Token::Category::KEYWORD_LET, "let"},
        {Token::Category::KEYWORD_IN, "in"},

        {Token::Category::KEYWORD_REF, "ref"},
        {Token::Category::KEYWORD_REF_TYPE, "Ref"},

        {Token::Category::CONSTANT_UNIT, "unit"},
        {Token::Category::KEYWORD_UNIT_TYPE, "Unit"},

        {Token::Category::KEYWORD_FIX, kKeywordFix},

        {Token::Category::MARKER_END, "<END>"},
        {Token::Category::MARKER_INVALID, "<INVALID>"},
    };

    if (token_to_str.find(token.GetCategory()) != std::end(token_to_str)) {
        out << token_to_str[token.GetCategory()];
    } else {
        switch (token.GetCategory()) {
            case Token::Category::IDENTIFIER:
                out << token.GetText();
                break;

            default:
                out << "<ILLEGAL_TOKEN>";
        }
    }

    return out;
}
}  // namespace lexer

namespace parser {

// TODO Add support for creating named (and anonymouse) record types.
class Type {
    friend std::ostream &operator<<(std::ostream &, const Type &);

   public:
    static Type &Top() {
        static Type type;
        type.category_ = TypeCategory::TOP;

        return type;
    }

    static Type &IllTyped() {
        static Type type;

        return type;
    }

    static Type &Bool() {
        static Type type(BaseType::BOOL);

        return type;
    }

    static Type &Nat() {
        static Type type(BaseType::NAT);

        return type;
    }

    static Type &Unit() {
        static Type type(BaseType::UNIT);

        return type;
    }

    static Type &Function(Type &lhs, Type &rhs) {
        static std::vector<std::unique_ptr<Type>> type_pool;

        auto result =
            std::find_if(std::begin(type_pool), std::end(type_pool),
                         [&](const std::unique_ptr<Type> &type) {
                             return type->lhs_ == &lhs && type->rhs_ == &rhs;
                         });

        if (result != std::end(type_pool)) {
            return **result;
        }

        type_pool.emplace_back(std::unique_ptr<Type>(new Type(lhs, rhs)));

        return *type_pool.back();
    }

    using RecordFields = std::unordered_map<std::string, Type &>;

    static Type &Record(RecordFields fields) {
        static std::vector<std::unique_ptr<Type>> type_pool;

        auto result = std::find_if(std::begin(type_pool), std::end(type_pool),
                                   [&](const std::unique_ptr<Type> &type) {
                                       return type->record_fields_ == fields;
                                   });

        if (result != std::end(type_pool)) {
            return **result;
        }

        type_pool.emplace_back(
            std::unique_ptr<Type>(new Type(std::move(fields))));

        return *type_pool.back();
    }

    static Type &Ref(Type &ref_type) {
        static std::vector<std::unique_ptr<Type>> type_pool;

        auto result = std::find_if(std::begin(type_pool), std::end(type_pool),
                                   [&](const std::unique_ptr<Type> &type) {
                                       return type->ref_type_ == &ref_type;
                                   });

        if (result != std::end(type_pool)) {
            return **result;
        }

        type_pool.emplace_back(std::unique_ptr<Type>(new Type(&ref_type)));

        return *type_pool.back();
    }

    Type(const Type &) = delete;
    Type &operator=(const Type &) = delete;

    Type(Type &&) = delete;
    Type &operator=(Type &&) = delete;

    ~Type() = default;

    bool operator==(const Type &other) const {
        if (category_ != other.category_) {
            return false;
        }

        switch (category_) {
            case TypeCategory::TOP:
                return other.category_ == TypeCategory::TOP;
            case TypeCategory::ILL:
                return other.category_ == TypeCategory::ILL;
            case TypeCategory::BASE:
                return base_type_ == other.base_type_;
            case TypeCategory::FUNCTION:
                assert(lhs_ && rhs_ && other.lhs_ && other.rhs_);
                return (*lhs_ == *other.lhs_) && (*rhs_ == *other.rhs_);
            case TypeCategory::REF:
                return *ref_type_ == *other.ref_type_;
            case TypeCategory::RECORD:
                return record_fields_ == other.record_fields_;
        }
    }

    bool operator!=(const Type &other) const { return !(*this == other); }

    bool IsIllTyped() const { return category_ == TypeCategory::ILL; }

    bool IsTop() const { return category_ == TypeCategory::TOP; }

    bool IsBool() const {
        return category_ == TypeCategory::BASE && base_type_ == BaseType::BOOL;
    }

    bool IsNat() const {
        return category_ == TypeCategory::BASE && base_type_ == BaseType::NAT;
    }

    bool IsUnit() const {
        return category_ == TypeCategory::BASE && base_type_ == BaseType::UNIT;
    }

    bool IsFunction() const { return category_ == TypeCategory::FUNCTION; }

    bool IsRecord() const { return category_ == TypeCategory::RECORD; }

    bool IsRef() const { return category_ == TypeCategory::REF; }

    Type &FunctionLHS() const {
        if (!IsFunction()) {
            throw std::invalid_argument("Invalid function type.");
        }

        return *lhs_;
    }

    Type &FunctionRHS() const {
        if (!IsFunction()) {
            throw std::invalid_argument("Invalid function type.");
        }

        return *rhs_;
    }

    const RecordFields &GetRecordFields() const {
        if (!IsRecord()) {
            throw std::invalid_argument("Invalid record type.");
        }

        return record_fields_;
    }

    Type &RefType() const {
        if (!IsRef()) {
            throw std::invalid_argument("Expected Ref type.");
        }

        return *ref_type_;
    }

    int StorageSize() const {
        if (IsBool() || IsUnit()) {
            return 1;
        } else if (IsNat() || IsFunction()) {
            return sizeof(int);
        } else if (IsRecord()) {
            int total_size = 1;

            for (auto &field : GetRecordFields()) {
                total_size += field.second.StorageSize();
            }

            return total_size;
        }

        std::ostringstream ss;
        ss << "Type size cannot be computed: " << *this;
        throw std::logic_error(ss.str());
    }

   private:
    enum class TypeCategory {
        BASE,
        FUNCTION,
        RECORD,
        TOP,
        REF,
        ILL,
    };

    enum class BaseType {
        BOOL,
        NAT,
        UNIT,
    };

    Type() = default;

    explicit Type(BaseType base_type)
        : base_type_(base_type), category_(TypeCategory::BASE) {}

    Type(Type &lhs, Type &rhs)
        : lhs_(&lhs), rhs_(&rhs), category_(TypeCategory::FUNCTION) {}

    Type(RecordFields fields)
        : record_fields_(std::move(fields)), category_(TypeCategory::RECORD) {}

    Type(Type *ref_type) : ref_type_(ref_type), category_(TypeCategory::REF) {}

    TypeCategory category_ = TypeCategory::ILL;

    BaseType base_type_;

    Type *lhs_ = nullptr;
    Type *rhs_ = nullptr;

    RecordFields record_fields_{};

    Type *ref_type_ = nullptr;
};

std::ostream &operator<<(std::ostream &out, const Type &type) {
    if (type.IsTop()) {
        out << lexer::kKeywordTop;
    } else if (type.IsBool()) {
        out << lexer::kKeywordBool;
    } else if (type.IsNat()) {
        out << lexer::kKeywordNat;
    } else if (type.IsUnit()) {
        out << lexer::kKeywordUnitType;
    } else if (type.IsFunction()) {
        out << "(" << *type.lhs_ << " "
            << lexer::Token(lexer::Token::Category::ARROW) << " " << *type.rhs_
            << ")";
    } else if (type.IsRecord()) {
        out << "{";

        bool first = true;
        for (auto &field : type.record_fields_) {
            if (!first) {
                out << ", ";
            }

            first = false;
            out << field.first << ":" << field.second;
        }

        out << "}";
    } else if (type.IsRef()) {
        out << "Ref " << *type.ref_type_;
    } else {
        out << "Ⱦ";
    }

    return out;
}

class Term {
    friend std::ostream &operator<<(std::ostream &, const Term &);

   public:
    enum class Category {
        EMPTY,
        LAMBDA,
        VARIABLE,
        APPLICATION,
        IF,
        TRUE,
        FALSE,
        SUCC,
        PRED,
        ISZERO,
        ZERO,
        RECORD,
        PROJECTION,
        LET,
        REF,
        DEREF,
        ASSIGNMENT,
        UNIT,
        SEQUENCE,
        PARENTHESIZED,
        FIX,

        STORE_LOCATION,
    };

    static Term Lambda(std::string arg_name, Type &arg_type) {
        Term result;
        result.lambda_arg_name_ = arg_name;
        result.lambda_arg_type_ = &arg_type;
        result.category_ = Category::LAMBDA;

        return result;
    }

    static Term Variable(std::string var_name, int de_bruijn_idx) {
        Term result;
        result.variable_name_ = var_name;
        result.de_bruijn_idx_ = de_bruijn_idx;
        result.category_ = Category::VARIABLE;

        return result;
    }

    static Term Application(std::unique_ptr<Term> lhs,
                            std::unique_ptr<Term> rhs) {
        Term result;
        result.category_ = Category::APPLICATION;
        result.application_lhs_ = std::move(lhs);
        result.application_rhs_ = std::move(rhs);

        return result;
    }

    static Term If() {
        Term result;
        result.category_ = Category::IF;

        return result;
    }

    static Term True() {
        Term result;
        result.category_ = Category::TRUE;

        return result;
    }

    static Term False() {
        Term result;
        result.category_ = Category::FALSE;

        return result;
    }

    static Term Succ() {
        Term result;
        result.category_ = Category::SUCC;

        return result;
    }

    static Term Pred() {
        Term result;
        result.category_ = Category::PRED;

        return result;
    }

    static Term IsZero() {
        Term result;
        result.category_ = Category::ISZERO;

        return result;
    }

    static Term Zero() {
        Term result;
        result.category_ = Category::ZERO;

        return result;
    }

    static Term Record() {
        Term result;
        result.category_ = Category::RECORD;

        return result;
    }

    static Term Projection(std::unique_ptr<Term> term, std::string label) {
        Term result;
        result.projection_term_ = std::move(term);
        result.projection_label_ = label;
        result.category_ = Category::PROJECTION;

        return result;
    }

    static Term Let(std::string binding_name) {
        Term result;
        result.category_ = Category::LET;
        result.let_binding_name_ = binding_name;

        return result;
    }

    static Term Ref() {
        Term result;
        result.category_ = Category::REF;

        return result;
    }

    static Term Deref() {
        Term result;
        result.category_ = Category::DEREF;

        return result;
    }

    static Term Assignment(std::unique_ptr<Term> lhs) {
        Term result;
        result.assignment_lhs_ = std::move(lhs);
        result.category_ = Category::ASSIGNMENT;

        return result;
    }

    static Term Unit() {
        Term result;
        result.category_ = Category::UNIT;

        return result;
    }

    static Term StoreLocation(int location) {
        Term result;
        result.category_ = Category::STORE_LOCATION;
        result.store_location_ = location;

        return result;
    }

    static Term Sequence(std::unique_ptr<Term> lhs) {
        Term result;
        result.sequence_lhs_ = std::move(lhs);
        result.category_ = Category::SEQUENCE;

        return result;
    }

    static Term ParenthesizedTerm() {
        Term result;
        result.category_ = Category::PARENTHESIZED;

        return result;
    }

    static Term FixTerm() {
        Term result;
        result.category_ = Category::FIX;

        return result;
    }

    Term() = default;

    Term(const Term &) = delete;
    Term &operator=(const Term &) = delete;

    Term(Term &&) = default;
    Term &operator=(Term &&) = default;

    ~Term() = default;

    bool IsLambda() const { return category_ == Category::LAMBDA; }

    void MarkAsComplete() { is_complete_ = true; }

    bool IsVariable() const { return category_ == Category::VARIABLE; }

    bool IsApplication() const { return category_ == Category::APPLICATION; }

    bool IsIf() const { return category_ == Category::IF; }

    bool IsTrue() const { return category_ == Category::TRUE; }

    bool IsFalse() const { return category_ == Category::FALSE; }

    bool IsSucc() const { return category_ == Category::SUCC; }

    bool IsPred() const { return category_ == Category::PRED; }

    bool IsIsZero() const { return category_ == Category::ISZERO; }

    bool IsConstantZero() const { return category_ == Category::ZERO; }

    bool IsRecord() const { return category_ == Category::RECORD; }

    bool IsProjection() const { return category_ == Category::PROJECTION; }

    bool IsLet() const { return category_ == Category::LET; }

    bool IsRef() const { return category_ == Category::REF; }

    bool IsDeref() const { return category_ == Category::DEREF; }

    bool IsAssignment() const { return category_ == Category::ASSIGNMENT; }

    bool IsUnit() const { return category_ == Category::UNIT; }

    bool IsStoreLocation() const {
        return category_ == Category::STORE_LOCATION;
    }

    bool IsSequence() const { return category_ == Category::SEQUENCE; }

    bool IsParenthesized() const {
        return category_ == Category::PARENTHESIZED;
    }

    bool IsFix() const { return category_ == Category::FIX; }

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
        } else if (IsTrue() || IsFalse() || IsConstantZero() || IsUnit() ||
                   IsStoreLocation()) {
            return false;
        } else if (IsSucc()) {
            return !unary_op_arg_;
        } else if (IsPred()) {
            return !unary_op_arg_;
        } else if (IsIsZero()) {
            return !unary_op_arg_;
        } else if (IsRecord()) {
            return record_labels_.size() == 0 ||
                   record_labels_.size() != record_terms_.size();
        } else if (IsProjection()) {
            return !projection_term_;
        } else if (IsLet()) {
            return let_binding_name_.empty() || !let_bound_term_ ||
                   !let_body_term_;
        } else if (IsRef()) {
            return !ref_term_;
        } else if (IsDeref()) {
            return !deref_term_;
        } else if (IsAssignment()) {
            return !assignment_lhs_ || !assignment_rhs_;
        } else if (IsSequence()) {
            return !sequence_lhs_ || !sequence_rhs_;
        } else if (IsParenthesized()) {
            return !parenthesized_term_;
        } else if (IsFix()) {
            return !fix_term_;
        }

        return true;
    }

    bool IsEmpty() const { return category_ == Category::EMPTY; }

    enum Category Category() const { return category_; }

    Term &Combine(Term &&term) {
        if (term.IsInvalid()) {
            throw std::invalid_argument(
                "Term::Combine() received an invalid Term.");
        }

        if (IsLambda()) {
            if (!lambda_body_) {
                lambda_body_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else if (IsVariable()) {
            ConvertToApplication(std::move(term));

            variable_name_ = "";
        } else if (IsApplication()) {
            ConvertToApplication(std::move(term));
        } else if (IsIf()) {
            if (!if_condition_) {
                if_condition_ = std::make_unique<Term>(std::move(term));
            } else if (!if_then_) {
                if_then_ = std::make_unique<Term>(std::move(term));
            } else {
                if (!if_else_) {
                    if_else_ = std::make_unique<Term>(std::move(term));
                } else {
                    ConvertToApplication(std::move(term));
                }
            }
        } else if (IsSucc()) {
            if (!unary_op_arg_) {
                unary_op_arg_ = std::make_unique<Term>(std::move(term));
            } else {
                throw std::invalid_argument(
                    "Trying to combine with succ(...).");
            }
        } else if (IsPred()) {
            if (!unary_op_arg_) {
                unary_op_arg_ = std::make_unique<Term>(std::move(term));
            } else {
                throw std::invalid_argument(
                    "Trying to combine with pred(...).");
            }
        } else if (IsIsZero()) {
            if (!unary_op_arg_) {
                unary_op_arg_ = std::make_unique<Term>(std::move(term));
            } else {
                throw std::invalid_argument(
                    "Trying to combine with iszero(...).");
            }
        } else if (IsTrue() || IsFalse() || IsConstantZero() || IsUnit()) {
            throw std::invalid_argument("Trying to combine with a constant.");
        } else if (IsRecord()) {
            if (is_complete_) {
                throw std::logic_error(
                    "Trying to combine with a complete Record.");
            }

            if (!IsRecordExpectingTerm()) {
                throw std::logic_error(
                    "Trying to combine with a Record while it isn't expecting "
                    "a value.");
            }

            record_terms_.push_back(std::make_unique<Term>(std::move(term)));
        } else if (IsProjection()) {
            ConvertToApplication(std::move(term));
        } else if (IsLet()) {
            if (!let_bound_term_) {
                let_bound_term_ = std::make_unique<Term>(std::move(term));
            } else {
                if (!let_body_term_) {
                    let_body_term_ = std::make_unique<Term>(std::move(term));
                } else {
                    ConvertToApplication(std::move(term));
                }
            }
        } else if (IsRef()) {
            if (!ref_term_) {
                ref_term_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else if (IsDeref()) {
            if (!deref_term_) {
                deref_term_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else if (IsAssignment()) {
            if (!assignment_lhs_) {
                throw std::logic_error("Combining with an empty assignment.");
            } else if (!assignment_rhs_) {
                assignment_rhs_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else if (IsSequence()) {
            if (!sequence_lhs_) {
                throw std::logic_error("Combining with an empty sequence.");
            } else if (!sequence_rhs_) {
                sequence_rhs_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else if (IsParenthesized()) {
            if (!parenthesized_term_) {
                parenthesized_term_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else if (IsFix()) {
            if (!fix_term_) {
                fix_term_ = std::make_unique<Term>(std::move(term));
            } else {
                ConvertToApplication(std::move(term));
            }
        } else {
            *this = std::move(term);
        }

        return *this;
    }

    void ConvertToProjection(std::string label) {
        *this = Projection(std::make_unique<Term>(std::move(*this)), label);
    }

    void ConvertToAssignment() {
        *this = Assignment(std::make_unique<Term>(std::move(*this)));
    }

    void ConvertToSequence() {
        *this = Sequence(std::make_unique<Term>(std::move(*this)));
    }

    void AddRecordLabel(std::string label) {
        if (!IsRecord()) {
            throw std::logic_error("Expected a Record.");
        }

        record_labels_.push_back(label);
    }

    void ConvertToApplication(Term &&term) {
        if (term.IsApplication()) {
            *this = Application(
                std::make_unique<Term>(Application(
                    std::make_unique<Term>(std::move(*this)),
                    std::make_unique<Term>(std::move(*term.application_lhs_)))),
                std::make_unique<Term>(std::move(*term.application_rhs_)));

            // If we have yet another nested application in this application's
            // lhs with wrong associativity, then fix it.
            //
            // NOTE: this is quite complicated and might not be general enough.
            //
            // TODO: try to find a test to break this.
            if (application_lhs_->IsApplication() &&
                application_lhs_->application_rhs_->IsApplication()) {
                application_lhs_->application_lhs_->ConvertToApplication(
                    std::move(*application_lhs_->application_rhs_));

                application_lhs_ =
                    std::move(application_lhs_->application_lhs_);
            }
        } else {
            *this = Application(std::make_unique<Term>(std::move(*this)),
                                std::make_unique<Term>(std::move(term)));
        }
    }

    /*
     * Shifts the de Bruijn indices of all free variables inside this Term up by
     * distance amount. For an example use, see Term::Substitute(int, Term&).
     */
    void Shift(int distance) {
        std::function<void(int, Term &)> walk = [&distance, &walk](
                                                    int binding_context_size,
                                                    Term &term) {
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
    void Substitute(int variable, Term &sub) {
        if (IsInvalid() || sub.IsInvalid()) {
            throw std::invalid_argument(
                "Trying to substitute using invalid terms.");
        }

        std::function<void(int, Term &)> walk = [&variable, &sub, &walk](
                                                    int binding_context_size,
                                                    Term &term) {
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
            } else if (term.IsLet()) {
                walk(binding_context_size + 1, *term.let_bound_term_);
                walk(binding_context_size + 1, *term.let_body_term_);
            } else if (term.IsApplication()) {
                walk(binding_context_size, *term.application_lhs_);
                walk(binding_context_size, *term.application_rhs_);
            } else if (term.IsIf()) {
                walk(binding_context_size, *term.if_condition_);
                walk(binding_context_size, *term.if_then_);
                walk(binding_context_size, *term.if_else_);
            } else if (term.IsSucc()) {
                walk(binding_context_size, *term.unary_op_arg_);
            } else if (term.IsPred()) {
                walk(binding_context_size, *term.unary_op_arg_);
            } else if (term.IsIsZero()) {
                walk(binding_context_size, *term.unary_op_arg_);
            } else if (term.IsProjection()) {
                walk(binding_context_size, *term.projection_term_);
            } else if (term.IsLet()) {
                walk(binding_context_size, *term.let_bound_term_);
                walk(binding_context_size, *term.let_body_term_);
            } else if (term.IsRef()) {
                walk(binding_context_size, *term.ref_term_);
            } else if (term.IsDeref()) {
                walk(binding_context_size, *term.deref_term_);
            } else if (term.IsAssignment()) {
                walk(binding_context_size, *term.assignment_lhs_);
                walk(binding_context_size, *term.assignment_rhs_);
            } else if (term.IsSequence()) {
                walk(binding_context_size, *term.sequence_lhs_);
                walk(binding_context_size, *term.sequence_rhs_);
            } else if (term.IsParenthesized()) {
                walk(binding_context_size, *term.parenthesized_term_);
            } else if (term.IsRecord()) {
                for (int i = 0; i < term.record_terms_.size(); ++i) {
                    walk(binding_context_size, *term.record_terms_[i]);
                }
            } else if (term.IsFix()) {
                walk(binding_context_size, *term.fix_term_);
            }
        };

        walk(0, *this);
    }

    Term &LambdaBody() const {
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

    Type &LambdaArgType() const {
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

    Term &ApplicationLHS() const {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalid application term.");
        }

        return *application_lhs_;
    }

    Term &ApplicationRHS() const {
        if (!IsApplication()) {
            throw std::invalid_argument("Invalid application term.");
        }

        return *application_rhs_;
    }

    Term &IfCondition() const {
        if (!IsIf()) {
            throw std::invalid_argument("Invalid if term.");
        }

        return *if_condition_;
    }

    Term &IfThen() const {
        if (!IsIf()) {
            throw std::invalid_argument("Invalid if term.");
        }

        return *if_then_;
    }

    Term &IfElse() const {
        if (!IsIf()) {
            throw std::invalid_argument("Invalid if term.");
        }

        return *if_else_;
    }

    Term &UnaryOpArg() const {
        if (!IsSucc() && !IsPred() && !IsIsZero()) {
            throw std::invalid_argument("Invalid term.");
        }

        return *unary_op_arg_;
    }

    const std::vector<std::string> &RecordLabels() const {
        return record_labels_;
    }

    const std::vector<std::unique_ptr<Term>> &RecordTerms() const {
        return record_terms_;
    }

    Term &ProjectionTerm() const { return *projection_term_; }

    std::string ProjectionLabel() const { return projection_label_; }

    std::string LetBindingName() const { return let_binding_name_; }

    Term &LetBoundTerm() const { return *let_bound_term_; }

    Term &LetBodyTerm() const { return *let_body_term_; }

    Term &RefTerm() const { return *ref_term_; }

    Term &DerefTerm() const { return *deref_term_; }

    Term &AssignmentLHS() const { return *assignment_lhs_; }

    Term &AssignmentRHS() const { return *assignment_rhs_; }

    Term &SequenceLHS() const { return *sequence_lhs_; }

    Term &SequenceRHS() const { return *sequence_rhs_; }

    int StoreLocationValue() const { return store_location_; }

    Term &GetParenthesizedTerm() const { return *parenthesized_term_; }

    Term &GetFixTerm() const { return *fix_term_; }

    bool operator==(const Term &other) const {
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

        if (IsSucc() && other.IsSucc()) {
            return UnaryOpArg() == other.UnaryOpArg();
        }

        if (IsPred() && other.IsPred()) {
            return UnaryOpArg() == other.UnaryOpArg();
        }

        if (IsIsZero() && other.IsIsZero()) {
            return UnaryOpArg() == other.UnaryOpArg();
        }

        if (IsConstantZero() && other.IsConstantZero()) {
            return true;
        }

        if (IsRecord() && other.IsRecord()) {
            return record_labels_ == record_labels_ &&
                   record_terms_ == record_terms_;
        }

        if (IsProjection() && other.IsProjection()) {
            return projection_label_ == other.projection_label_ &&
                   *projection_term_ == *other.projection_term_;
        }

        if (IsLet() && other.IsLet()) {
            return let_binding_name_ == other.let_binding_name_ &&
                   *let_bound_term_ == *other.let_bound_term_ &&
                   *let_body_term_ == *other.let_body_term_;
        }

        if (IsRef() && other.IsRef()) {
            return *ref_term_ == *other.ref_term_;
        }

        if (IsDeref() && other.IsDeref()) {
            return *deref_term_ == *other.deref_term_;
        }

        if (IsAssignment() && other.IsAssignment()) {
            return *assignment_lhs_ == *other.assignment_lhs_ &&
                   *assignment_rhs_ == *other.assignment_rhs_;
        }

        if (IsUnit() && other.IsUnit()) {
            return true;
        }

        if (IsSequence() && other.IsSequence()) {
            return *sequence_lhs_ == *other.sequence_lhs_ &&
                   *sequence_rhs_ == *other.sequence_rhs_;
        }

        if (IsParenthesized() && other.IsParenthesized()) {
            return *parenthesized_term_ == *other.parenthesized_term_;
        }

        if (IsFix() && other.IsFix()) {
            return *fix_term_ == *other.fix_term_;
        }

        return false;
    }

    bool operator!=(const Term &other) const { return !(*this == other); }

    std::string ASTString(int indentation = 0) const {
        std::ostringstream out;
        std::string prefix = std::string(indentation, '-');
        std::string prefix_extra = std::string(indentation + 2, '-');

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
        } else if (IsSucc()) {
            out << prefix << "succ\n";
            out << unary_op_arg_->ASTString(indentation + 2);
        } else if (IsPred()) {
            out << prefix << "pred\n";
            out << unary_op_arg_->ASTString(indentation + 2);
        } else if (IsIsZero()) {
            out << prefix << "iszero\n";
            out << unary_op_arg_->ASTString(indentation + 2);
        } else if (IsConstantZero()) {
            out << prefix << "0";
        } else if (IsRecord()) {
            out << prefix << "{\n";

            for (int i = 0; i < record_labels_.size(); ++i) {
                out << prefix << "=\n";

                out << prefix_extra << record_labels_[i] << "\n";
                out << record_terms_[i]->ASTString(indentation + 2) << "\n";
            }

            out << prefix << "}";
        } else if (IsProjection()) {
            out << prefix << ".\n";
            out << projection_term_->ASTString(indentation + 2) << "\n";
            out << prefix_extra << projection_label_;
        } else if (IsLet()) {
            out << prefix << "let\n";
            out << prefix_extra << let_binding_name_ << "\n";
            out << prefix << "=\n";
            out << let_bound_term_->ASTString(indentation + 2) << "\n";
            out << prefix << "in\n";
            out << let_body_term_->ASTString(indentation + 2);
        } else if (IsRef()) {
            out << prefix << "ref\n";
            out << ref_term_->ASTString(indentation + 2);
        } else if (IsDeref()) {
            out << prefix << "!\n";
            out << deref_term_->ASTString(indentation + 2);
        } else if (IsAssignment()) {
            out << prefix << ":=\n";
            out << assignment_lhs_->ASTString(indentation + 2) << "\n";
            out << assignment_rhs_->ASTString(indentation + 2);
        } else if (IsUnit()) {
            out << prefix << "unit";
        } else if (IsStoreLocation()) {
            out << prefix << "l[" << store_location_ << "]";
        } else if (IsSequence()) {
            out << prefix << ";\n";
            out << sequence_lhs_->ASTString(indentation + 2) << "\n";
            out << sequence_rhs_->ASTString(indentation + 2);
        } else if (IsParenthesized()) {
            out << prefix << "(\n";
            out << parenthesized_term_->ASTString(indentation + 2) << "\n";
            out << prefix << ")";
        } else if (IsFix()) {
            out << prefix << "fix \n";
            out << fix_term_->ASTString(indentation + 2) << "\n";
        }

        return out.str();
    }

    Term Clone() const {
        if (IsInvalid()) {
            throw std::logic_error("Trying to clone an invalid term.");
        }

        if (IsLambda()) {
            return std::move(Lambda(lambda_arg_name_, *lambda_arg_type_)
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
        } else if (IsConstantZero()) {
            return Term::Zero();
        } else if (IsUnit()) {
            return Term::Unit();
        } else if (IsSucc()) {
            return std::move(Term::Succ().Combine(unary_op_arg_->Clone()));
        } else if (IsPred()) {
            return std::move(Term::Pred().Combine(unary_op_arg_->Clone()));
        } else if (IsIsZero()) {
            return std::move(Term::IsZero().Combine(unary_op_arg_->Clone()));
        } else if (IsRecord()) {
            Term result = Term::Record();

            for (int i = 0; i < record_labels_.size(); ++i) {
                result.AddRecordLabel(record_labels_[i]);
                result.Combine(record_terms_[i]->Clone());
            }

            return std::move(result);
        } else if (IsRef()) {
            return std::move(Term::Ref().Combine(ref_term_->Clone()));
        } else if (IsDeref()) {
            return std::move(Term::Deref().Combine(deref_term_->Clone()));
        } else if (IsStoreLocation()) {
            return std::move(Term::StoreLocation(store_location_));
        } else if (IsParenthesized()) {
            return std::move(Term::ParenthesizedTerm().Combine(
                parenthesized_term_->Clone()));
        } else if (IsAssignment()) {
            return std::move(Term::Assignment(std::make_unique<Term>(
                                                  assignment_lhs_->Clone()))
                                 .Combine(assignment_rhs_->Clone()));
        } else if (IsSequence()) {
            return std::move(
                Term::Sequence(std::make_unique<Term>(sequence_lhs_->Clone()))
                    .Combine(sequence_rhs_->Clone()));
        } else if (IsProjection()) {
            return std::move(Term::Projection(
                std::make_unique<Term>(projection_term_->Clone()),
                projection_label_));
        } else if (IsLet()) {
            return std::move(Term::Let(let_binding_name_)
                                 .Combine(let_bound_term_->Clone())
                                 .Combine(let_body_term_->Clone()));
        } else if (IsFix()) {
            return std::move(Term::FixTerm().Combine(fix_term_->Clone()));
        } else if (IsIf()) {
            return std::move(Term::If()
                                 .Combine(if_condition_->Clone())
                                 .Combine(if_then_->Clone())
                                 .Combine(if_else_->Clone()));
        }

        std::ostringstream error_ss;
        error_ss << "Couldn't clone term: " << *this;
        throw std::logic_error(error_ss.str());
    }

    bool IsRecordExpectingLabel() const {
        if (!IsRecord()) {
            throw std::logic_error("Expected a Record.");
        }

        int diff = record_labels_.size() - record_terms_.size();
        assert(diff == 0 || diff == 1);

        return diff == 0;
    }

    bool IsRecordExpectingTerm() const {
        if (!IsRecord()) {
            throw std::logic_error("Expected a Record.");
        }

        int diff = record_labels_.size() - record_terms_.size();
        assert(diff == 0 || diff == 1);

        return diff == 1;
    }

    void SetName(std::string name) { name_ = std::move(name); }

    std::string GetName() const { return name_; }

   private:
    enum Category category_ = Category::EMPTY;
    std::string name_;
    bool is_complete_ = false;

    std::string lambda_arg_name_ = "";
    Type *lambda_arg_type_ = nullptr;
    std::unique_ptr<Term> lambda_body_{};
    // Marks whether parsing for the body of the lambda term is finished or not.

    std::string variable_name_ = "";
    int de_bruijn_idx_ = -1;

    std::unique_ptr<Term> application_lhs_{};
    std::unique_ptr<Term> application_rhs_{};

    std::unique_ptr<Term> if_condition_{};
    std::unique_ptr<Term> if_then_{};
    std::unique_ptr<Term> if_else_{};

    std::unique_ptr<Term> unary_op_arg_{};

    std::vector<std::string> record_labels_{};
    std::vector<std::unique_ptr<Term>> record_terms_{};

    std::unique_ptr<Term> projection_term_{};
    std::string projection_label_ = "";

    std::string let_binding_name_ = "";
    std::unique_ptr<Term> let_bound_term_{};
    std::unique_ptr<Term> let_body_term_{};

    std::unique_ptr<Term> ref_term_{};

    std::unique_ptr<Term> deref_term_{};

    std::unique_ptr<Term> assignment_lhs_{};
    std::unique_ptr<Term> assignment_rhs_{};

    int store_location_;

    std::unique_ptr<Term> sequence_lhs_{};
    std::unique_ptr<Term> sequence_rhs_{};

    std::unique_ptr<Term> parenthesized_term_{};

    std::unique_ptr<Term> fix_term_{};
};

std::ostream &operator<<(std::ostream &out, const Term &term) {
    if (term.IsInvalid()) {
        out << "<INVALID>";
    } else if (term.IsVariable()) {
        out << term.variable_name_;
    } else if (term.IsLambda()) {
        out << "{l " << term.lambda_arg_name_ << " : " << *term.lambda_arg_type_
            << ". " << *term.lambda_body_ << "}";
    } else if (term.IsApplication()) {
        out << *term.application_lhs_ << " <- " << *term.application_rhs_;
    } else if (term.IsIf()) {
        out << "if " << *term.if_condition_ << " then " << *term.if_then_
            << " else " << *term.if_else_;
    } else if (term.IsTrue()) {
        out << "true";
    } else if (term.IsFalse()) {
        out << "false";
    } else if (term.IsSucc()) {
        out << "succ " << *term.unary_op_arg_;
    } else if (term.IsPred()) {
        out << "pred " << *term.unary_op_arg_;
    } else if (term.IsIsZero()) {
        out << "iszero " << *term.unary_op_arg_;
    } else if (term.IsConstantZero()) {
        out << "0";
    } else if (term.IsRecord()) {
        out << "{";

        for (int i = 0; i < term.record_terms_.size(); ++i) {
            if (i > 0) {
                out << ", ";
            }

            out << term.record_labels_[i] << "=" << *term.record_terms_[i];
        }

        out << "}";
    } else if (term.IsProjection()) {
        out << *term.projection_term_ << "." << term.projection_label_;
    } else if (term.IsLet()) {
        out << "let " << term.let_binding_name_ << " = "
            << *term.let_bound_term_ << " in " << *term.let_body_term_;
    } else if (term.IsRef()) {
        out << "ref " << *term.ref_term_;
    } else if (term.IsDeref()) {
        out << "!" << *term.deref_term_;
    } else if (term.IsAssignment()) {
        out << *term.assignment_lhs_ << " := " << *term.assignment_rhs_;
    } else if (term.IsUnit()) {
        out << "unit";
    } else if (term.IsStoreLocation()) {
        out << "l[" << term.store_location_ << "]";
    } else if (term.IsSequence()) {
        out << "" << *term.sequence_lhs_ << "; " << *term.sequence_rhs_;
    } else if (term.IsParenthesized()) {
        out << "(" << *term.parenthesized_term_ << ")";
    } else if (term.IsFix()) {
        out << "fix " << *term.fix_term_;
    } else {
        out << "<ERROR>";
    }

    return out;
}

class Parser {
    using Token = lexer::Token;

   public:
    Parser(std::istringstream &&in) : lexer_(std::move(in)) {}

    using TermStack = std::vector<Term>;

    Term ParseStatement() {
        std::string statement_name{""};
        {
            // Check if this is a named statement. A names statement is one of
            // the form:
            //   id = term
            // The name of the statement can be used in later statements/terms
            // where it is expanded/substituted into the those statements.
            auto token = lexer_.NextToken();

            if (token.GetCategory() == Token::Category::IDENTIFIER) {
                auto token2 = lexer_.NextToken();

                if (token2.GetCategory() == Token::Category::EQUAL) {
                    statement_name = token.GetText();
                } else {
                    lexer_.PutBackToken();
                    lexer_.PutBackToken();
                }
            } else {
                lexer_.PutBackToken();
            }
        }

        Token next_token;
        TermStack term_stack;
        term_stack.emplace_back(Term());
        int balance_parens = 0;
        // Contains a list of bound variables in order of binding. For example,
        // for a term λ x. λ y. x y, this list would eventually contains {"x" ,
        // "y"} in that order. This is used to assign de Bruijn indices/static
        // distances to bound variables (ref: tapl,§6.1).
        std::vector<std::string> bound_variables;

        while ((next_token = lexer_.NextToken()).GetCategory() !=
               Token::Category::MARKER_END) {
            switch (next_token.GetCategory()) {
                case Token::Category::LAMBDA: {
                    auto lambda_arg = ParseLambdaArg();
                    auto lambda_arg_name = lambda_arg.first;
                    bound_variables.push_back(lambda_arg_name);

                    // If the current stack top is empty, use its slot for the
                    // lambda.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() =
                            Term::Lambda(lambda_arg_name, lambda_arg.second);
                    } else {
                        // Else, push a new term on the stack to start building
                        // the lambda term.
                        term_stack.emplace_back(
                            Term::Lambda(lambda_arg_name, lambda_arg.second));
                    }

                    // Start a new empty term to build the lambda body.
                    term_stack.emplace_back(Term());

                    break;
                }

                case Token::Category::IDENTIFIER: {
                    switch (CalculateIdentifierSubCategoryFromContext(
                        next_token, term_stack)) {
                        case Token::IdentitySubCategory::RECORD_LABEL: {
                            term_stack.back().AddRecordLabel(
                                next_token.GetText());
                            break;
                        }

                        case Token::IdentitySubCategory::VARIABLE: {
                            auto bound_variable_it =
                                std::find(std::rbegin(bound_variables),
                                          std::rend(bound_variables),
                                          next_token.GetText());
                            int de_bruijn_idx = -1;

                            if (bound_variable_it !=
                                std::rend(bound_variables)) {
                                de_bruijn_idx =
                                    std::distance(std::rbegin(bound_variables),
                                                  bound_variable_it);
                            } else {
                                // The naming context for free variables (ref:
                                // tapl,§6.1.2) is computed from the ASCII code
                                // of a identifier's name.
                                de_bruijn_idx =
                                    ComputeDeBruijnIdx(next_token.GetText(),
                                                       bound_variables.size());
                            }

                            auto peeked_token = lexer_.NextToken();
                            lexer_.PutBackToken();

                            if (peeked_token.GetCategory() ==
                                Token::Category::DOT) {
                                // Add a term to the stack that will potentially
                                // be turned into a projection.
                                if (term_stack.back().IsEmpty()) {
                                    term_stack.back() = Term::Variable(
                                        next_token.GetText(), de_bruijn_idx);
                                } else {
                                    term_stack.emplace_back(Term::Variable(
                                        next_token.GetText(), de_bruijn_idx));
                                }
                            } else {
                                term_stack.back().Combine(Term::Variable(
                                    next_token.GetText(), de_bruijn_idx));
                            }

                            break;
                        }
                    }

                    break;
                }

                case Token::Category::KEYWORD_IF: {
                    // If the current stack top is empty, use its slot for the
                    // if condition.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::If();
                    } else {
                        // Else, push a new term on the stack to start building
                        // the if condition.
                        term_stack.emplace_back(Term::If());
                    }

                    term_stack.emplace_back(Term());
                    ++balance_parens;

                    break;
                }

                case Token::Category::KEYWORD_THEN: {
                    UnwindStack(term_stack, bound_variables,
                                Term::Category::IF);

                    --balance_parens;

                    if (!term_stack.back().IsIf()) {
                        throw std::invalid_argument("Unexpected 'then'");
                    }

                    term_stack.emplace_back(Term());
                    ++balance_parens;

                    break;
                }

                case Token::Category::KEYWORD_ELSE: {
                    UnwindStack(term_stack, bound_variables,
                                Term::Category::IF);

                    --balance_parens;

                    if (!term_stack.back().IsIf()) {
                        throw std::invalid_argument("Unexpected 'else'");
                    }

                    break;
                }

                case Token::Category::KEYWORD_SUCC: {
                    // If the current stack top is empty, use its slot for
                    // the succ term.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::Succ();
                    } else {
                        // Else, push a new term on the stack to start
                        // building the if condition.
                        term_stack.emplace_back(Term::Succ());
                    }

                    break;
                }

                case Token::Category::KEYWORD_PRED: {
                    // If the current stack top is empty, use its slot for
                    // the pred term.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::Pred();
                    } else {
                        // Else, push a new term on the stack to start
                        // building the if condition.
                        term_stack.emplace_back(Term::Pred());
                    }

                    break;
                }

                case Token::Category::KEYWORD_ISZERO: {
                    // If the current stack top is empty, use its slot for
                    // the iszero term.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::IsZero();
                    } else {
                        // Else, push a new term on the stack to start
                        // building the if condition.
                        term_stack.emplace_back(Term::IsZero());
                    }

                    break;
                }

                case Token::Category::OPEN_PAREN: {
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::ParenthesizedTerm();
                    } else {
                        term_stack.emplace_back(Term::ParenthesizedTerm());
                    }

                    term_stack.emplace_back(Term());
                    ++balance_parens;

                    break;
                }

                case Token::Category::CLOSE_PAREN: {
                    UnwindStack(term_stack, bound_variables,
                                Term::Category::PARENTHESIZED);

                    --balance_parens;

                    break;
                }

                case Token::Category::CONSTANT_TRUE: {
                    term_stack.back().Combine(Term::True());

                    break;
                }

                case Token::Category::CONSTANT_FALSE: {
                    term_stack.back().Combine(Term::False());

                    break;
                }

                case Token::Category::CONSTANT_ZERO: {
                    term_stack.back().Combine(Term::Zero());

                    break;
                }

                case Token::Category::OPEN_BRACE: {
                    // If the current stack top is empty, use its slot for
                    // the record term.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::Record();
                    } else {
                        // Else, push a new term on the stack to start
                        // building the if condition.
                        term_stack.emplace_back(Term::Record());
                    }

                    break;
                }

                case Token::Category::EQUAL: {
                    if (!term_stack.back().IsRecord() ||
                        !term_stack.back().IsRecordExpectingTerm()) {
                        throw std::invalid_argument("Unexpected =");
                    }

                    term_stack.emplace_back(Term());
                    ++balance_parens;

                    break;
                }

                case Token::Category::COMMA:
                case Token::Category::CLOSE_BRACE: {
                    UnwindStack(term_stack, bound_variables,
                                Term::Category::RECORD);

                    --balance_parens;

                    if (!term_stack.back().IsRecord()) {
                        throw std::invalid_argument("Unexpected }");
                    }

                    if (next_token.GetCategory() ==
                        Token::Category::CLOSE_BRACE) {
                        term_stack.back().MarkAsComplete();
                    }

                    break;
                }

                case Token::Category::DOT: {
                    if (term_stack.empty() || term_stack.back().IsInvalid()) {
                        throw std::invalid_argument("Unexpected '.'");
                    }

                    Token new_token = lexer_.NextToken();

                    if (new_token.GetCategory() !=
                        Token::Category::IDENTIFIER) {
                        throw std::invalid_argument("Expected a label.");
                    }

                    term_stack.back().ConvertToProjection(new_token.GetText());
                    break;
                }

                case Token::Category::KEYWORD_LET: {
                    Token new_token = lexer_.NextToken();

                    if (new_token.GetCategory() !=
                        Token::Category::IDENTIFIER) {
                        throw std::invalid_argument(
                            "Expected let-binding name.");
                    }

                    bound_variables.push_back(new_token.GetText());

                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::Let(new_token.GetText());
                    } else {
                        term_stack.emplace_back(Term::Let(new_token.GetText()));
                    }

                    new_token = lexer_.NextToken();

                    if (new_token.GetCategory() != Token::Category::EQUAL) {
                        throw std::invalid_argument(
                            "Expected '=' for let-binding.");
                    }

                    term_stack.emplace_back(Term());
                    ++balance_parens;

                    break;
                }

                case Token::Category::KEYWORD_IN: {
                    UnwindStack(term_stack, bound_variables,
                                Term::Category::LET);

                    --balance_parens;

                    if (!term_stack.back().IsLet()) {
                        throw std::invalid_argument("Unexpected 'in'");
                    }

                    // Start a new empty term to build the let body.
                    term_stack.emplace_back(Term());

                    break;
                }

                case Token::Category::KEYWORD_REF: {
                    // If the current stack top is empty, use its slot for
                    // the ref term.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::Ref();
                    } else {
                        // Else, push a new term on the stack to start
                        // building the ref term.
                        term_stack.emplace_back(Term::Ref());
                    }

                    break;
                }

                case Token::Category::EXCLAMATION: {
                    // If the current stack top is empty, use its slot for
                    // the de-ref term.
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::Deref();
                    } else {
                        // Else, push a new term on the stack to start
                        // building the de-ref term.
                        term_stack.emplace_back(Term::Deref());
                    }

                    break;
                }

                case Token::Category::ASSIGN: {
                    if (term_stack.empty() || term_stack.back().IsInvalid()) {
                        throw std::invalid_argument("Unexpected ':='");
                    }

                    term_stack.back().ConvertToAssignment();
                    term_stack.emplace_back(Term());

                    break;
                }

                case Token::Category::CONSTANT_UNIT: {
                    term_stack.back().Combine(Term::Unit());

                    break;
                }

                case Token::Category::SEMICOLON: {
                    if (term_stack.empty() || term_stack.back().IsInvalid()) {
                        throw std::invalid_argument("Unexpected ';'");
                    }

                    term_stack.back().ConvertToSequence();
                    term_stack.emplace_back(Term());

                    break;
                }

                case Token::Category::KEYWORD_FIX: {
                    if (term_stack.back().IsEmpty()) {
                        term_stack.back() = Term::FixTerm();
                    } else {
                        term_stack.emplace_back(Term::FixTerm());
                    }

                    break;
                }

                default: {
                    std::ostringstream error_ss;
                    error_ss << __LINE__ << " Unexpected token: " << next_token;
                    throw std::invalid_argument(error_ss.str());
                }
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

        term_stack.back().SetName(statement_name);
        return std::move(term_stack.back());
    }

    void UnwindStack(TermStack &term_stack,
                     std::vector<std::string> &bound_variables,
                     enum Term::Category target_cat) {
        do {
            if (term_stack.back().IsLambda()) {
                // λ's variable is no longer part of the current binding
                // context, therefore pop it.
                bound_variables.pop_back();
            }

            if (term_stack.back().IsLet()) {
                // let-binding's variable is no longer part of the current
                // binding context, therefore pop it.
                bound_variables.pop_back();
            }

            CombineStackTop(term_stack);
        } while (term_stack.size() > 1 &&
                 term_stack.back().Category() != target_cat);
    }

    void CombineStackTop(std::vector<Term> &term_stack) {
        if (term_stack.size() < 2) {
            throw std::invalid_argument(
                "Invalid term: probably because a ( is not matched by a )");
        }

        Term top = std::move(term_stack.back());
        term_stack.pop_back();
        term_stack.back().Combine(std::move(top));
    }

    std::pair<std::string, Type &> ParseLambdaArg() {
        auto token = lexer_.NextToken();

        if (token.GetCategory() != Token::Category::IDENTIFIER) {
            throw std::invalid_argument("Expected to parse a variable.");
        }

        auto arg_name = token.GetText();
        token = lexer_.NextToken();

        if (token.GetCategory() != Token::Category::COLON) {
            throw std::invalid_argument("Expected to parse a ':'.");
        }

        Type &type = ParseType();
        token = lexer_.NextToken();

        if (token.GetCategory() != Token::Category::DOT) {
            throw std::invalid_argument("Expected to parse a '.'.");
        }

        return {arg_name, type};
    }

    Type &ParseType() {
        std::vector<Type *> parts;
        while (true) {
            auto token = lexer_.NextToken();

            if (token.GetCategory() == Token::Category::KEYWORD_BOOL) {
                parts.emplace_back(&Type::Bool());
            } else if (token.GetCategory() == Token::Category::KEYWORD_NAT) {
                parts.emplace_back(&Type::Nat());
            } else if (token.GetCategory() ==
                       Token::Category::KEYWORD_UNIT_TYPE) {
                parts.emplace_back(&Type::Unit());
            } else if (token.GetCategory() == Token::Category::OPEN_PAREN) {
                parts.emplace_back(&ParseType());

                if (lexer_.NextToken().GetCategory() !=
                    Token::Category::CLOSE_PAREN) {
                    std::ostringstream error_ss;
                    error_ss << __LINE__ << ": Unexpected token: " << token;
                    throw std::invalid_argument(error_ss.str());
                }
            } else if (token.GetCategory() == Token::Category::OPEN_BRACE) {
                lexer_.PutBackToken();
                parts.emplace_back(&ParseRecordType());
            } else if (token.GetCategory() ==
                       Token::Category::KEYWORD_REF_TYPE) {
                parts.emplace_back(&Type::Ref(ParseType()));
            } else {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::invalid_argument(error_ss.str());
            }

            token = lexer_.NextToken();

            if (token.GetCategory() == Token::Category::DOT) {
                lexer_.PutBackToken();
                break;
            } else if (token.GetCategory() == Token::Category::CLOSE_PAREN ||
                       token.GetCategory() == Token::Category::CLOSE_BRACE ||
                       token.GetCategory() == Token::Category::COMMA) {
                lexer_.PutBackToken();
                break;
            } else if (token.GetCategory() != Token::Category::ARROW) {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::invalid_argument(error_ss.str());
            }
        }

        // Types are right-associative, combine them accordingly.
        for (int i = parts.size() - 2; i >= 0; --i) {
            parts[i] = &Type::Function(*parts[i], *parts[i + 1]);
        }

        return *parts[0];
    }

    Type &ParseRecordType() {
        Token token = lexer_.NextToken();

        if (token.GetCategory() != Token::Category::OPEN_BRACE) {
            std::ostringstream error_ss;
            error_ss << __LINE__ << ": Unexpected token: " << token;
            throw std::invalid_argument(error_ss.str());
        }

        Type::RecordFields fields;
        token = lexer_.NextToken();

        while (true) {
            if (token.GetCategory() != Token::Category::IDENTIFIER) {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::invalid_argument(error_ss.str());
            }

            std::string field_id = token.GetText();

            token = lexer_.NextToken();

            if (token.GetCategory() != Token::Category::COLON) {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::invalid_argument(error_ss.str());
            }

            Type &type = ParseType();
            token = lexer_.NextToken();
            fields.insert({field_id, type});

            if (token.GetCategory() == Token::Category::CLOSE_BRACE) {
                break;
            } else if (token.GetCategory() == Token::Category::COMMA) {
                token = lexer_.NextToken();
                continue;
            } else {
                std::ostringstream error_ss;
                error_ss << __LINE__ << ": Unexpected token: " << token;
                throw std::invalid_argument(error_ss.str());
            }
        }

        if (fields.empty()) {
            std::ostringstream error_ss;
            error_ss << __LINE__ << ": Invalid record.";
            throw std::invalid_argument(error_ss.str());
        }

        return Type::Record(std::move(fields));
    }

    Token::IdentitySubCategory CalculateIdentifierSubCategoryFromContext(
        Token token, const std::vector<Term> &term_stack) {
        assert(token.GetCategory() == Token::Category::IDENTIFIER);

        if (term_stack.back().IsRecord() &&
            term_stack.back().IsRecordExpectingLabel()) {
            return Token::IdentitySubCategory::RECORD_LABEL;
        }

        return Token::IdentitySubCategory::VARIABLE;
    }

   private:
    int ComputeDeBruijnIdx(std::string id_name, int offset) {
        int idx = offset;

        for (char c : id_name) {
            if (std::isalpha(c)) {
                idx += (std::tolower(c) - 'a');
            } else if (std::isdigit(c)) {
                idx += (std::tolower(c) - '0' + 26);
            } else if (c == '_') {
                idx += 36;
            }
        }

        return idx;
    }

   private:
    lexer::Lexer lexer_;
};
}  // namespace parser

namespace {
using Term = parser::Term;

bool IsNatValue(const Term &term) {
    return term.IsConstantZero() ||
           (term.IsSucc() && IsNatValue(term.UnaryOpArg()));
}

bool IsValue(const Term &term);

bool IsRecordValue(const Term &term) {
    if (!term.IsRecord()) {
        return false;
    }

    for (auto &record_term : term.RecordTerms()) {
        if (!IsValue(*record_term)) {
            return false;
        }
    }

    return true;
}

bool IsValue(const Term &term) {
    return term.IsLambda() || term.IsTrue() || term.IsFalse() ||
           IsNatValue(term) || IsRecordValue(term) || term.IsUnit() ||
           term.IsStoreLocation();
}

int ConvertNatValueToDecimal(const Term &nat_val) {
    if (!IsNatValue(nat_val)) {
        throw std::logic_error("Trying to convert a non-Nat value to decimal.");
    }

    std::ostringstream ss;
    ss << nat_val;

    auto term_str = ss.str();

    std::size_t start_pos = 0;
    int num = 0;

    while ((start_pos = term_str.find("succ", start_pos)) !=
           std::string::npos) {
        ++num;
        ++start_pos;
    }

    return num;
}

Term ConvertDecimalToNatValue(int dec_val) {
    std::ostringstream res;

    for (int i = 0; i < dec_val; ++i) {
        res << "succ ";
    }

    res << "0";

    parser::Parser p(std::move(std::istringstream(res.str())));

    return p.ParseStatement();
}
}  // namespace

namespace runtime {

class Store {
    using Term = parser::Term;
    using Type = parser::Type;

   public:
    int StoreValue(const Term &term, const Type &type) {
        int location = first_free_store_location_;
        store_.resize(store_.size() + type.StorageSize());
        StoreValue(term, type, location);
        first_free_store_location_ = location + type.StorageSize();
        store_location_to_type_[location] = &type;

        return location;
    }

    int StoreValue(const Term &term, const Type &type, int location) {
        if (term.IsFalse()) {
            store_[location] = 0;
        } else if (term.IsTrue()) {
            store_[location] = 1;
        } else if (term.IsUnit()) {
            store_[location] = 2;
        } else if (IsNatValue(term)) {
            int convereted_term = ConvertNatValueToDecimal(term);

            for (int i = 0; i < type.StorageSize(); ++i) {
                store_[location + i] =
                    static_cast<unsigned char>(convereted_term & 0xFF);
                convereted_term >>= 8;
            }

        } else if (term.IsLambda()) {
            int stored_lambda_index = stored_lambdas_.size();
            stored_lambdas_.emplace_back(std::make_unique<Term>(term.Clone()));

            for (int i = 0; i < type.StorageSize(); ++i) {
                store_[location + i] =
                    static_cast<unsigned char>(stored_lambda_index & 0xFF);
                stored_lambda_index >>= 8;
            }
        } else if (IsRecordValue(term)) {
            const auto &record_fields = type.GetRecordFields();
            // Reserve 1 byte to properly handle records whose first element is
            // also a record (TODO verify with a test case and document
            // properly).
            int field_location = location + 1;
            store_location_to_record_lables_[location].clear();

            for (int i = 0; i < term.RecordTerms().size(); ++i) {
                std::string element_label = term.RecordLabels()[i];
                Term *element_term = &*term.RecordTerms()[i];

                store_location_to_record_lables_[location].emplace_back(
                    element_label);

                StoreValue(*element_term, record_fields.at(element_label),
                           field_location);
                field_location += record_fields.at(element_label).StorageSize();
            }
        } else {
            std::ostringstream ss;
            ss << "Trying to store a non-value term: " << term;
            throw std::invalid_argument(ss.str());
        }

        return location;
    }

    Term RetrieveValue(int store_location, const Type &type) {
        if (type.IsBool()) {
            if (store_.at(store_location) == 0) {
                return Term::False();
            } else if (store_[store_location] == 1) {
                return Term::True();
            }
        } else if (type.IsUnit()) {
            if (store_.at(store_location) == 2) {
                return Term::Unit();
            }
        } else if (type.IsNat()) {
            int stored_value_in_decimal = 0;

            for (int i = 0; i < type.StorageSize(); ++i) {
                stored_value_in_decimal |=
                    (store_.at(store_location + i) << (i * 8));
            }

            return ConvertDecimalToNatValue(stored_value_in_decimal);
        } else if (type.IsFunction()) {
            unsigned long stored_lambda_index = 0;

            for (int i = 0; i < type.StorageSize(); ++i) {
                stored_lambda_index |=
                    (store_.at(store_location + i) << (i * 8));
            }

            Term *res = reinterpret_cast<Term *>(
                stored_lambdas_[stored_lambda_index].get());

            return res->Clone();
        } else if (type.IsRecord()) {
            Term res = Term::Record();
            const auto &record_type_fields = type.GetRecordFields();
            int field_location_start = store_location + 1;

            for (std::string label :
                 store_location_to_record_lables_[store_location]) {
                res.AddRecordLabel(label);
                res.Combine(RetrieveValue(field_location_start,
                                          record_type_fields.at(label)));

                field_location_start +=
                    record_type_fields.at(label).StorageSize();
            }

            return res;
        }

        std::ostringstream ss;
        ss << "Unexpected store value: " << store_[store_location]
           << " at location: " << store_location;
        throw std::logic_error(ss.str());
    }

    const Type &GetStoredValueType(int value_location) const {
        return *store_location_to_type_.at(value_location);
    }

   private:
    // Provides a store for referenced values. The store is little endian, a
    // multi-byte value store with its least significant byte first and its most
    // significant byte last.
    std::vector<unsigned char> store_;
    int first_free_store_location_ = 0;
    std::unordered_map<int, const Type *> store_location_to_type_;

    std::unordered_map<int, std::vector<std::string>>
        store_location_to_record_lables_;

    std::vector<std::unique_ptr<Term>> stored_lambdas_;
};

class NamedStatementStore {
    using Type = parser::Type;
    using Term = parser::Term;

   public:
    void AddNamedTerm(const std::string &name, Term term, Type &type) {
        names_to_terms_map_[name] = std::move(term);
        names_to_types_map_[name] = &type;
    }

    bool ContainsName(const std::string &name) const {
        return names_to_terms_map_.find(name) != std::end(names_to_terms_map_);
    }

    const Term &GetStatement(const std::string &name) const {
        return names_to_terms_map_.at(name);
    }

    Type &GetType(const std::string &name) const {
        return *names_to_types_map_.at(name);
    }

   private:
    std::unordered_map<std::string, Term> names_to_terms_map_;
    std::unordered_map<std::string, Type *> names_to_types_map_;
};

}  // namespace runtime

namespace type_checker {
using parser::Term;
using parser::Type;

class TypeChecker {
    using Context = std::deque<std::pair<std::string, Type *>>;

   public:
    Type &TypeOf(const runtime::NamedStatementStore &named_statement_store,
                 const Term &term) {
        Context ctx;
        return TypeOf(named_statement_store, ctx, term);
    }

    /**
     * Determines if \p s is a sub-type of \p t.
     *
     * @return true of \p s is sub-type of \p t, false otherwise.
     */
    bool IsSubtype(const Type &s, const Type &t) {
        if (s == t) {
            return true;
        }

        if (s.IsRecord() && t.IsRecord()) {
            for (const auto &t_field : t.GetRecordFields()) {
                bool found_t_field_in_s = false;

                for (const auto &s_field : s.GetRecordFields()) {
                    if (t_field.first == s_field.first &&
                        IsSubtype(s_field.second, t_field.second)) {
                        found_t_field_in_s = true;
                        break;
                    }
                }

                if (!found_t_field_in_s) {
                    return false;
                }
            }

            return true;
        }

        if (s.IsFunction() && t.IsFunction()) {
            return IsSubtype(t.FunctionLHS(), s.FunctionLHS()) &&
                   IsSubtype(s.FunctionRHS(), t.FunctionRHS());
        }

        return false;
    }

    /**
     * Calculates the join (least common supertype) of \p s and \p t.
     *
     * For example, if s == {x:Nat, y:Bool}, and t == {x:Nat, z:Bool}, then the
     * method returns {x:Nat}.
     */
    Type &Join(Type &s, Type &t) {
        if (s == t) {
            return s;
        }

        if (s.IsFunction() && t.IsFunction()) {
            Type &s1 = s.FunctionLHS();
            Type &s2 = s.FunctionRHS();
            Type &t1 = t.FunctionLHS();
            Type &t2 = t.FunctionRHS();

            Type &m1 = Meet(s1, t1);
            Type &j2 = Join(s2, t2);

            if (m1.IsIllTyped()) {
                return Type::IllTyped();
            }

            return Type::Function(m1, j2);
        }

        if (s.IsRecord() && t.IsRecord()) {
            Type::RecordFields join_fields;

            for (auto &s_field : s.GetRecordFields()) {
                auto t_iter = t.GetRecordFields().find(s_field.first);

                if (t_iter != std::end(t.GetRecordFields())) {
                    join_fields.insert(
                        {s_field.first, Join(s_field.second, t_iter->second)});
                }
            }

            return Type::Record(join_fields);
        }

        // Top is the last resort supertype of all types.
        return Type::Top();
    }

    /**
     * Calculates the join (greatest common subtype) of \p s and \p t.
     *
     * For example, if s == {x:Nat, y:Bool}, and t == {x:Nat, z:Bool}, then the
     * method returns {x:Nat, y:Bool, z:Bool}.

     */
    Type &Meet(Type &s, Type &t) {
        if (s == t) {
            return s;
        }

        if (s.IsTop()) {
            return t;
        }

        if (t.IsTop()) {
            return s;
        }

        if (s.IsFunction() && t.IsFunction()) {
            Type &s1 = s.FunctionLHS();
            Type &s2 = s.FunctionRHS();
            Type &t1 = t.FunctionLHS();
            Type &t2 = t.FunctionRHS();

            Type &j1 = Join(s1, t1);
            Type &m2 = Meet(s2, t2);

            if (m2.IsIllTyped()) {
                return Type::IllTyped();
            }

            return Type::Function(j1, m2);
        }

        if (s.IsRecord() && t.IsRecord()) {
            Type::RecordFields meet_fields;

            for (auto &s_field : s.GetRecordFields()) {
                auto t_iter = t.GetRecordFields().find(s_field.first);

                if (t_iter != std::end(t.GetRecordFields())) {
                    Type &m = Meet(s_field.second, t_iter->second);

                    if (m.IsIllTyped()) {
                        return Type::IllTyped();
                    }

                    meet_fields.insert({s_field.first, m});
                } else {
                    meet_fields.insert({s_field.first, s_field.second});
                }
            }

            for (auto &t_field : t.GetRecordFields()) {
                auto s_iter = s.GetRecordFields().find(t_field.first);

                if (s_iter == std::end(s.GetRecordFields())) {
                    meet_fields.insert({t_field.first, t_field.second});
                }
            }

            return Type::Record(meet_fields);
        }

        return Type::IllTyped();
    }

   private:
    Type &TypeOf(const runtime::NamedStatementStore &named_statement_store,
                 const Context &ctx, const Term &term) {
        Type *res = &Type::IllTyped();

        if (term.IsTrue() || term.IsFalse()) {
            res = &Type::Bool();
        } else if (term.IsConstantZero()) {
            res = &Type::Nat();
        } else if (term.IsIf()) {
            if (TypeOf(named_statement_store, ctx, term.IfCondition()) ==
                Type::Bool()) {
                Type &then_type =
                    TypeOf(named_statement_store, ctx, term.IfThen());
                Type &else_type =
                    TypeOf(named_statement_store, ctx, term.IfElse());
                return Join(then_type, else_type);
            }
        } else if (term.IsSucc() || term.IsPred()) {
            auto &subterm_type =
                TypeOf(named_statement_store, ctx, term.UnaryOpArg());

            if (subterm_type == Type::Nat()) {
                res = &Type::Nat();
            }
        } else if (term.IsIsZero()) {
            auto &subterm_type =
                TypeOf(named_statement_store, ctx, term.UnaryOpArg());

            if (subterm_type == Type::Nat()) {
                res = &Type::Bool();
            }
        } else if (term.IsLambda()) {
            Context new_ctx =
                AddBinding(ctx, term.LambdaArgName(), term.LambdaArgType());
            Type &return_type =
                TypeOf(named_statement_store, new_ctx, term.LambdaBody());
            res = &Type::Function(term.LambdaArgType(), return_type);
        } else if (term.IsLet()) {
            Context new_ctx =
                AddBinding(ctx, term.LetBindingName(), Type::IllTyped());
            Type &let_bound_type =
                TypeOf(named_statement_store, new_ctx, term.LetBoundTerm());
            new_ctx[0].second = &let_bound_type;
            res = &TypeOf(named_statement_store, new_ctx, term.LetBodyTerm());
        } else if (term.IsApplication()) {
            Type &lhs_type =
                TypeOf(named_statement_store, ctx, term.ApplicationLHS());
            Type &rhs_type =
                TypeOf(named_statement_store, ctx, term.ApplicationRHS());

            if (lhs_type.IsFunction() &&
                IsSubtype(rhs_type, lhs_type.FunctionLHS())) {
                res = &lhs_type.FunctionRHS();
            }
        } else if (term.IsVariable()) {
            int idx = term.VariableDeBruijnIdx();

            if (idx >= 0 && idx < ctx.size() &&
                ctx[idx].first == term.VariableName()) {
                res = ctx[idx].second;
            } else {
                // Check if the free variable represents a named statement
                if (named_statement_store.ContainsName(term.VariableName())) {
                    // If so, get the type of that statement
                    res = &named_statement_store.GetType(term.VariableName());
                }
            }
        } else if (term.IsRecord()) {
            Type::RecordFields record_type_fields;

            for (int i = 0; i < term.RecordLabels().size(); ++i) {
                Type &field_type =
                    TypeOf(named_statement_store, ctx, *term.RecordTerms()[i]);

                if (field_type.IsIllTyped()) {
                    break;
                }

                record_type_fields.insert({term.RecordLabels()[i], field_type});
            }

            if (record_type_fields.size() == term.RecordLabels().size()) {
                res = &Type::Record(record_type_fields);
            }
        } else if (term.IsProjection()) {
            Type &term_type =
                TypeOf(named_statement_store, ctx, term.ProjectionTerm());

            if (term_type.IsRecord()) {
                for (auto &field : term_type.GetRecordFields()) {
                    if (field.first == term.ProjectionLabel()) {
                        res = &field.second;
                        break;
                    }
                }
            }
        } else if (term.IsUnit()) {
            res = &Type::Unit();
        } else if (term.IsRef()) {
            Type &ref_term_type =
                TypeOf(named_statement_store, ctx, term.RefTerm());

            if (!ref_term_type.IsIllTyped()) {
                res = &Type::Ref(ref_term_type);
            }
        } else if (term.IsDeref()) {
            Type &deref_term_type =
                TypeOf(named_statement_store, ctx, term.DerefTerm());

            if (deref_term_type.IsRef()) {
                res = &deref_term_type.RefType();
            }
        } else if (term.IsAssignment()) {
            Type &lhs_type =
                TypeOf(named_statement_store, ctx, term.AssignmentLHS());
            Type &rhs_type =
                TypeOf(named_statement_store, ctx, term.AssignmentRHS());

            if (lhs_type.IsRef() && lhs_type.RefType() == rhs_type) {
                res = &Type::Unit();
            }
        } else if (term.IsSequence()) {
            Type &lhs_type =
                TypeOf(named_statement_store, ctx, term.SequenceLHS());
            Type &rhs_type =
                TypeOf(named_statement_store, ctx, term.SequenceRHS());

            if (lhs_type.IsUnit()) {
                res = &rhs_type;
            }
        } else if (term.IsParenthesized()) {
            res = &TypeOf(named_statement_store, ctx,
                          term.GetParenthesizedTerm());
        } else if (term.IsFix()) {
            Type &fix_term_type =
                TypeOf(named_statement_store, ctx, term.GetFixTerm());

            if (fix_term_type.IsFunction() &&
                fix_term_type.FunctionLHS() == fix_term_type.FunctionRHS()) {
                res = &fix_term_type.FunctionLHS();
            }
        }

        return *res;
    }

    Context AddBinding(const Context &current_ctx, std::string var_name,
                       Type &type) {
        Context new_ctx = current_ctx;
        new_ctx.push_front({var_name, &type});

        return new_ctx;
    }
};
}  // namespace type_checker

namespace interpreter {
using namespace type_checker;

class Interpreter {
    using Term = parser::Term;

   public:
    std::pair<std::string, Type &> Interpret(Term &statement) {
        Type &type = type_checker_.TypeOf(named_statement_store_, statement);
        std::string statement_name = statement.GetName();

        if (!type.IsIllTyped()) {
            Eval(statement);

            if (!statement_name.empty()) {
                named_statement_store_.AddNamedTerm(statement_name,
                                                    statement.Clone(), type);
            }
        }

        std::ostringstream ss;
        ss << statement;

        auto term_str = ss.str();

        if (IsNatValue(statement)) {
            term_str = std::to_string(ConvertNatValueToDecimal(statement));
        }

        return {term_str, type};
    }

   private:
    void Eval(Term &term) {
        try {
            Eval1(term);
            Eval(term);
        } catch (std::invalid_argument &) {
        }
    }

    void Eval1(Term &term) {
        auto term_subst_top = [](Term &s, Term &t) {
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
        } else if (term.IsLet() && IsValue(term.LetBoundTerm())) {
            term_subst_top(term.LetBoundTerm(), term.LetBodyTerm());
            std::swap(term, term.LetBodyTerm());
        } else if (term.IsLet()) {
            Eval1(term.LetBoundTerm());
        } else if (term.IsIf()) {
            if (term.IfCondition() == Term::True()) {
                std::swap(term, term.IfThen());
            } else if (term.IfCondition() == Term::False()) {
                std::swap(term, term.IfElse());
            } else {
                Eval1(term.IfCondition());
            }
        } else if (term.IsSucc()) {
            Eval1(term.UnaryOpArg());
        } else if (term.IsPred()) {
            auto &pred_arg = term.UnaryOpArg();

            if (pred_arg.IsConstantZero()) {
                std::swap(term, term.UnaryOpArg());
            } else if (pred_arg.IsSucc()) {
                if (IsNatValue(pred_arg)) {
                    std::swap(term, pred_arg.UnaryOpArg());
                } else {
                    Eval1(pred_arg);
                }
            } else {
                Eval1(pred_arg);
            }
        } else if (term.IsIsZero()) {
            auto &iszero_arg = term.UnaryOpArg();

            if (iszero_arg.IsConstantZero()) {
                auto temp = Term::True();
                std::swap(term, temp);
            } else if (iszero_arg.IsSucc()) {
                if (IsNatValue(iszero_arg)) {
                    auto temp = Term::False();
                    std::swap(term, temp);
                } else {
                    Eval1(iszero_arg);
                }
            } else {
                Eval1(iszero_arg);
            }
        } else if (term.IsProjection()) {
            Term &projection_term = term.ProjectionTerm();

            if (IsRecordValue(projection_term)) {
                for (int i = 0; i < projection_term.RecordLabels().size();
                     ++i) {
                    if (projection_term.RecordLabels()[i] ==
                        term.ProjectionLabel()) {
                        std::swap(term, *projection_term.RecordTerms()[i]);

                        break;
                    }
                }
            } else {
                Eval1(projection_term);
            }
        } else if (term.IsRecord() && !IsRecordValue(term)) {
            for (auto &record_term : term.RecordTerms()) {
                if (!IsValue(*record_term)) {
                    Eval1(*record_term);
                    break;
                }
            }
        } else if (term.IsRef() && IsValue(term.RefTerm())) {
            // Allocate a new store location for the value.
            Type &referenced_term_type =
                type_checker_.TypeOf(named_statement_store_, term.RefTerm());
            int store_location =
                store_.StoreValue(term.RefTerm(), referenced_term_type);
            Term new_term = Term::StoreLocation(store_location);
            std::swap(term, new_term);
        } else if (term.IsRef()) {
            Eval1(term.RefTerm());
        } else if (term.IsDeref() && term.DerefTerm().IsStoreLocation()) {
            int store_location = term.DerefTerm().StoreLocationValue();
            Term stored_value = store_.RetrieveValue(
                store_location, store_.GetStoredValueType(store_location));
            std::swap(term, stored_value);
        } else if (term.IsDeref()) {
            Eval1(term.DerefTerm());
        } else if (term.IsAssignment() &&
                   term.AssignmentLHS().IsStoreLocation() &&
                   IsValue(term.AssignmentRHS())) {
            Type &rhs_type = type_checker_.TypeOf(named_statement_store_,
                                                  term.AssignmentRHS());
            store_.StoreValue(term.AssignmentRHS(), rhs_type,
                              term.AssignmentLHS().StoreLocationValue());
            Term unit = Term::Unit();
            std::swap(term, unit);
        } else if (term.IsAssignment() &&
                   term.AssignmentLHS().IsStoreLocation()) {
            Eval1(term.AssignmentRHS());
        } else if (term.IsAssignment()) {
            Eval1(term.AssignmentLHS());
        } else if (term.IsSequence() && term.SequenceLHS().IsUnit()) {
            std::swap(term, term.SequenceRHS());
        } else if (term.IsSequence()) {
            Eval1(term.SequenceLHS());
        } else if (term.IsParenthesized()) {
            std::swap(term, term.GetParenthesizedTerm());
        } else if (term.IsFix() && term.GetFixTerm().IsLambda()) {
            // Term clone = term.Clone();
            term_subst_top(term, term.GetFixTerm().LambdaBody());
            std::swap(term, term.GetFixTerm().LambdaBody());
        } else if (term.IsFix()) {
            Eval1(term.GetFixTerm());
        } else if (term.IsVariable() &&
                   named_statement_store_.ContainsName(term.VariableName())) {
            Term temp = named_statement_store_.GetStatement(term.VariableName())
                            .Clone();
            std::swap(term, temp);
        } else {
            throw std::invalid_argument("No applicable rule.");
        }
    }

   private:
    TypeChecker type_checker_;
    runtime::Store store_;
    runtime::NamedStatementStore named_statement_store_;
};
}  // namespace interpreter
