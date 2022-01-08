#include "python_helpers.hpp"
#include "python_exceptions.hpp"

enum class TokenID : char {
    Identifier = 0, Integer, Float, String, Operator, Punctuation, WhiteSpace, Comment, End, EndFile
};

// Likely To Change
struct Token {
    TokenID id;
    std::string string_value;
    double double_value;
    int integer_value;

    Token() =default;
    Token(TokenID i, std::string s): id{i}, string_value{s} {}
    Token(TokenID i, double d): id{i} {if (id == TokenID::Integer) integer_value = d; else double_value = d;}
    Token(TokenID i): id{i} {}
};
//

class TokenStream {
    private:
        void close() {if (owner) delete input;};

        const std::string name;
        std::istream* input;
        bool owner;
        std::deque<Token> line_buf {};

        // private helpers
        std::string get_string(std::stringstream&, const char delimiter);
        std::string get_comment(std::stringstream&, const char start);
        std::string get_whitespace(std::stringstream&, const char start);

    public:
        TokenStream(std::istream& s, std::string n) : input{&s}, owner{false}, name {n} {*input >> std::noskipws;}
        TokenStream(std::istream* s, std::string n) : input{s}, owner{true}, name {n} {*input >> std::noskipws;}
        ~TokenStream() {close();}

        void put_back(const Token& t) {line_buf.push_front(t);}
        void print();
        const std::deque<Token>& tokenize_next_line(bool);
        const Token& peek();
        Token& get();
};