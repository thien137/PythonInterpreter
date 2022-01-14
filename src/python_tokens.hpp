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
    int position_in_line;
    Token() =default;
    Token(TokenID i, std::string s, int c = 0): id{i}, string_value{s}, position_in_line{c} {}
    Token(TokenID i, double d, int c = 0): id{i}, position_in_line{c} {if (id == TokenID::Integer) integer_value = d; else double_value = d;}
    Token(TokenID i): id{i} {}
};
//

class TokenStream {
    private:
        void close() {if (owner) delete input;};

        const std::string name;
        int current_line = 0;
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
        bool empty() {return line_buf.empty() or (line_buf.size() == 1 and line_buf.front().id == TokenID::End);}
        void print();
        const std::deque<Token>& tokenize_next_line(bool);
        const Token& peek();
        Token& get();
};