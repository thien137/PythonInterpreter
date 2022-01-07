#include <iostream>
#include <string>
#include <vector>
#include <deque>
#include <list>
#include <limits>
#include <sstream>
#include <exception>

std::list<std::string> split(std::string);

enum class TokenID : char {
    Identifier, Number, String, Operator, Punctuation, WhiteSpace, Comment, End, EndFile
};

// Likely To Change
struct Token {
    TokenID id;
    std::string string_value;
    double double_value;

    Token(TokenID i, std::string s): id{i}, string_value{s} {}
    Token(TokenID i, double d): id{i}, double_value{d} {}
    Token(TokenID i): id{i} {}
};
//

class TokenStream {
    private:
        void close() {if (owner) delete input;};

        std::istream* input;
        bool owner;
        std::deque<Token> line_buf;
        
        // private helpers
        const std::deque<Token>& tokenize_next_line(bool);
        std::string get_string(std::stringstream&, const char delimiter);
        std::string get_comment(std::stringstream&, const char start);
        std::string get_whitespace(std::stringstream&, const char start);

    public:
        TokenStream(std::istream& s) : input{&s}, owner{false} {*input >> std::noskipws;}
        TokenStream(std::istream* s) : input{s}, owner{true} {*input >> std::noskipws;}
        ~TokenStream() {close();}

        void put_back(Token& t) {line_buf.push_front(t);}
        const Token& peek();
        Token& get();
};