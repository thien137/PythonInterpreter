#include "headers/python_tokens.h"

std::string TokenStream::get_string(std::stringstream& in, const char delimiter) {
    // After we get an open '"'
    std::string result {delimiter};
    char pilot;
    while (in.peek() != delimiter and in.peek() != EOF) {
        in.get(pilot);
        result += pilot;
    }
    if (in.peek() == delimiter) {
        in.get(pilot);
        result += pilot;
    }
    else {
        in.get(pilot);
        throw 20;
    }
    return result;
}

std::string TokenStream::get_comment(std::stringstream& in, const char start) {
    // After we get a '#'
    std::string result {start};
    char pilot;
    while (in.get(pilot)) {
        result += pilot;
    }
    return result;
}

std::string TokenStream::get_whitespace(std::stringstream& in, const char start) {
    // After we get a ' ' or '\t'; Collect all the whitespace
    std::string result = {start};
    char pilot;
    while (in.peek() == ' ' or in.peek() == '\t') {
        in.get(pilot);
        if (pilot == '\t') result += "    ";
        else result += pilot;
    }
    return result;
}

const std::vector<Token>& TokenStream::tokenize_next_line(bool continuation) {
    // Tokenize a line and return list of tokens
    if (!continuation) current.clear();

    std::string line = "";
    std::getline(*input, line);

    std::stringstream line_stream(line);
    char pilot;
    std::string string_result = "";
    double integral_result;

    while (line_stream.get(pilot)) {
        switch (pilot) {
            // End of Python Line
            case '\n':
                current.push_back({TokenID::End});
                break;
            // Comment:
            case '#':
                string_result = get_comment(line_stream, pilot);
                current.push_back({TokenID::Comment, string_result});
                break;
            // Whitespace
            case '\t':
            case ' ':
                string_result = get_whitespace(line_stream, pilot);
                current.push_back({TokenID::WhiteSpace, string_result});
                break;
            // Operators
            case '*':
            case '/':
            case '>':
            case '<':
                if (line_stream.peek() == pilot) {
                    string_result += pilot;
                    line_stream.get(pilot);
                }
                break;
            case '+':
            case '-':
            case '&':
            case '|':
            case '%':
            case '^':
            case '=':
                if (line_stream.peek() == '=') {
                    string_result += pilot;
                    line_stream.get(pilot);
                }
                current.push_back({TokenID::Operator, string_result});
                break;
            // Punctuation
            case '\'':
            case '"':
                string_result = get_string(line_stream, pilot);
                current.push_back({TokenID::String, string_result});
                break;
            case '(':
            case ')':
            case '[':
            case ']':
            case '{':
            case '}':
            case ';':
            case ',':
            case ':':
                string_result += pilot;
                current.push_back({TokenID::Punctuation, string_result});
                break;
            // Line Continuation
            case '\\':
                tokenize_next_line(true);
                break;
            // Special Case '.' (Operator or Floating Point Number)
            case '.':
                if (!isdigit(line_stream.peek())) {
                    string_result += pilot;
                    current.push_back({TokenID::Operator, string_result});
                    break;
                }
                else {
                    // Keep going to read as a number!
                }
            default:
                // Identifiers
                if (isalpha(pilot) or pilot == '_') {
                    string_result += pilot;
                    while (isalnum(line_stream.peek()) or pilot == '_') {
                        line_stream.get(pilot);
                        string_result += pilot;
                    }
                    current.push_back({TokenID::Identifier, string_result});
                    break;
                }
                // Numbers
                else if (pilot == '.' or isdigit(pilot)) {
                    line_stream.putback(pilot);
                    line_stream >> integral_result;
                    current.push_back({TokenID::Number, integral_result});
                    break; 
                }
                else {
                    throw 20;
                }
        }
        string_result = "";
    }
    return current;
}
