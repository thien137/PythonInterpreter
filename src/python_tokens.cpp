#include "python_tokens.hpp"

void TokenStream::print() {
    std::cout << "Tokens (" << line_buf.size() << "):" << std::endl;
    for (Token& t : line_buf) {
        std::cout << "   ";
        switch (t.id) {
            case TokenID::WhiteSpace:
                std::cout << "TokenID::WhiteSpace:  " << std::endl;
                break;
            case TokenID::Comment:
                std::cout << "TokenID::Comment: " << t.string_value << std::endl;
                break;
            case TokenID::End:
                std::cout << "TokenID::End: " << "\\n" << std::endl;
                break;
            case TokenID::Operator:
                std::cout << "TokenID::Operator: " << t.string_value << std::endl;
                break;
            case TokenID::Punctuation:
                std::cout << "TokenID::Punctuation: " << t.string_value << std::endl;
                break;
            case TokenID::String:
                std::cout << "TokenID::String: " << t.string_value << std::endl;
                break;
            case TokenID::Identifier:
                std::cout << "TokenID::Identifier: " << t.string_value << std::endl;
                break;
            case TokenID::Integer:
                std::cout << "TokenID::Integer: " << t.integer_value << std::endl;
                break;
            case TokenID::Float:
                std::cout << "TokenID::Float: " << t.double_value << std::endl;
                break;
        }
    }
}

const Token& TokenStream::peek() {
    // Returns reference to token at the front of token_stream queue object without removing it
    return line_buf.front();
}

Token& TokenStream::get() {
    // Gets (removes) token from token_stream queue object and returns it
    Token& temp = line_buf.front();
    line_buf.pop_front();
    return temp;
}

std::string TokenStream::get_string(std::stringstream& in, const char delimiter) {
    // Grabs string from input stream; keeps reading more lines until it reaches the appropriate
    // delimiter
    std::string result {delimiter};
    char pilot;
    while ((in.peek() != delimiter) and (in.peek() != EOF)) {
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
    // Grabs a '#' delimited from token stream
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

const std::deque<Token>& TokenStream::tokenize_next_line(bool continuation) {
    // Tokenize a line and return list of tokens
    if (!continuation) line_buf.clear();
    // Update Curret Line number
    current_line += 1;

    // Grab a line from the input stream
    std::string line = "";
    std::getline(*input, line);
    line.push_back('\n'); // Manually add newline character    
    
    // Convert line into an input stream for ease of use
    std::stringstream line_stream(line);

    // Get-character from stream
    char pilot;
    int character_position = 0;

    // Binary Results
    std::string string_result = "";
    double integral_result;

    // Grab characters from stream and organize them into tokens
    while (line_stream.get(pilot)) {
        character_position += 1;
        switch (pilot) {
            // End of Python Line
            case '\n':
                line_buf.push_back({TokenID::End, string_result, character_position});
                break;
            // Comment:
            case '#':
                string_result = get_comment(line_stream, pilot);
                line_buf.push_back({TokenID::Comment, string_result, character_position});
                break;
            // Whitespace
            case '\t':
            case ' ':
                string_result = get_whitespace(line_stream, pilot);
                line_buf.push_back({TokenID::WhiteSpace, string_result, character_position});
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
            case '+':
            case '-':
                if (pilot == '-' and line_stream.peek() == '>') {
                    string_result += pilot;
                    line_stream.get(pilot);
                    string_result += pilot;
                    line_buf.push_back({TokenID::Punctuation, string_result, character_position});
                    break;
                }
            case '&':
            case '|':
            case '%':
            case '^':
            case '=':
            case ':':
                string_result += pilot;
                if (line_stream.peek() == '=') {
                    line_stream.get(pilot);
                    string_result += pilot;
                }
                else if (pilot == ':') {
                    line_buf.push_back({TokenID::Punctuation, string_result, character_position});
                }
                line_buf.push_back({TokenID::Operator, string_result, character_position});
                break;
            // Punctuation
            case '\'':
            case '"':
                string_result = get_string(line_stream, pilot);
                line_buf.push_back({TokenID::String, string_result, character_position});
                break;
            case '(':
            case ')':
            case '[':
            case ']':
            case '{':
            case '}':
            case ',':
            case ';':
            case '_':
                string_result += pilot;
                line_buf.push_back({TokenID::Punctuation, string_result, character_position});
                break;
            // Line Continuation
            case '\\':
                tokenize_next_line(true);
                break;
            // Special Case '.' (Operator or Floating Point Number)
            case '.':
                if (!isdigit(line_stream.peek())) {
                    string_result += pilot;
                    // Check for ellipses '...'
                    if (line_stream.peek() == '.') {
                        line_stream.get(pilot);
                        if (line_stream.peek() == '.') {
                            line_stream.get(pilot);
                            string_result += pilot; string_result += pilot;
                        }
                        else line_stream.putback(pilot);
                    }
                    line_buf.push_back({TokenID::Punctuation, string_result, character_position});
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
                    line_buf.push_back({TokenID::Identifier, string_result, character_position});
                    break;
                }
                // Numbers
                else if (pilot == '.' or isdigit(pilot)) {
                    line_stream.putback(pilot);
                    line_stream >> integral_result;
                    line_buf.push_back({TokenID::Float, integral_result, character_position});
                    break;
                }
                else {
                    throw SyntaxError();
                }
        }
        string_result = "";
    }
    return line_buf;
}
