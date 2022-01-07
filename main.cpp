// Python Interpreter and REPL

#include <iostream>
#include <exception>
#include <limits>
#include "headers/python_parser.hpp"
#include "headers/python_tokens.hpp"
#include "headers/python_exceptions.hpp"

int main(int argc, char *argv[]) {
    TokenStream ts {std::cin};
    Parser p {ts};
    while (true) {
        try {
            std::unique_ptr<Node> parse_tree = p.parse(false);
            parse_tree->print();
        }
        catch (SyntaxError& e) {
            std::cout << e.what() << std::endl;
        } 
    }
}