// Python Interpreter and REPL

#include <iostream>
#include <exception>
#include <csignal>
#include <string>
#include "headers/python_exceptions.h"
#include "headers/python_tokens.h"

int main(int argc, char *argv[]) {
    std::signal(SIGINT, keyboard_interrupt_handler);
    TokenStream ts {std::cin};
    while (true) {
        std::cout << ">>> ";
        for (const Token& t : ts.tokenize_next_line(false)) {
            switch (t.id) {
                case TokenID::Operator:
                case TokenID::String:
                case TokenID::WhiteSpace:
                case TokenID::Punctuation:
                case TokenID::Identifier:
                case TokenID::Comment:
                    std::cout << t.string_value << std::endl;
                    break;
                case TokenID::Number:
                    std::cout << t.double_value << std::endl;
                    break;
            }
        }
    }
    return 0;
}