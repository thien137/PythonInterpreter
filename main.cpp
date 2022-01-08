// Python Interpreter and REPL

#include "src/python_parser.hpp"

void interpret(std::istream& input, std::string input_name) {
    TokenStream ts {std::cin, "<stdin>"};
    Parser p {ts};
    while (true) {
        std::cout << ">>> ";
        try {
            std::unique_ptr<Node> parse_tree = p.parse_next_line(false);
            if (parse_tree != nullptr)
                parse_tree->print("");
        }
        catch (SyntaxError& e) {
            std::cout << e.what() << std::endl;
        } 
    }
}

int main(int argc, char *argv[]) {
    switch (argc) {
        case 1: {
            interpret(std::cin, "<stdin>");
            break;
        }
    }
}