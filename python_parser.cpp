#include "headers/python_parser.h"

void Node::print(std::string indent) {

}

std::unique_ptr<Node> Parser::parse_statement(std::stack<std::unique_ptr<Node>>& parse_stack) {
    const std::vector<Token>& current_line = ts.get();
    std::unique_ptr<Statement> s = std::make_unique<Statement>();
    for (int i = 0; current_line[i].id != TokenID::End; i++) {
        const Token& current_token = current_line[i];
        switch (current_line[i].id) {
            case TokenID::Number:
                if (current_token.double_value == std::floor(current_token.double_value))
                    parse_stack.push(std::make_unique<LiteralExpression<int>>(current_token.double_value));
                else  
                    parse_stack.push(std::make_unique<LiteralExpression<double>>(current_token.double_value));
                break;
            case TokenID::String:
                parse_stack.push(std::make_unique<LiteralExpression<std::string>>(current_token.string_value));
                break;
            case TokenID::Identifier:
                parse_stack.push(std::make_unique<ReferenceExpression>(current_token.string_value));
                break;
            case TokenID::Operator: 
                parse_stack.push(std::make_unique<OperatorExpression>(current_token.string_value));
                break;
            case TokenID::Punctuation:
                break;
        }
    }
}

std::unique_ptr<Node> Parser::parse() {
    bool start_of_line = true;
    std::string current_indent = "";
    
    std::unique_ptr<Node> parse_tree = std::make_unique<Node>();
    std::stack<std::unique_ptr<Node>> parse_stack;

    ts.get();
    while (!ts.peek().empty()) {
        parse_tree->add_node(parse_statement(parse_stack));
        ts.get();
    }

    return parse_tree;
}