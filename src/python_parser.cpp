#include "python_parser.hpp"

void Node::print(std::string tab) {
    if (tab == "") {
        std::cout << "--------AST--------" << std::endl;
    }
    std::cout << tab;
    switch (tok.id) {
        case TokenID::Integer:
            std::cout << tok.integer_value;
            break;
        case TokenID::Float:
            std::cout << tok.double_value;
            break;
        case TokenID::Identifier:
        case TokenID::Operator:
        case TokenID::String:
        case TokenID::Punctuation:
            std::cout << tok.string_value;
            break;
    }
    if (!block.empty()) {
        std::cout << ":" << std::endl;
        tab += "   ";
        for (std::unique_ptr<Node>& n: block) {
            n->print(tab);
        }
    }
    else {
        std::cout << std::endl;
    }
}

std::unique_ptr<Node> Parser::expect_atomic(bool get) {
    // atom:
    //     | NAME
    //     | 'True' 
    //     | 'False' 
    //     | 'None' 
    //     | strings
    //     | NUMBER
    //     | (tuple | group | genexp)
    //     | (list | listcomp)
    //     | (dict | set | dictcomp | setcomp)
    //     | '...'
    if (get) ts.get();

    // Temporary fix for white spaces
    while (ts.peek().id == TokenID::WhiteSpace) {
        ts.get();
    }

    const Token& current_token = ts.peek(); 
    std::unique_ptr<Node> atom;
    switch (current_token.id) {
        case TokenID::Integer:
        case TokenID::Float:
            atom = std::make_unique<Node>(NodeKind::LITERAL, current_token);
            break;
        case TokenID::String:
            atom = std::make_unique<Node>(NodeKind::LITERAL, current_token);
            break;
        case TokenID::Identifier:
            break;
        case TokenID::End:
            throw SyntaxError();
            break;
        default:
            throw SyntaxError();
    }
    ts.get();
    return atom;
}

std::unique_ptr<Node> Parser::expect_primary(bool get) {
    // primary:
    //     | primary '.' NAME 
    //     | primary genexp 
    //     | primary '(' [arguments] ')' 
    //     | primary '[' slices ']' 
    //     | atom
    std::unique_ptr<Node> atom = expect_atomic(get);

    while (true) {
        const Token& current_token = ts.peek();
        switch (current_token.id) {
            case TokenID::WhiteSpace:
                ts.get();
                break;
            case TokenID::Punctuation:
                if (current_token.string_value == ".") {
                    std::unique_ptr<Node> prim = std::make_unique<Node>(NodeKind::OPERATOR_CALL, current_token);
                    prim->add_node(expect_atomic(true));
                    prim->add_node(atom);
                    atom = move(prim);
                    break;
                }
            default:
                return atom;
        }
    }
}

std::unique_ptr<Node> Parser::expect_power(bool get) {
    // power:
    //     | await_primary '**' factor 
    //     | await_primary
    std::unique_ptr<Node> prim = expect_primary(get);

    while (true) {
        const Token& current_token = ts.peek();
        switch (current_token.id) {
            case TokenID::WhiteSpace:
                ts.get();
                break;
            case TokenID::Operator:
                if (current_token.string_value == "**") {
                    std::unique_ptr<Node> power = std::make_unique<Node>(NodeKind::OPERATOR_CALL, current_token);
                    power->add_node(expect_factor(true));
                    power->add_node(prim);
                    prim = move(power);
                    break;
                }
            default:
                return prim;
        }
    }
}

std::unique_ptr<Node> Parser::expect_factor(bool get) {
    // factor:
    //     | '+' factor 
    //     | '-' factor 
    //     | '~' factor 
    //     | power
    if (get) ts.get();

    const Token& current_token = ts.peek();
    switch (current_token.id) {
        case TokenID::Operator:
            if (current_token.string_value == "+" or
                    current_token.string_value == "-" or
                        current_token.string_value == "~") {
                std::unique_ptr<Node> factor = std::make_unique<Node> (NodeKind::OPERATOR_CALL, current_token);
                factor->add_node(expect_factor(true));
                return factor;
            }
            else {
                // Continue on to process the following tokens as a power
            } 
        default:
            return expect_power(false);
    }
}

std::unique_ptr<Node> Parser::expect_term(bool get) {
    // term:
    //     | term '*' factor 
    //     | term '/' factor 
    //     | term '//' factor 
    //     | term '%' factor 
    //     | term '@' factor 
    //     | factor
    std::unique_ptr<Node> factor = expect_factor(get);

    while (true) {
        const Token& current_token = ts.peek();
        switch (current_token.id) {
            case TokenID::WhiteSpace:
                ts.get();
                break;
            case TokenID::Operator:
                if (current_token.string_value == "*" or 
                        current_token.string_value == "/" or 
                            current_token.string_value == "//" or 
                                current_token.string_value == "%" or      
                                    current_token.string_value == "@") {
                    std::unique_ptr<Node> term = std::make_unique<Node>(NodeKind::OPERATOR_CALL, current_token);
                    term->add_node(expect_factor(true));
                    term->add_node(factor);
                    factor = move(term);
                    break;
                }
            default:
                return factor;
        }
    }
}

std::unique_ptr<Node> Parser::expect_sum(bool get) {
    // sum:
    //     | sum '+' term 
    //     | sum '-' term 
    //     | term
    std::unique_ptr<Node> term = expect_term(get);

    while (true) {
        const Token& current_token = ts.peek();
        switch (current_token.id) {
            case TokenID::WhiteSpace:
                ts.get();
                break;
            case TokenID::Operator:
                if (current_token.string_value == "+" or 
                        current_token.string_value == "-") {
                    std::unique_ptr<Node> sum = std::make_unique<Node>(NodeKind::OPERATOR_CALL, current_token);
                    sum->add_node(expect_term(true));
                    sum->add_node(term);
                    term = move(sum);
                    break;
                }
            default:
                return term;
        }
    }
}

std::unique_ptr<Node> Parser::expect_expression(bool get) {
    // To be finished
    return expect_sum(get);
}

std::unique_ptr<Node> Parser::expect_simplestatements(bool get) {
    // simple_stmts:
    //     | simple_stmt !';' NEWLINE  # Not needed, there for speedup
    //     | ';'.simple_stmt+ [';'] NEWLINE 
    std::unique_ptr<Node> expr = expect_expression(get);

    while (true) {
        const Token& current_token = ts.peek();
        switch (current_token.id) {
            case TokenID::WhiteSpace:
                ts.get();
                break;
            case TokenID::Punctuation:
                if (current_token.string_value == ";") {
                    std::unique_ptr<Node> simple_statements = std::make_unique<Node>(NodeKind::BLOCK, current_token);
                    simple_statements->add_node(expect_expression(true));
                    simple_statements->add_node(expr);
                    expr = move(simple_statements);
                    break;
                }
            default:
                return expr;
        }
    }
}

std::unique_ptr<Node> Parser::expect_compoundstatement(bool get) {
    // handles special compoundstatements (ie. "for", "while", "try")
    const Token& next_token = ts.peek();

    if (get) ts.get();

    return std::make_unique<Node>(NodeKind::BLOCK, ts.peek());
}

std::unique_ptr<Node> Parser::expect_statement(bool get) {
    const Token& current_token = ts.peek();
        switch (current_token.id) {
            case TokenID::Comment:
                ts.get();
                break;
            case TokenID::WhiteSpace:
                ts.get();
                break;
            case TokenID::Integer:
            case TokenID::Float:
            case TokenID::Identifier:
            case TokenID::String:
                return expect_simplestatements(get);
            case TokenID::End:
                return nullptr;
            default:
                std::cout << "Asdasdadd" << std::endl;
                throw SyntaxError();
        }
    return nullptr;
}

std::unique_ptr<Node> Parser::parse_next_line(bool get) {
    ts.tokenize_next_line(false);
    ts.print();
    return expect_statement(get);
}