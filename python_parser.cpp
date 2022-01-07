#include "headers/python_parser.hpp"

template <class T>
void ASTNode<T>::print(std::string tab) {
    std::cout << tab << value;
    tab += "   ";
    for (Node& n: block) {
        n.print(tab);
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

    const Token& current_token = ts.peek(); 
    std::unique_ptr<Node> atom;
    switch (current_token.id) {
        case TokenID::WhiteSpace:
            break;
        case TokenID::Number:
            if (current_token.double_value == std::floor(current_token.double_value))
                atom = std::make_unique<ASTNode<int>>(NodeKind::LITERAL, current_token.double_value);
            else
                atom = std::make_unique<ASTNode<double>>(NodeKind::LITERAL, current_token.double_value);
            break;
        case TokenID::String:
            atom = std::make_unique<ASTNode<std::string>>(NodeKind::LITERAL, current_token.string_value);
            break;
        case TokenID::Identifier:
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

    const Token& current_token = ts.peek();
    while (true) {
        switch (current_token.id) {
            case TokenID::Punctuation:
                if (current_token.string_value == ".") {
                    atom = std::make_unique<ASTNode<std::string>>(NodeKind::OPERATOR_CALL, current_token.string_value)
                        ->add_node(expect_atomic(true))
                            ->add_node(atom);
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

    const Token& current_token = ts.peek();
    while (true) {
        switch (current_token.id) {
            case TokenID::Operator:
                if (current_token.string_value == "**") {
                    prim = std::make_unique<ASTNode<std::string>>(NodeKind::OPERATOR_CALL, current_token.string_value)
                        ->add_node(expect_factor(true))
                            ->add_node(prim);
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
    while (true) {
        switch (current_token.id) {
            case TokenID::Operator:
                // FOR FUTURE FIX
                if (current_token.string_value == "+" or current_token.string_value == "-") {
                    return std::make_unique<ASTNode<std::string>>(NodeKind::OPERATOR_CALL, current_token.string_value)
                        ->add_node(expect_factor(true));
                }
            default:
                return expect_power(get);
        }
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

    const Token& current_token = ts.peek();
    while (true) {
        switch (current_token.id) {
            case TokenID::Operator:
                if (current_token.string_value == "*" or 
                        current_token.string_value == "/" or 
                            current_token.string_value == "//" or 
                                current_token.string_value == "%" or      
                                    current_token.string_value == "@") {
                    factor = std::make_unique<ASTNode<std::string>>(NodeKind::OPERATOR_CALL, current_token.string_value)
                        ->add_node(expect_factor(true))
                            ->add_node(factor);
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

    const Token& current_token = ts.peek();
    while (true) {
        switch (current_token.id) {
            case TokenID::Operator:
                if (current_token.string_value == "+" or 
                        current_token.string_value == "-") {
                    term = std::make_unique<ASTNode<std::string>>(NodeKind::OPERATOR_CALL, current_token.string_value)
                        ->add_node(expect_term(true))
                            ->add_node(term);
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

    const Token& current_token = ts.peek();
    while (true) {
        switch (current_token.id) {
            case TokenID::Punctuation:
                if (current_token.string_value == ";") {
                    expr = std::make_unique<Node>(NodeKind::OPERATOR_CALL)
                        ->add_node(expect_expression(true));
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

    return std::make_unique<Node>(nullptr);
}

std::unique_ptr<Node> Parser::expect_statement(bool get) {
    const Token& current_token = ts.peek();
    switch (current_token.id) {
        case TokenID::End:
            return std::unique_ptr<Node>(nullptr);
        case TokenID::Number:
        case TokenID::String:
            return expect_simplestatements(get);
        case TokenID::Identifier:
            break;
        case TokenID::Punctuation:
            break;
    }
}

std::unique_ptr<Node> Parser::parse(bool get) {
    return expect_statement(get);
}