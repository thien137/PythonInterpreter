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

std::unique_ptr<Node> Parser::parse_next_line(bool get) {
    ts.tokenize_next_line(false);
    ts.print();
    auto tree = expect_statement(get);
    if (!ts.empty()) throw SyntaxError();                                                                                                                                                                                                                                                                                                                                                                                                         
    return tree;
}

std::unique_ptr<Node> Parser::expect_statement(bool get, Flag flags) {
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
            case TokenID::Operator:
            case TokenID::Punctuation:
                return expect_simplestatements(get);
            case TokenID::End:
                return nullptr;
            default:
                throw SyntaxError();
        }
    return nullptr;
}

std::unique_ptr<Node> Parser::expect_simplestatements(bool get, Flag flags) {
    // simple_stmts:
    //     | simple_stmt !';' NEWLINE  # Not needed, there for speedup
    //     | ';'.simple_stmt+ [';'] NEWLINE 
    return expect_helper(get, TokenID::Punctuation, {";"}, NodeKind::TUPLE, &Parser::expect_simplestatement, &Parser::expect_simplestatement, flags);
}

std::unique_ptr<Node> Parser::expect_simplestatement(bool get, Flag flags) {
        // simple_stmt:
        //     | assignment
        //     | star_expressions 
        //     | return_stmt
        //     | import_stmt
        //     | raise_stmt
        //     | 'pass' 
        //     | del_stmt
        //     | yield_stmt
        //     | assert_stmt
        //     | 'break' 
        //     | 'continue' 
        //     | global_stmt
        //     | nonlocal_stmt
        return expect_star_expressions(get);
}

std::unique_ptr<Node> Parser::expect_compoundstatement(bool get, Flag flags) {
    // handles special compoundstatements (ie. "for", "while", "try")
    const Token& next_token = ts.peek();

    if (get) ts.get();

    return std::make_unique<Node>(NodeKind::BLOCK, ts.peek());
}

std::unique_ptr<Node> Parser::expect_assignment(bool get, Flag flags) {
    // assignment:
    //     | NAME ':' expression ['=' annotated_rhs ] 
    //     | ('(' single_target ')' 
    //          | single_subscript_attribute_target) ':' expression ['=' annotated_rhs ] 
    //     | (star_targets '=' )+ (yield_expr | star_expressions) !'=' [TYPE_COMMENT] 
    //     | single_target augassign ~ (yield_expr | star_expressions)
}

std::unique_ptr<Node> Parser::expect_augassign(bool get, Flag flags) {
    // augassign:
    //     | '+=' 
    //     | '-=' 
    //     | '*=' 
    //     | '@=' 
    //     | '/=' 
    //     | '%=' 
    //     | '&=' 
    //     | '|=' 
    //     | '^=' 
    //     | '<<=' 
    //     | '>>=' 
    //     | '**=' 
    //     | '//=' 
    return expect_token_get(get, TokenID::Operator, {"+=", "-=", "*=", "@=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "**=", "//="}, flags);
}

std::unique_ptr<Node> Parser::expect_global_stmt(bool get, Flag flags) {
    // global_stmt: 'global' ','.NAME+
    std::unique_ptr<Node> global_stmt = expect_token_get(get, TokenID::Identifier, "global", flags);
    global_stmt->add_node_front(expect_helper(false, TokenID::Identifier, {","}, NodeKind::OPERATOR_CALL, &Parser::expect_name, &Parser::expect_name, flags));
    return global_stmt;
}

std::unique_ptr<Node> Parser::expect_nonlocal_stmt(bool get, Flag flags) {
    // nonlocal_stmt: 'nonlocal' ','.NAME+ 
    std::unique_ptr<Node> nonlocal_stmt = expect_token_get(get, TokenID::Identifier, "nonlocal", flags);
    nonlocal_stmt->add_node_front(expect_helper(false, TokenID::Identifier, {","}, NodeKind::OPERATOR_CALL, &Parser::expect_name, &Parser::expect_name, flags));
    return nonlocal_stmt
}

std::unique_ptr<Node> Parser::expect_yield_stmt(bool get, Flag flags) {
    // yield_stmt: yield_expr 
    return expect_yield_expr(get, flags);
}

std::unique_ptr<Node> Parser::expect_assert_stmt(bool get, Flag flags) {
    // assert_stmt: 'assert' expression [',' expression ]
    std::unique_ptr<Node> assert_stmt = expect_token_get(get, TokenID::Identifier, "assert", flags); 
    std::unique_ptr<Node> expression = expect_expression(false, flags);
    if (expect_token(TokenID::Punctuation, ",")) {
        std::unique_ptr<Node> comma = expect_token_get(false, TokenID::Punctuation, ",", flags);
        comma->add_node_front(expect_expression(false, flags));
        comma->add_node_front(expression);
        expression = move(comma);
    }
    assert_stmt->add_node_front(expression);
    return assert_stmt;
}

std::unique_ptr<Node> Parser::expect_del_stmt(bool get, Flag flags) {
    // del_stmt:
    //     | 'del' del_targets &(';' | NEWLINE)
    std::unique_ptr<Node> del_stmt = expect_token_get(get, TokenID::Identifier, "del", flags);
    del_stmt->add_node_back(expect_del_targets(false, flags));
    if (!(expect_token(TokenID::Punctuation, ";") or expect_token(TokenID::End))) throw SyntaxError();
    return del_stmt;
}

std::unique_ptr<Node> Parser::import_stmt(bool get, Flag flags) {
    // import_stmt: import_name | import_from
    std::unique_ptr<Node> import_stmt = expect_token_get(get, TokenID::Identifier, "import", flags);
    if (expect_token(TokenID::Identifier, "from"))
        import_stmt->add_node_front(expect_import_from(false, flags));
    else
        import_stmt->add_node_front(expect_import_from(false, flags));
    return import_stmt;
}

std::unique_ptr<Node> Parser::expect_import_name(bool get, Flag flags) {
    // import_name: 'import' dotted_as_names
    // Assumes that 'import' has been read already 
    return expect_dotted_as_names(get, flags);
}

std::unique_ptr<Node> Parser::expect_import_from(bool get, Flag flags) {
    // # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
    // import_from:
    //     | 'from' ('.' | '...')* dotted_name 'import' import_from_targets 
    //     | 'from' ('.' | '...')+ 'import' import_from_targets 
    // Assumes that 'import' has been read, and 'from' is next
    std::unique_ptr<Node> import_from = expect_token_get(get, TokenID::Identifier, "from", flags);
    if (expect_token(TokenID::Punctuation, ".") or expect_token(TokenID::Punctuation, "...")) {
        while(expect_token(TokenID::Punctuation, ".") or expect_token(TokenID::Punctuation, "...")) {
            import_from->add_node_back(expect_token_get(false, TokenID::Punctuation, ".", Flag{false, true} | flags));
            import_from ->add_node_back(expect_token_get(false, TokenID::Punctuation, "...", Flag{false, true} | flags));
        }
    }
    else import_from->add_node_back(expect_dotted_name(get, flags));
    
    import_from->add_node_back(expect_token_get(get, TokenID::Identifier, "import", flags));
    import_from->add_node_back(expect_import_from_targets(false, flags));
    return import_from;
}

std::unique_ptr<Node> Parser::expect_import_from_targets(bool get, Flag flags) {
// import_from_targets:
//     | '(' import_from_as_names [','] ')' 
//     | import_from_as_names !','
//     | '*' 
    std::unique_ptr<Node> import_from_targets {};
    if (expect_token(TokenID::Punctuation, "(")) {
        import_from_targets = std::make_unique<Node>(NodeKind::TUPLE, Token{TokenID::Punctuation, "()"});
        import_from_targets->add_node_front(expect_helper(true, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_import_from_as_names, &Parser::expect_import_from_as_names, flags));
        // Check for ')' delimiter
        if (!expect_token(TokenID::Punctuation, ")")) throw SyntaxError("expected ')'");
    }
    else if (expect_token(TokenID::Operator, "*")) {
        import_from_targets = expect_token_get(get, TokenID::Operator, "*", flags);
    }
    else {
        import_from_targets = expect_import_from_as_names(get, flags);
        // ',' next is illegal
        if (expect_token(TokenID::Punctuation, ",")) throw SyntaxError("did not expect ','"); 
    }
    return import_from_targets;
}

std::unique_ptr<Node> Parser::expect_import_from_as_names(bool get, Flag flags) {
    // import_from_as_names:
    //     | ','.import_from_as_name+ 
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_import_from_as_name, &Parser::expect_import_from_as_name, Flag{false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_import_from_as_name(bool get, Flag flags) {
    // import_from_as_name:
    //     | NAME ['as' NAME ]
    return expect_helper(get, TokenID::Identifier, {"as"}, NodeKind::OPERATOR_CALL, &Parser::expect_name, &Parser::expect_name, Flag{false, false, false, false, false, false, true} | flags); 
}

std::unique_ptr<Node> Parser::expect_dotted_as_names(bool get, Flag flags) {
    // dotted_as_names:
    //     | ','.dotted_as_name+ 
    return expect_helper(get, TokenID::Identifier, {","}, NodeKind::COLLECTION, &Parser::expect_dotted_as_name, &Parser::expect_dotted_as_name, Flag{false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_dotted_as_name(bool get, Flag flags) {
    // dotted_as_name:
    //     | dotted_name ['as' NAME ]
    return expect_helper(get, TokenID::Identifier, {"as"}, NodeKind::OPERATOR_CALL, &Parser::expect_dotted_name, &Parser::expect_name, Flag{false, false, false, false, false, false, true} | flags);  
}

std::unique_ptr<Node> Parser::expect_dotted_name(bool get, Flag flags) {
    // dotted_name:
    //     | dotted_name '.' NAME 
    //     | NAME
    return expect_helper(get, TokenID::Identifier, {"."}, NodeKind::OPERATOR_CALL, &Parser::expect_name, &Parser::expect_name, flags);
}
std::unique_ptr<Node> Parser::expect_if_stmt(bool get, Flag flags) {
    // if_stmt:
    //     | 'if' named_expression ':' block elif_stmt 
    //     | 'if' named_expression ':' block [else_block] 
    std::unique_ptr<Node> if_stmt = expect_token_get(get, TokenID::Identifier, "if", flags);
    if_stmt->add_node_back(expect_named_expression(false, flags));
    expect_token_get(get, TokenID::Punctuation, ":", flags); // Formality
    
    std::unique_ptr<Node> elif_stmt = expect_elif_stmt(false, Flag{false, true} | flags);
    if (elif_stmt != nullptr) if_stmt->add_node_back(elif_stmt);
    else if_stmt->add_node_back(expect_else_block(false, Flag{false, true} | flags));

    return if_stmt;
}
std::unique_ptr<Node> Parser::expect_elif_stmt(bool get, Flag flags) {
    // elif_stmt:
    //     | 'elif' named_expression ':' block elif_stmt 
    //     | 'elif' named_expression ':' block [else_block] 
    std::unique_ptr<Node> elif_stmt = expect_token_get(get, TokenID::Identifier, "elif", flags);
    elif_stmt->add_node_back(expect_named_expression(false, flags));
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality

    std::unique_ptr<Node> elif_stmt2 = expect_elif_stmt(false, Flag{false, true} | flags);
    if (elif_stmt2 != nullptr) elif_stmt->add_node_back(elif_stmt2);
    else elif_stmt->add_node_back(expect_else_block(false, Flag{false, true} | flags));

    return elif_stmt;
}

std::unique_ptr<Node> Parser::expect_else_block(bool get, Flag flags) {
    // else_block:
    //     | 'else' ':' block
    std::unique_ptr<Node> else_block = expect_token_get(get, TokenID::Identifier, "else", flags);
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    else_block->add_node_back(expect_block(false, flags));

    return else_block;
}

std::unique_ptr<Node> Parser::expect_while_stmt(bool get, Flag flags) {
    // while_stmt:
    //     | 'while' named_expression ':' block [else_block] 
    std::unique_ptr<Node> while_stmt = expect_token_get(get, TokenID::Identifier, "if", flags);
    while_stmt->add_node_back(expect_named_expression(false, flags));
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    while_stmt->add_node_back(expect_block(false, flags));
    while_stmt->add_node_back(expect_else_block(false, Flag{false, true} | flags));

    return while_stmt;
}

std::unique_ptr<Node> Parser::expect_for_stmt(bool get, Flag flags) {
// for_stmt:
//     | 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block] 
//     | ASYNC 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
    std::unique_ptr<Node> for_stmt = expect_token_get(get, TokenID::Identifier, "for", flags);
    for_stmt->add_node_back(expect_star_targets(false, flags));
    expect_token_get(false, TokenID::Identifier, "in", flags); // Formality
    for_stmt->add_node_back(expect_star_expressions(false, flags));
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    for_stmt->add_node_back(expect_block(false, flags));
    for_stmt->add_node_back(expect_else_block(false, Flag{false, true} | flags));

    return for_stmt;
}

std::unique_ptr<Node> Parser::expect_with_stmt(bool get, Flag flags) {
    // with_stmt:
    //     | 'with' '(' ','.with_item+ ','? ')' ':' block 
    //     | 'with' ','.with_item+ ':' [TYPE_COMMENT] block 
    //     | ASYNC 'with' '(' ','.with_item+ ','? ')' ':' block 
    //     | ASYNC 'with' ','.with_item+ ':' [TYPE_COMMENT] block
    std::unique_ptr<Node> with_stmt = expect_token_get(get, TokenID::Identifier, "with", flags);
    if (expect_token(TokenID::Punctuation, "(")) {
        std::unique_ptr<Node> items = std::make_unique<Node>(NodeKind::TUPLE, Token{TokenID::Punctuation, "()"});
        items->add_node_front(expect_helper(true, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_with_item, &Parser::expect_with_item, flags));
        // Check for ')' delimiter
        if (!expect_token(TokenID::Punctuation, ")")) throw SyntaxError("expected ')'");
    }
    else {
        with_stmt->add_node_back(expect_helper(false, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_with_item, &Parser::expect_with_item, Flag{false, false, true} | flags));
    }
    with_stmt->add_node_back(expect_token_get(true, TokenID::Operator, ":", flags));
    with_stmt->add_node_back(expect_block(false, flags));

    return with_stmt;
}

std::unique_ptr<Node> Parser::expect_with_item(bool get, Flag flags) {
    // with_item:
    //     | expression 'as' star_target &(',' | ')' | ':') 
    //     | expression 
    std::unique_ptr<Node> with_item = expect_expression(get, flags);
    if (std::unique_ptr<Node> as = expect_token_get(false, TokenID::Identifier, "as", Flag{false, true} | flags)) {
        as->add_node_front(expect_star_target(false, flags));
        if (!expect_token(TokenID::Punctuation, {",", ")", ":"})) throw SyntaxError("expected ',' | ')' | ':'");
        with_item = move(as);
    }
    return with_item;
}

std::unique_ptr<Node> Parser::expect_try_stmt(bool get, Flag flags) {
    // try_stmt:
    //     | 'try' ':' block finally_block 
    //     | 'try' ':' block except_block+ [else_block] [finally_block] 
    std::unique_ptr<Node> try_stmt =  expect_token_get(get, TokenID::Identifier, "try", flags);
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    try_stmt->add_node_back(expect_block(false, flags));
    if (std::unique_ptr<Node> except_block = expect_except_block(false, Flag{false, true} | flags)) {
        while (except_block != nullptr) {
            try_stmt->add_node_back(except_block);
            except_block = expect_except_block(false, Flag{false, true} | flags);
        }
        try_stmt->add_node_back(expect_else_block(false, Flag{false, true} | flags));
        try_stmt->add_node_back(expect_finally_block(false, Flag{false, true} | flags));
    }
    else {
        try_stmt->add_node_back(expect_finally_block(false, flags));
    }
    
    return try_stmt;
}
std::unique_ptr<Node> Parser::expect_except_block(bool get, Flag flags) {
    // except_block:
    //     | 'except' expression ['as' NAME ] ':' block
    //     | 'except' ':' block
    std::unique_ptr<Node> except_block =  expect_token_get(false, TokenID::Identifier, "try", flags);
    except_block->add_node_back(expect_helper(false, TokenID::Identifier, {"as"}, NodeKind::OPERATOR_CALL, &Parser::expect_expression, &Parser::expect_name, Flag{false, true, false, false, false, false, true} | flags));
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    except_block->add_node_back(expect_block(false, flags));
    
    return except_block;
}

std::unique_ptr<Node> Parser::expect_finally_block(bool get, Flag flags) {
    // finally_block:
    //     | 'finally' ':' block
    std::unique_ptr<Node> finally_block = expect_token_get(get, TokenID::Identifier, "finally", flags);
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    finally_block->add_node_back(expect_block(false, flags));

    return finally_block;
}

std::unique_ptr<Node> Parser::expect_match_stmt(bool get, Flag flags) {
    // match_stmt:
    //     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT 
    std::unique_ptr<Node> match_stmt = expect_token_get(get, TokenID::Identifier, "match", flags);
    match_stmt->add_node_back(expect_subject_expr(false, flags));
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    expect_newline(false, flags); // Formality
    expect_indent(false, flags); // Formality

    std::unique_ptr<Node> case_block = expect_case_block(false, flags);
    while (case_block != nullptr) {
        match_stmt->add_node_back(case_block);
        case_block = expect_case_block(false, Flag{false, true} | flags);
    }
    expect_dedent(false, flags); // Formality

    return match_stmt;
}

std::unique_ptr<Node> Parser::expect_subject_expr(bool get, Flag flags) {
    // subject_expr:
    //     | star_named_expression ',' star_named_expressions? 
    //     | named_expression
    if (expect_token(TokenID::Operator, "*")) return expect_star_named_expressions(get, Flag{false, false, true} | flags);
    else return expect_named_expression(false, flags);
}

std::unique_ptr<Node> Parser::expect_case_block(bool get, Flag flags) {
    // case_block:
    //     | "case" patterns guard? ':' block 
    std::unique_ptr<Node> case_block = expect_token_get(get, TokenID::Identifier, "case", flags);
    case_block->add_node_back(expect_patterns(false, flags));
    case_block->add_node_back(expect_guard(false, flags));
    expect_token_get(false, TokenID::Punctuation, ":", flags); // Formality
    case_block->add_node_back(expect_block(false, flags));\

    return case_block;
}

std::unique_ptr<Node> Parser::expect_guard(bool get, Flag flags) {
    // guard: 'if' named_expression 
    std::unique_ptr<Node> guard = expect_token_get(get, TokenID::Identifier, "if", flags);
    guard->add_node_back(expect_named_expression(false, flags));

    return guard;
}

std::unique_ptr<Node> Parser::expect_patterns(bool get, Flag flags) {
    // patterns:
    //     | open_sequence_pattern 
    //     | pattern
    if (std::unique_ptr<Node> open_sequence_pattern = expect_open_sequence_pattern(get, Flag{false, true} | flags))
        return open_sequence_pattern;
    else 
        return expect_pattern(false, flags);
}

std::unique_ptr<Node> Parser::expect_pattern(bool get, Flag flags) {
    // pattern:
    //     | as_pattern
    //     | or_pattern
    // as_pattern:
    //     | or_pattern 'as' pattern_capture_target
    std::unique_ptr<Node> pattern = expect_or_pattern(get, flags); 
    if (std::unique_ptr<Node> as = expect_token_get(false, TokenID::Identifier, "as", flags)) {
        as->add_node_front(expect_pattern_capture_target(false, flags));
        as->add_node_front(pattern);
        pattern = move(as);
    }
    
    return pattern;
}

// Unnecessary
// std::unique_ptr<Node> Parser::expect_as_pattern(bool get, Flag flags);
//     // as_pattern:
//     //     | or_pattern 'as' pattern_capture_target 

std::unique_ptr<Node> Parser::expect_or_pattern(bool get, Flag flags) {
    // or_pattern:
    //     | '|'.closed_pattern+
    return expect_helper(get, TokenID::Operator, {"|"}, NodeKind::OPERATOR_CALL, &Parser::expect_or_pattern, &Parser::expect_pattern_capture_target, flags);
}

std::unique_ptr<Node> Parser::expect_closed_pattern(bool get, Flag flags) {
    // closed_pattern:
    //     | literal_pattern
    //     | capture_pattern
    //     | wildcard_pattern
    //     | value_pattern
    //     | group_pattern
    //     | sequence_pattern
    //     | mapping_pattern
    //     | class_pattern
    return expect_literal_pattern(get, Flag{false, true} | flags) 
            or expect_capture_pattern(false, Flag{false, true} | flags)
                or expect_wildcard_pattern(false, Flag{false, true} | flags)
                        or expect_value_pattern(false, Flag{false, true} | flags)
                            or expect_group_pattern(false, Flag{false, true} | flags)
                                or expect_sequence_pattern(false, Flag{false, true} | flags)
                                    or expect_mapping_pattern(false, Flag{false, true} | flags)
                                        or expect_class_pattern(false, flags);
}

std::unique_ptr<Node> Parser::expect_literal_pattern(bool get, Flag flags) {
    // # Literal patterns are used for equality and identity constraints
    // literal_pattern:
    //     | signed_number !('+' | '-') 
    //     | complex_number 
    //     | strings 
    //     | 'None' 
    //     | 'True' 
    //     | 'False' 
    // literal_expr:
    //     | signed_number !('+' | '-')
    //     | complex_number
    //     | strings
    //     | 'None' 
    //     | 'True' 
    //     | 'False' 
    // Literally the same
    if (std::unique_ptr<Node> signed_number = expect_signed_number(get, Flag{false, true} | flags)) {
        if (expect_token(TokenID::Operator, {"+", "-"})) throw SyntaxError("did not expect '+' | '-'");
        else return signed_number;
    }
    else {
        return expect_signed_number(false, Flag{false, true} | flags)
                    or expect_complex_number(false, Flag{false, true} | flags)
                        or expect_strings(false, Flag{false, true} | flags)
                            or expect_token_get(false, TokenID::Identifier, "None", Flag{false, true} | flags)
                                or expect_capture_pattern(false, TokenID::Identifier, "True", Flag{false, true} | flags)
                                    or expect_capture_pattern(false, TokenID::Identifier, "False", Flag{false, true} | flags);
    }
}

std::unique_ptr<Node> Parser::expect_complex_number(bool get, Flag flags) {
    // complex_number:
    //     | signed_real_number '+' imaginary_number 
    //     | signed_real_number '-' imaginary_number
    return expect_helper(get, TokenID::Operator, {"-, +"}, NodeKind::OPERATOR_CALL, &Parser::expect_signed_real_number, &Parser::expect_imaginary_number, Flag{false, false, false, false, false, true, true} | flags); 
}

std::unique_ptr<Node> Parser::expect_signed_number(bool get, Flag flags) {
    // signed_number:
    //     | NUMBER
    //     | '-' NUMBER
    return expect_number(get, Flag{false, true} | flags) or expect_helper(get, TokenID::Operator, {"-"}, NodeKind::OPERATOR_CALL, &Parser::expect_number, &Parser::expect_number, Flag{false, false, false, false, false, true, true} | flags); 
}

std::unique_ptr<Node> Parser::expect_signed_real_number(bool get, Flag flags) {
    // signed_real_number:
    //     | real_number
    //     | '-' real_number 
    return expect_real_number(get, Flag{false, true} | flags) or expect_helper(get, TokenID::Operator, {"-"}, NodeKind::OPERATOR_CALL, &expect_real_number, &expect_real_number, Flag{false, false, false, false, false, true, true} | flags); 
}

std::unique_ptr<Node> Parser::expect_real_number(bool get, Flag flags) {
    // real_number:
    //     | NUMBER
    return expect_number(get, flags);
} 

std::unique_ptr<Node> Parser::expect_imaginary_number(bool get, Flag flags) {
    // imaginary_number:
    //     | NUMBER 
    return expect_number(get, flags);
}

std::unique_ptr<Node> Parser::expect_capture_pattern(bool get, Flag flags) {
    // capture_pattern:
    //     | pattern_capture_target
    return expect_pattern_capture_target(get, flags); 
}

std::unique_ptr<Node> Parser::expect_pattern_capture_target(bool get, Flag flags) {
    // pattern_capture_target:
    //     | !"_" NAME !('.' | '(' | '=') 
}

std::unique_ptr<Node> Parser::expect_wildcard_pattern(bool get, Flag flags) {
    // wildcard_pattern:
    //     | "_" 
    return expect_token_get(get, TokenID::Punctuation, "_", flags);
}

std::unique_ptr<Node> Parser::expect_value_pattern(bool get, Flag flags) {
    // value_pattern:
    //     | attr !('.' | '(' | '=') 
    std::unique_ptr<Node> value_pattern = expect_attr(get, flags);
    if (expect_token(TokenID::Punctuation, ".") or expect_token(TokenID::Punctuation, "(") or expect_token(TokenID::Operator, "=")) throw SyntaxError("did not expect '.' | '(' | '='");

    return value_pattern;
}

std::unique_ptr<Node> Parser::expect_attr(bool get, Flag flags) {
    // attr:
    //     | name_or_attr '.' NAME 
    return expect_helper(get, TokenID::Punctuation, {"."}, NodeKind::ATTRIBUTE_CALL, &Parser::expect_name_or_attr, &Parser::expect_name, flags);
}

std::unique_ptr<Node> Parser::expect_name_or_attr(bool get, Flag flags) {
    // name_or_attr:
    //     | attr
    //     | NAME
    if (std::unique_ptr<Node> attr = expect_attr(get, Flag{false, true} | flags)) return attr;
    else return expect_name(false, flags);
}

std::unique_ptr<Node> Parser::expect_group_pattern(bool get, Flag flags) {
    // group_pattern:
    //     | '(' pattern ')' 
    std::unique_ptr<Node> group_pattern = std::make_unique<Node>(NodeKind::COLLECTION, Token{TokenID::Punctuation, "()"});
    expect_token_get(get, TokenID::Punctuation, "(", flags); // Formality
    group_pattern->add_node_front(expect_pattern(false, flags));
    // Check for ')' delimiter
    if (!expect_token(TokenID::Punctuation, ")")) throw SyntaxError("expected ')'");

    return group_pattern;
}

std::unique_ptr<Node> Parser::expect_sequence_pattern(bool get, Flag flags) {
    // sequence_pattern:
    //     | '[' maybe_sequence_pattern? ']' 
    //     | '(' open_sequence_pattern? ')'
    std::unique_ptr<Node> sequence_pattern {};
    if (expect_token_get(get, TokenID::Punctuation, {"["}, Flag{false, true} | flags)) {
        sequence_pattern = std::make_unique<Node>(NodeKind::COLLECTION, Token{TokenID::Punctuation, "[]"});
        sequence_pattern->add_node_front(expect_maybe_sequence_pattern(false, Flag{false, true} | flags));
        // Check for ']' delimiter
        if (!expect_token(TokenID::Punctuation, "]")) throw SyntaxError("expected ')'");
    }
    else if (expect_token_get(false, TokenID::Punctuation, {"("}, flags)) {
        sequence_pattern = std::make_unique<Node>(NodeKind::COLLECTION, Token{TokenID::Punctuation, "()"});
        sequence_pattern->add_node_front(expect_open_sequence_pattern(false, Flag{false, true} | flags));
        // Check for ')' delimiter
        if (!expect_token(TokenID::Punctuation, ")")) throw SyntaxError("expected ')'");        
    }
}

std::unique_ptr<Node> Parser::expect_open_sequence_pattern(bool get, Flag flags) { 
    // open_sequence_pattern:
    //     | maybe_star_pattern ',' maybe_sequence_pattern? 
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_maybe_star_pattern, &Parser::expect_maybe_sequence_pattern, Flag{false, false, true, false, false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_maybe_sequence_pattern(bool get, Flag flags) {
    // maybe_sequence_pattern:
    //     | ','.maybe_star_pattern+ ','? 
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_maybe_star_pattern, &Parser::expect_maybe_star_pattern, Flag{false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_maybe_star_pattern(bool get, Flag flags) {
    // maybe_star_pattern:
    //     | star_pattern
    //     | pattern
    return expect_star_pattern(get, Flag{false, true} | flags) or expect_patter(get, flags);
}

std::unique_ptr<Node> Parser::expect_star_pattern(bool get, Flag flags) {
    // star_pattern:
    //     | '*' pattern_capture_target 
    //     | '*' wildcard_pattern
    return expect_helper(get, TokenID::Operator, {"*"}, NodeKind::OPERATOR_CALL, nullptr, &Parser::expect_pattern_capture_target, Flag{false, false, false, false, false, true, true} | flags) 
        or expect_helper(false, TokenID::Operator, {"*"}, NodeKind::OPERATOR_CALL, nullptr, &Parser::expect_wildcard_pattern, Flag{false, false, false, false, false, true, true} | flags); 
}

std::unique_ptr<Node> Parser::expect_mapping_pattern(bool get, Flag flags) {
    // mapping_pattern:
    //     | '{' '}' 
    //     | '{' double_star_pattern ','? '}' 
    //     | '{' items_pattern ',' double_star_pattern ','? '}' 
    //     | '{' items_pattern ','? '}' 
}

std::unique_ptr<Node> Parser::expect_items_pattern(bool get, Flag flags) {
    // items_pattern:
    //     | ','.key_value_pattern+
    return expect_helper(get, TokenID::Operator, {","}, NodeKind::COLLECTION, &Parser::expect_key_value_pattern, &Parser::expect_key_value_pattern, flags);
}

std::unique_ptr<Node> Parser::expect_key_value_pattern(bool get, Flag flags) {
    // key_value_pattern:
    //     | (literal_expr | attr) ':' pattern 
    return 
}

std::unique_ptr<Node> Parser::expect_double_star_pattern(bool get, Flag flags) {
    // double_star_pattern:
    //     | '**' pattern_capture_target 
    return expect_helper(get, TokenID::Operator, {"**"}, NodeKind::OPERATOR_CALL, nullptr, &Parser::expect_pattern_capture_target, Flag{false, false, false, false, false, true, true} | flags);
}

std::unique_ptr<Node> Parser::expect_class_pattern(bool get, Flag flags) {
    // class_pattern:
    //     | name_or_attr '(' ')' 
    //     | name_or_attr '(' positional_patterns ','? ')' 
    //     | name_or_attr '(' keyword_patterns ','? ')' 
    //     | name_or_attr '(' positional_patterns ',' keyword_patterns ','? ')' 
}

std::unique_ptr<Node> Parser::expect_positional_pattern(bool get, Flag flags) {
    // positional_patterns:
    //     | ','.pattern+ 
    return expect_helper(get, TokenID::Operator, {","}, NodeKind::COLLECTION, &Parser::expect_pattern, &Parser::expect_pattern, flags);
}

std::unique_ptr<Node> Parser::expect_keyword_patterns(bool get, Flag flags) {
    // keyword_patterns:
    //     | ','.keyword_pattern+
    return expect_helper(get, TokenID::Operator, {","}, NodeKind::COLLECTION, &Parser::expect_keyword_pattern, &Parser::expect_keyword_pattern, flags);
}

std::unique_ptr<Node> Parser::expect_keyword_pattern(bool get, Flag flags) {
    // keyword_pattern:
    //     | NAME '=' pattern 
}

std::unique_ptr<Node> Parser::return_stmt(bool get, Flag flags) {
    // return_stmt:
    //     | 'return' [star_expressions]
    std::unique_ptr<Node> return_stmt = expect_token_get(get, TokenID::Identifier, "return", flags);
    return_stmt->add_node_back(expect_star_expressionf(false, Flag{false, true} | flags)); 
    return return_stmt;
}

std::unique_ptr<Node> Parser::expect_raise_stmt(bool get, Flag flags) {
    // raise_stmt:
    //     | 'raise' expression ['from' expression ] 
    //     | 'raise' 
    std::unique_ptr<Node> raise_stmt = expect_token_get(get, TokenID::Identifier, "raise", flags);
    if (std::unique_ptr<Node> expression = expect_expression(false, Flag{false, true} | flags)) {
        if (expect_token(TokenID::Identifier, "from")) {
            std::unique_ptr<Node> from = expect_token_get(false, expressions)
        }
    }
}

std::unique_ptr<Node> Parser::expect_function_def(bool get, Flag flags) {
    // function_def:
    //     | decorators function_def_raw 
    //     | function_def_raw
}

std::unique_ptr<Node> Parser::expect_function_def_raw(bool get, Flag flags) {
    // function_def_raw:
    //     | 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block 
    //     | ASYNC 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block 
}

std::unique_ptr<Node> Parser::expect_func_type_comment(bool get, Flag flags) {
    // func_type_comment:
    //     | NEWLINE TYPE_COMMENT &(NEWLINE INDENT)   # Must be followed by indented block
    //     | TYPE_COMMENT
}

std::unique_ptr<Node> Parser::expect_params(bool get, Flag flags) {
    // params:
    //     | parameters
}

std::unique_ptr<Node> Parser::expect_parameters(bool get, Flag flags) {
    // parameters:
    //     | slash_no_default param_no_default* param_with_default* [star_etc] 
    //     | slash_with_default param_with_default* [star_etc] 
    //     | param_no_default+ param_with_default* [star_etc] 
    //     | param_with_default+ [star_etc] 
    //     | star_etc 
}

// # Some duplication here because we can't write (',' | &')'),
// # which is because we don't support empty alternatives (yet).
// #
std::unique_ptr<Node> Parser::expect_slash_no_default(bool get, Flag flags) {
    // slash_no_default:
    //     | param_no_default+ '/' ',' 
    //     | param_no_default+ '/' &')
}

std::unique_ptr<Node> Parser::expect_slash_with_default(bool get, Flag flags) {
    // slash_with_default:
    //     | param_no_default* param_with_default+ '/' ',' 
    //     | param_no_default* param_with_default+ '/' &')' 
}

std::unique_ptr<Node> Parser::expect_star_etc(bool get, Flag flags) {
    // star_etc:
    //     | '*' param_no_default param_maybe_default* [kwds] 
    //     | '*' ',' param_maybe_default+ [kwds] 
    //     | kwds 
}

std::unique_ptr<Node> Parser::expect_kwds(bool get, Flag flags) {
    // kwds: '**' param_no_default 
}

// # One parameter.  This *includes* a following comma and type comment.
// #
// # There are three styles:
// # - No default
// # - With default
// # - Maybe with default
// #
// # There are two alternative forms of each, to deal with type comments:
// # - Ends in a comma followed by an optional type comment
// # - No comma, optional type comment, must be followed by close paren
// # The latter form is for a final parameter without trailing comma.
// #

std::unique_ptr<Node> Parser::expect_param_no_default(bool get, Flag flags) {
    // param_no_default:
    //     | param ',' TYPE_COMMENT? 
    //     | param TYPE_COMMENT? &')' 
}

std::unique_ptr<Node> Parser::expect_param_with_default(bool get, Flag flags) {
    // param_with_default:
    //     | param default ',' TYPE_COMMENT? 
    //     | param default TYPE_COMMENT? &')' 
}

std::unique_ptr<Node> Parser::expect_param_maybe_default(bool get, Flag flags) {
    // param_maybe_default:
    //     | param default? ',' TYPE_COMMENT? 
    //     | param default? TYPE_COMMENT? &')' 
}

std::unique_ptr<Node> Parser::expect_param(bool get, Flag flags) {
    // param: NAME annotation? 
}

std::unique_ptr<Node> Parser::expect_annotation(bool get, Flag flags) {
    // annotation: ':' expression 
    // default: '=' expression 
}

std::unique_ptr<Node> Parser::expect_decorators(bool get, Flag flags) {
    // decorators: ('@' named_expression NEWLINE )+ 
}

std::unique_ptr<Node> Parser::expect_class_def(bool get, Flag flags) {
    // class_def:
    //     | decorators class_def_raw 
    //     | class_def_raw
}

std::unique_ptr<Node> Parser::expect_class_def(bool get, Flag flags) {
    // class_def_raw:
    //     | 'class' NAME ['(' [arguments] ')' ] ':' block 
}

std::unique_ptr<Node> Parser::expect_block(bool get, Flag flags) {
    // block:
    //     | NEWLINE INDENT statements DEDENT 
    //     | simple_stmts
}

std::unique_ptr<Node> Parser::expect_star_expressions(bool get, Flag flags) {
    // star_expressions:
    //     | star_expression (',' star_expression )+ [','] 
    //     | star_expression ',' 
    //     | star_expression
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_star_expression, &Parser::expect_star_expression, Flag{false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_star_expression(bool get, Flag flags) {
    // star_expression:
    //     | '*' bitwise_or 
    //     | expression
    return expect_helper(get, TokenID::Operator, {"*"}, NodeKind::OPERATOR_CALL, &Parser::expect_expression, &Parser::expect_bitwise_or, Flag{true} | flags);
}

std::unique_ptr<Node> Parser::expect_star_named_expressions(bool get, Flag flags) {
    // star_named_expressions: ','.star_named_expression+ [','] 
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_star_named_expression, &Parser::expect_star_named_expression, Flag{false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_star_named_expression(bool get, Flag flags) {
    // star_named_expression:
    //     | '*' bitwise_or 
    //     | named_expression
    return expect_helper(get, TokenID::Operator, {"*"}, NodeKind::OPERATOR_CALL, &Parser::expect_named_expression, &Parser::expect_bitwise_or, Flag{true} | flags);
}

std::unique_ptr<Node> Parser::expect_named_expression(bool get, Flag flags) {
    // named_expression:
    //     | assignment_expression
    //     | expression !':='
    // assignment_expression:
    //     | NAME ':=' ~ expression 
    return expect_helper(get, TokenID::Operator, {":="}, NodeKind::OPERATOR_CALL, &Parser::expect_expression, &Parser::expect_expression, flags);

    // TO BE FINISHED
}

std::unique_ptr<Node> Parser::expect_expressions(bool get, Flag flags) {
    // expressions:
    //     | expression (',' expression )+ [','] 
    //     | expression ',' 
    //     | expression
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_expression, &Parser::expect_expression, flags);
}

std::unique_ptr<Node> Parser::expect_expression(bool get, Flag flags) {
    // expression:
    //     | disjunction 'if' disjunction 'else' expression 
    //     | disjunction
    //     | lambdef
    // TO BE FINISHED
    return expect_disjunction(get, flags);
}

std::unique_ptr<Node> Parser::expect_disjunction(bool get, Flag flags) {
    // disjunction:
    //     | conjunction ('or' conjunction )+ 
    //     | conjunction
    return expect_helper(get, TokenID::Identifier, {"or"}, NodeKind::OPERATOR_CALL, &Parser::expect_conjunction, &Parser::expect_conjunction, flags);
}

std::unique_ptr<Node> Parser::expect_conjunction(bool get, Flag flags) {
    // conjunction:
    //     | inversion ('and' inversion )+ 
    //     | inversion
    return expect_helper(get, TokenID::Identifier, {"and"}, NodeKind::OPERATOR_CALL, &Parser::expect_inversion, &Parser::expect_inversion, flags);
}

std::unique_ptr<Node> Parser::expect_inversion(bool get, Flag flags) {
    // inversion:
    //     | 'not' inversion 
    //     | comparison
    return expect_helper(get, TokenID::Identifier, {"not"}, NodeKind::OPERATOR_CALL, &Parser::expect_comparison, &Parser::expect_inversion, Flag{true} | flags);
}

std::unique_ptr<Node> Parser::expect_comparison(bool get, Flag flags) {
    // comparison:
    //     | bitwise_or compare_op_bitwise_or_pair+ 
    //     | bitwise_or
    // compare_op_bitwise_or_pair:
    //     | eq_bitwise_or
    //     | noteq_bitwise_or
    //     | lte_bitwise_or
    //     | lt_bitwise_or
    //     | gte_bitwise_or
    //     | gt_bitwise_or
    //     | notin_bitwise_or
    //     | in_bitwise_or
    //     | isnot_bitwise_or
    //     | is_bitwise_or
    // eq_bitwise_or: '==' bitwise_or
    // noteq_bitwise_or:
    //     | ('!=' ) bitwise_or
    // lte_bitwise_or: '<=' bitwise_or
    // lt_bitwise_or: '<' bitwise_or
    // gte_bitwise_or: '>=' bitwise_or
    // gt_bitwise_or: '>' bitwise_or
    // is_bitwise_or: 'is' bitwise_or
    // not_bitwise_or: 'not' bitwise_or
    // in_bitwise_or: 'in' bitwise_or 
    return expect_helper(get, TokenID::Operator, {"<=", "<", ">", ">=", "==", "!="}, NodeKind::OPERATOR_CALL, &Parser::expect_bitwise_or, &Parser::expect_bitwise_or, flags);
}

std::unique_ptr<Node> Parser::expect_is_bitwise_or(bool get, Flag flags) {
    // is_bitwise_or: 'is' bitwise_or

    if (get) ts.get();

    while (true) {
        if (expect_token(TokenID::WhiteSpace)) {
                ts.get();
        }
        else if (expect_token(TokenID::Identifier, "is")) {
            std::unique_ptr<Node> is_bitwise_or = std::make_unique<Node>(NodeKind::OPERATOR_CALL, ts.peek());
            is_bitwise_or->add_node_front(expect_not_bitwise_or(true, flags));
            return is_bitwise_or;
        }
        else {
            return expect_not_bitwise_or(false, flags);
        }
    } 
}

std::unique_ptr<Node> Parser::expect_not_bitwise_or(bool get, Flag flags) {
    // not_bitwise_or: 'not' bitwise_or
    if (get) ts.get();

    while (true) {
        if (expect_token(TokenID::WhiteSpace)) {
                ts.get();
        }
        else if (expect_token(TokenID::Identifier, "not")) {
                std::unique_ptr<Node> is_not_bitwise_or = std::make_unique<Node>(NodeKind::OPERATOR_CALL, ts.peek());
                is_not_bitwise_or->add_node_front(expect_in_bitwise_or(true, flags));
                return is_not_bitwise_or;
        }
        else {
            return expect_in_bitwise_or(false, flags);
        }
    } 
}

std::unique_ptr<Node> Parser::expect_in_bitwise_or(bool get, Flag flags) {
    // in_bitwise_or: 'in' bitwise_or
    if (get) ts.get();

    while (true) {
        if (expect_token(TokenID::WhiteSpace)) {
                ts.get();
        }
        else if (expect_token(TokenID::Identifier, "in")) {
            std::unique_ptr<Node> is_not_in_bitwise_or = std::make_unique<Node>(NodeKind::OPERATOR_CALL, ts.peek());
            is_not_in_bitwise_or->add_node_front(expect_bitwise_or(true, flags));
            return is_not_in_bitwise_or;
        }
        else {
            return expect_bitwise_or(false, flags);
        }
    } 
} 

std::unique_ptr<Node> Parser::expect_bitwise_or(bool get, Flag flags) {
    // bitwise_or:
    //     | bitwise_or '|' bitwise_xor 
    //     | bitwise_xor
    return expect_helper(get, TokenID::Operator, {"|"}, NodeKind::OPERATOR_CALL, &Parser::expect_bitwise_xor, &Parser::expect_bitwise_xor, flags);
}

std::unique_ptr<Node> Parser::expect_bitwise_xor(bool get, Flag flags) {
    // bitwise_xor:
    //     | bitwise_xor '^' bitwise_and 
    //     | bitwise_and
    return expect_helper(get, TokenID::Operator, {"^"}, NodeKind::OPERATOR_CALL, &Parser::expect_bitwise_and, &Parser::expect_bitwise_and, flags);
}

std::unique_ptr<Node> Parser::expect_bitwise_and(bool get, Flag flags) {
    // bitwise_and:
    //     | bitwise_and '&' shift_expr 
    //     | shift_expr
    return expect_helper(get, TokenID::Operator, {"&"}, NodeKind::OPERATOR_CALL, &Parser::expect_shift_expr, &Parser::expect_shift_expr, flags);
}

std::unique_ptr<Node> Parser::expect_shift_expr(bool get, Flag flags) {
    // shift_expr:
    //     | shift_expr '<<' sum 
    //     | shift_expr '>>' sum 
    //     | sum
    return expect_helper(get, TokenID::Operator, {"<<", ">>"}, NodeKind::OPERATOR_CALL, &Parser::expect_sum, &Parser::expect_sum, flags);
}

std::unique_ptr<Node> Parser::expect_sum(bool get, Flag flags) {
    // sum:
    //     | sum '+' term 
    //     | sum '-' term 
    //     | term
    return expect_helper(get, TokenID::Operator, {"+", "-"}, NodeKind::OPERATOR_CALL, &Parser::expect_term, &Parser::expect_term, flags);
}

std::unique_ptr<Node> Parser::expect_term(bool get, Flag flags) {
    // term:
    //     | term '*' factor 
    //     | term '/' factor 
    //     | term '//' factor 
    //     | term '%' factor 
    //     | term '@' factor 
    //     | factor
    return expect_helper(get, TokenID::Operator, {"*", "/", "//", "%", "@"}, NodeKind::OPERATOR_CALL, &Parser::expect_factor, &Parser::expect_factor, flags);
}

std::unique_ptr<Node> Parser::expect_factor(bool get, Flag flags) {
    // factor:
    //     | '+' factor 
    //     | '-' factor 
    //     | '~' factor 
    //     | power
    return expect_helper(get, TokenID::Operator, {"+", "-", "~"}, NodeKind::OPERATOR_CALL, &Parser::expect_power, &Parser::expect_factor, Flag{true} | flags);
}

std::unique_ptr<Node> Parser::expect_power(bool get, Flag flags) {
    // power:
    //     | await_primary '**' factor 
    //     | await_primary
    return expect_helper(get, TokenID::Operator, {"**"}, NodeKind::OPERATOR_CALL, &Parser::expect_primary, &Parser::expect_factor, flags);
}

std::unique_ptr<Node> Parser::expect_primary(bool get, Flag flags) {
    // primary:
    //     | primary '.' NAME 
    //     | primary genexp 
    //     | primary '(' [arguments] ')' 
    //     | primary '[' slices ']' 
    //     | atom
    return expect_helper(get, TokenID::Operator, {"."}, NodeKind::OPERATOR_CALL, &Parser::expect_atomic, &Parser::expect_atomic, flags);
}

std::unique_ptr<Node> Parser::expect_atomic(bool get, Flag flags) {
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
    std::unique_ptr<Node> atom = nullptr;
    if (expect_token(current_token, TokenID::Integer) \
            or expect_token(current_token, TokenID::Float)
                or expect_token(current_token, TokenID::String))
        atom = std::make_unique<Node>(NodeKind::LITERAL, current_token);
    else if (expect_token(current_token, TokenID::Identifier))
        atom = std::make_unique<Node>(NodeKind::NAME, current_token);
    else if (expect_token(current_token, TokenID::Punctuation, {"("}))
        atom = handle_parentheses(true, flags);
    else if (expect_token(current_token, TokenID::Punctuation, {"{"}))
        atom = handle_angled_brackets(true, flags);
    else if (expect_token(current_token, TokenID::Punctuation, {"["}))
        atom = handle_square_brackets(true, flags);
    else
        return atom;
    ts.get();
    return atom;
}

std::unique_ptr<Node> Parser::handle_square_brackets(bool get, Flag flags) {
    // Handle lists (delimited by "[]")
    // Assumes that '[' has been read
    if (get) ts.get();
    std::unique_ptr<Node> sqr_expr {};
    std::unique_ptr<Node> list = std::make_unique<Node>(NodeKind::LIST, Token{TokenID::Punctuation, "[]"});
    
    if (expect_token(TokenID::Punctuation, "]")) {
        // empty tuple
        sqr_expr = move(list);
    }
    else if (expect_token(TokenID::Operator, {"*"})) {
        list->add_node_front(expect_star_named_expressions(false));
        sqr_expr = move(list);
    }
    else {
        sqr_expr = expect_named_expression(false);
        
        // list:
        // | '[' [star_named_expressions] ']' 
        if (expect_token(TokenID::Punctuation, {","})) {
            std::unique_ptr<Node> star_named_expressions = expect_star_named_expressions(false, Flag{false, false, false, false, false, true} | flags);
            star_named_expressions->add_node_front(sqr_expr);
            list->add_node_front(star_named_expressions);
            sqr_expr = move(list);
        }
        else if (expect_token(TokenID::Identifier, {"for"})) {
            // listcomp:
            //     | '[' named_expression for_if_clauses ']' 
            list->add_node_front(expect_for_if_clauses(false, Flag{false, true} | flags));
            list->add_node_front(sqr_expr);
            sqr_expr = move(list);
        }
        else {
            list->add_node_front(sqr_expr);
            sqr_expr = move(list);
        } 
    }
    // Check for ']' delimiter
    if (expect_token(TokenID::Punctuation, "]")) return sqr_expr;
    // Must have delimiter, or else throw syntax error
    else throw SyntaxError();
}

std::unique_ptr<Node> Parser::handle_parentheses(bool get, Flag flags) {
    // Handle tuple, group, and genexp (delimited by "()")
    // Assumes that '(' has been read
    if (get) ts.get();
    std::unique_ptr<Node> par_expr {};
    std::unique_ptr<Node> tuple = std::make_unique<Node>(NodeKind::TUPLE, Token{TokenID::Punctuation, "()"});
    
    if (expect_token(TokenID::Punctuation, ")")) {
        // empty tuple
        par_expr = move(tuple);
    }
    else if (expect_token(TokenID::Identifier, {"yield"})) {
        // group:
        //     | '(' (yield_expr | named_expression) ')' 
        par_expr = expect_yield_expr(false);
    }
    else {
        par_expr = expect_star_named_expression(false);
        
        // tuple:
        //     | '(' [star_named_expression ',' [star_named_expressions]  ] ')'
        if (expect_token(TokenID::Punctuation, {","})) {
            std::unique_ptr<Node> star_named_expressions = expect_star_named_expressions(false, Flag{false, false, false, false, false, true} | flags);
            star_named_expressions->add_node_front(par_expr);
            tuple->add_node_front(star_named_expressions);
            par_expr = move(tuple);
        }
        else if (expect_token(TokenID::Identifier, {"for"})) {
            // genexp:
            // '(' ( assignment_expression | expression !':=') for_if_clauses ')' 
            tuple->add_node_front(expect_for_if_clauses(false, Flag{false, true} | flags));
            tuple->add_node_front(par_expr);
            par_expr = move(tuple);
        } 
    }
    // Check for ')' delimiter
    if (expect_token(TokenID::Punctuation, ")")) return par_expr;
    // Must have delimiter, or else throw syntax error
    else throw SyntaxError();
}

std::unique_ptr<Node> Parser::handle_angled_brackets(bool get, Flag flags) {
    // Handle sets and dicts (delimited by "{}")
    // Assumes that '{' has been read
    if (get) ts.get();
    std::unique_ptr<Node> ang_expr {};
    std::unique_ptr<Node> dict = std::make_unique<Node>(NodeKind::DICT, Token{TokenID::Punctuation, "{}"});
    std::unique_ptr<Node> set = std::make_unique<Node>(NodeKind::SET, Token{TokenID::Punctuation, "{}"});
    
    if (expect_token(TokenID::Punctuation, "}")) {
        //empty dict
        ang_expr = move(dict);
    }
    else if (expect_token(TokenID::Operator, {"**"})) {
        ang_expr = expect_double_starred_kvpairs(false);
        dict->add_node_front(ang_expr);
        ang_expr = move(dict);
    }
    else {
        ang_expr = expect_star_named_expression(false);
        
        // If we have expression
        // If comma is after -> star_named_expressions -> set
        if (expect_token(TokenID::Punctuation, {","})) {
            std::unique_ptr<Node> star_named_expressions = expect_star_named_expressions(false, Flag{false, false, false, false, false, true} | flags);
            star_named_expressions->add_node_front(ang_expr);
            set->add_node_front(star_named_expressions);
            ang_expr = move(set);
        }
        // If we have named_expression -> setcomp
        else if (expect_node(ang_expr, NodeKind::OPERATOR_CALL, ":=")) {
            // setcomp->add_node_front(expect_for_if_clauses(false));
            set->add_node_front(ang_expr);
            ang_expr = move(set);
        }
        // If colon is after -> dictionary
        else if (expect_token(TokenID::Operator, {":"})) {
            // construct kvpair
            std::unique_ptr<Node> kvpair = expect_kvpair(false, Flag{false, false, false, false, false, true});
            kvpair->add_node_front(ang_expr);
            std::unique_ptr<Node> more_double_starred_kvpairs = expect_double_starred_kvpairs(false, {false, true, false, false, false, true});
            if (more_double_starred_kvpairs != nullptr) {
                more_double_starred_kvpairs->add_node_front(kvpair);
                dict->add_node_front(more_double_starred_kvpairs);
            }
            else dict->add_node_front(kvpair);
            ang_expr = move(dict);
        }
        else {
            set->add_node_front(ang_expr);
            ang_expr = move(set);
        }
    }
    // Check for '}' delimiter
    if (expect_token(TokenID::Punctuation, "}")) return ang_expr;
    // Must have delimiter, or else throw syntax error
    else throw SyntaxError();
}


std::unique_ptr<Node> Parser::expect_double_starred_kvpairs(bool get, Flag flags) {
    // double_starred_kvpairs: ','.double_starred_kvpair+ [',']
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_double_starred_kvpair, &Parser::expect_double_starred_kvpair, Flag{false, false, true} | flags);
}

std::unique_ptr<Node> Parser::expect_double_starred_kvpair(bool get, Flag flags) {
    // double_starred_kvpair:
    //     | '**' bitwise_or 
    //     | kvpair
    return expect_helper(get, TokenID::Operator, {"**"}, NodeKind::OPERATOR_CALL, &Parser::expect_kvpair, &Parser::expect_bitwise_or, Flag{true} | flags);
}

std::unique_ptr<Node> Parser::expect_kvpair(bool get, Flag flags) {
    // kvpair: expression ':' expression 
    return expect_helper(get, TokenID::Operator, {":"}, NodeKind::PAIR, &Parser::expect_expression, &Parser::expect_expression, flags);
}

std::unique_ptr<Node> Parser::expect_for_if_clauses(bool get, Flag flags) {
    // for_if_clauses:
    //     | for_if_clause+
    return expect_helper(get, TokenID::Identifier, {"for"}, NodeKind::COMPREHENSION, &Parser::expect_for_if_clauses, &Parser::expect_for_if_clause, Flag{false, false, true, false, false, false} | flags);
}

std::unique_ptr<Node> Parser::expect_for_if_clause(bool get, Flag flags) {
    // for_if_clause:
    //     | ASYNC 'for' star_targets 'in' ~ disjunction ('if' disjunction )* 
    //     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
    // Assumes 'for' has just been read 
    std::unique_ptr<Node> for_clause = expect_helper(get, TokenID::Identifier, {"in"}, NodeKind::OPERATOR_CALL, &Parser::expect_star_targets, &Parser::expect_disjunction, Flag{false, false, false, false, false, false, true} | flags);
    for_clause->add_node_front(expect_helper(get, TokenID::Identifier, {"if"}, NodeKind::OPERATOR_CALL, &Parser::expect_disjunction, &Parser::expect_disjunction, Flag{false, true, false, false, true, true} | flags));
    return for_clause;
}

std::unique_ptr<Node> Parser::expect_yield_expr(bool get, Flag flags) {
    // yield_expr:
    //     | 'yield' 'from' expression 
    //     | 'yield' [star_expressions]
    // Assumes that yield has already been found 
    return nullptr;
}

std::unique_ptr<Node> Parser::expect_star_target(bool get, Flag flags = {}) {
    // star_targets:
    //     | star_target !',' 
    //     | star_target (',' star_target )* [','] 
    return expect_helper(get, TokenID::Punctuation, {","}, NodeKind::COLLECTION, &Parser::expect_star_target, &Parser::expect_star_target, flags);
}

std::unique_ptr<Node> Parser::expect_star_target(bool get, Flag flags = {}) {
    // star_target:
    //     | '*' (!'*' star_target) 
    //     | target_with_star_atom
    return expect_helper(get, TokenID::Operator, {"*"}, NodeKind::OPERATOR_CALL, &expect_target_with_star_atom, &expect_star_target, Flag{true} | flags);
}

std::unique_ptr<Node> expect_target_with_star_atom(bool  get, Flag flags = {}) {
    // target_with_star_atom:
    //     | t_primary '.' NAME !t_lookahead 
    //     | t_primary '[' slices ']' !t_lookahead 
    //     | star_atom
}

std::unique_ptr<Node> expect_star_atom(bool get, Flag flags = {}) {
    // star_atom:
    //     | NAME 
    //     | '(' target_with_star_atom ')' 
    //     | '(' [star_targets_tuple_seq] ')' 
    //     | '[' [star_targets_list_seq] ']' 
}

/* helpers for helpers
*
*
*
*
*/

std::unique_ptr<Node> Parser::expect_helper(bool get, 
                                                TokenID id, 
                                                    std::initializer_list<std::string> exp_v, 
                                                        NodeKind kind,
                                                            std::unique_ptr<Node> (Parser::*left)(bool, Flag), 
                                                                std::unique_ptr<Node> (Parser::*right)(bool, Flag),
                                                                    Flag flags) {
    // If recusive: Right is the recursive value, left is the non-recursive value
    std::unique_ptr<Node> left_val = nullptr;
    // If parent is optional, then children are optional
    if (!flags.recursive and !flags.left_found) {
         left_val = (this->*left)(get, {false, flags.optional});
         if (flags.optional and left_val == nullptr) return left_val;
    }
    else if (get) ts.get();

    while (true) {
        const Token& current_token = ts.peek();
        if (expect_token(current_token, TokenID::WhiteSpace)) {
            ts.get();
        }
        else if (expect_token(current_token, id, exp_v)) {
            if (flags.right_restricted) throw SyntaxError();
            flags.oper_required = false;

            std::unique_ptr<Node> new_left_val = std::make_unique<Node> (kind, current_token);
            std::unique_ptr<Node> right_val = (this->*right)(true, {false, flags.optional or flags.right_optional});
            bool successful_match = right_val != nullptr;
            new_left_val->add_node_front(right_val);
            if (!flags.recursive) {
                new_left_val->add_node_front(left_val); 
                left_val = move(new_left_val);
                if (!successful_match or flags.once) return left_val;
            }
            else {return new_left_val;}
        }
        else {
            if (flags.optional and flags.oper_required and flags.left_found) return nullptr;
            if (flags.oper_required) throw SyntaxError();
    
            if (flags.recursive) left_val = (this->*left)(false, {false, flags.optional});
            if (!flags.optional and left_val == nullptr) {
                throw SyntaxError();
            }
            else
                return left_val;
        }
    }
}

bool Parser::expect_token(const Token& tok, TokenID id, std::initializer_list<std::string> exp_v) {
    for (auto s : exp_v) {
        if ((tok.string_value == s)) return expect_token(tok, id);
    }
    return false;
}

bool Parser::expect_token(TokenID id, std::string s) {
    const Token& tok = ts.peek();
    return expect_token(tok, id) and tok.string_value == s;
}

bool Parser::expect_token(TokenID id) {
    const Token& tok = ts.peek();
    return expect_token(tok, id);
}

bool Parser::expect_token(const Token& tok, TokenID id) {
    return tok.id == id;
}

bool Parser::expect_node(const std::unique_ptr<Node>& node, NodeKind kind, std::string s) {
    return expect_node(node, kind) and (node->tok.string_value == s);
}

bool Parser::expect_node(const std::unique_ptr<Node>& node, NodeKind kind) {
    return (node->kind == kind);
}