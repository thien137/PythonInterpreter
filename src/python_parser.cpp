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

std::unique_ptr<Node> Parser::expect_assignment(bool, Flag = {}) {
    // assignment:
    //     | NAME ':' expression ['=' annotated_rhs ] 
    //     | ('(' single_target ')' 
    //          | single_subscript_attribute_target) ':' expression ['=' annotated_rhs ] 
    //     | (star_targets '=' )+ (yield_expr | star_expressions) !'=' [TYPE_COMMENT] 
    //     | single_target augassign ~ (yield_expr | star_expressions)
}

std::unique_ptr<Node> Parser::expect_augassign(bool, Flag = {}) {
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
}

std::unique_ptr<Node> Parser::expect_global_stmt(bool, Flag = {}) {
    // global_stmt: 'global' ','.NAME+
}

std::unique_ptr<Node> Parser::expect_nonlocal_stmt(bool, Flag = {}) {
// nonlocal_stmt: 'nonlocal' ','.NAME+ 
}

std::unique_ptr<Node> expect_yield_stmt(bool, Flag = {});
// yield_stmt: yield_expr 

std::unique_ptr<Node> expect_assert_stmt(bool, Flag = {});
// assert_stmt: 'assert' expression [',' expression ] 

std::unique_ptr<Node> expect_del_stmt(bool, Flag = {});
// del_stmt:
//     | 'del' del_targets &(';' | NEWLINE) 

std::unique_ptr<Node> expect_import_stmt(bool, Flag = {});
// import_stmt: import_name | import_from

std::unique_ptr<Node> expect_import_name(bool, Flag = {});
// import_name: 'import' dotted_as_names 

std::unique_ptr<Node> expect_import_name(bool, Flag = {});
// # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
// import_from:
//     | 'from' ('.' | '...')* dotted_name 'import' import_from_targets 
//     | 'from' ('.' | '...')+ 'import' import_from_targets 

std::unique_ptr<Node> expect_import_from_targets(bool, Flag = {});
// import_from_targets:
//     | '(' import_from_as_names [','] ')' 
//     | import_from_as_names !','
//     | '*' 

std::unique_ptr<Node> expect_import_from_as_names(bool, Flag = {});
// import_from_as_names:
//     | ','.import_from_as_name+ 

std::unique_ptr<Node> expect_import_from_as_names(bool, Flag = {});
// import_from_as_name:
//     | NAME ['as' NAME ] 

std::unique_ptr<Node> expect_dotted_as_names(bool, Flag = {});
// dotted_as_names:
//     | ','.dotted_as_name+ 

std::unique_ptr<Node> expect_dotted_as_name(bool, Flag = {});
// dotted_as_name:
//     | dotted_name ['as' NAME ] 

std::unique_ptr<Node> expect_dotted_name(bool, Flag = {});
// dotted_name:
//     | dotted_name '.' NAME 
//     | NAME

std::unique_ptr<Node> expect_if_stmt(bool, Flag = {});
// if_stmt:
//     | 'if' named_expression ':' block elif_stmt 
//     | 'if' named_expression ':' block [else_block] 

std::unique_ptr<Node> expect_elif_stmt(bool, Flag = {});
// elif_stmt:
//     | 'elif' named_expression ':' block elif_stmt 
//     | 'elif' named_expression ':' block [else_block] 

std::unique_ptr<Node> expect_else_block(bool, Flag = {});
// else_block:
//     | 'else' ':' block 

std::unique_ptr<Node> expect_while_stmt(bool, Flag = {});
// while_stmt:
//     | 'while' named_expression ':' block [else_block] 

std::unique_ptr<Node> expect_for_stmt(bool, Flag = {});
// for_stmt:
//     | 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block] 
//     | ASYNC 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block] 

std::unique_ptr<Node> expect_with_stmt(bool, Flag = {});
// with_stmt:
//     | 'with' '(' ','.with_item+ ','? ')' ':' block 
//     | 'with' ','.with_item+ ':' [TYPE_COMMENT] block 
//     | ASYNC 'with' '(' ','.with_item+ ','? ')' ':' block 
//     | ASYNC 'with' ','.with_item+ ':' [TYPE_COMMENT] block 

std::unique_ptr<Node> expect_with_item(bool, Flag = {});
// with_item:
//     | expression 'as' star_target &(',' | ')' | ':') 
//     | expression 

std::unique_ptr<Node> expect_try_stmt(bool, Flag = {});
// try_stmt:
//     | 'try' ':' block finally_block 
//     | 'try' ':' block except_block+ [else_block] [finally_block] 

std::unique_ptr<Node> expect_except_block(bool, Flag = {});
// except_block:
//     | 'except' expression ['as' NAME ] ':' block 
//     | 'except' ':' block 

std::unique_ptr<Node> expect_finally_block(bool, Flag = {});
// finally_block:
//     | 'finally' ':' block 

std::unique_ptr<Node> expect_match_stmt(bool, Flag = {});
// match_stmt:
//     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT 

std::unique_ptr<Node> expect_subject_expr(bool, Flag = {});
// subject_expr:
//     | star_named_expression ',' star_named_expressions? 
//     | named_expression

std::unique_ptr<Node> expect_case_block(bool, Flag = {});
// case_block:
//     | "case" patterns guard? ':' block 

std::unique_ptr<Node> expect_guard(bool, Flag = {});
// guard: 'if' named_expression 

std::unique_ptr<Node> expect_patterns(bool, Flag = {});
// patterns:
//     | open_sequence_pattern 
//     | pattern

std::unique_ptr<Node> expect_pattern(bool, Flag = {});
// pattern:
//     | as_pattern
//     | or_pattern

std::unique_ptr<Node> expect_as_pattern(bool, Flag = {});
// as_pattern:
//     | or_pattern 'as' pattern_capture_target 

std::unique_ptr<Node> expect_or_pattern(bool, Flag = {});
// or_pattern:
//     | '|'.closed_pattern+ 

std::unique_ptr<Node> expect_closed_pattern(bool, Flag = {});
// closed_pattern:
//     | literal_pattern
//     | capture_pattern
//     | wildcard_pattern
//     | value_pattern
//     | group_pattern
//     | sequence_pattern
//     | mapping_pattern
//     | class_pattern

std::unique_ptr<Node> expect_literal_pattern(bool, Flag = {});
// # Literal patterns are used for equality and identity constraints
// literal_pattern:
//     | signed_number !('+' | '-') 
//     | complex_number 
//     | strings 
//     | 'None' 
//     | 'True' 
//     | 'False' 

std::unique_ptr<Node> expect_literal_expr(bool, Flag = {});
// # Literal expressions are used to restrict permitted mapping pattern keys
// literal_expr:
//     | signed_number !('+' | '-')
//     | complex_number
//     | strings
//     | 'None' 
//     | 'True' 
//     | 'False' 

std::unique_ptr<Node> expect_complex_number(bool, Flag = {});
// complex_number:
//     | signed_real_number '+' imaginary_number 
//     | signed_real_number '-' imaginary_number  

std::unique_ptr<Node> expect_signed_number(bool, Flag = {});
// signed_number:
//     | NUMBER
//     | '-' NUMBER 

std::unique_ptr<Node> expect_signed_real_number(bool, Flag = {});
// signed_real_number:
//     | real_number
//     | '-' real_number 

std::unique_ptr<Node> expect_real_number(bool, Flag = {});
// real_number:
//     | NUMBER 

std::unique_ptr<Node> expect_imaginary_number(bool, Flag = {});
// imaginary_number:
//     | NUMBER 

std::unique_ptr<Node> expect_capture_pattern(bool, Flag = {});
// capture_pattern:
//     | pattern_capture_target 

std::unique_ptr<Node> expect_pattern_capture_target(bool, Flag = {});
// pattern_capture_target:
//     | !"_" NAME !('.' | '(' | '=') 

std::unique_ptr<Node> expect_wildcard_pattern(bool, Flag = {});
// wildcard_pattern:
//     | "_" 

std::unique_ptr<Node> expect_value_pattern(bool, Flag = {});
// value_pattern:
//     | attr !('.' | '(' | '=') 

std::unique_ptr<Node> expect_attr(bool, Flag = {});
// attr:
//     | name_or_attr '.' NAME 

std::unique_ptr<Node> expect_name_or_attr(bool, Flag = {});
// name_or_attr:
//     | attr
//     | NAME

std::unique_ptr<Node> expect_group_pattern(bool, Flag = {});
// group_pattern:
//     | '(' pattern ')' 

std::unique_ptr<Node> expect_sequence_pattern(bool, Flag = {});
// sequence_pattern:
//     | '[' maybe_sequence_pattern? ']' 
//     | '(' open_sequence_pattern? ')'

std::unique_ptr<Node> expect_open_sequence_pattern(bool, Flag = {}); 
// open_sequence_pattern:
//     | maybe_star_pattern ',' maybe_sequence_pattern? 

std::unique_ptr<Node> expect_maybe_sequence_pattern(bool, Flag = {});
// maybe_sequence_pattern:
//     | ','.maybe_star_pattern+ ','? 

std::unique_ptr<Node> expect_maybe_star_pattern(bool, Flag = {});
// maybe_star_pattern:
//     | star_pattern
//     | pattern

std::unique_ptr<Node> expect_star_pattern(bool, Flag = {});
// star_pattern:
//     | '*' pattern_capture_target 
//     | '*' wildcard_pattern 

std::unique_ptr<Node> expect_mapping_pattern(bool, Flag = {});
// mapping_pattern:
//     | '{' '}' 
//     | '{' double_star_pattern ','? '}' 
//     | '{' items_pattern ',' double_star_pattern ','? '}' 
//     | '{' items_pattern ','? '}' 

std::unique_ptr<Node> expect_items_pattern(bool, Flag = {});
// items_pattern:
//     | ','.key_value_pattern+

std::unique_ptr<Node> expect_items_pattern(bool, Flag = {});
// key_value_pattern:
//     | (literal_expr | attr) ':' pattern 

std::unique_ptr<Node> expect_double_star_pattern(bool, Flag = {});
// double_star_pattern:
//     | '**' pattern_capture_target 

std::unique_ptr<Node> expect_class_pattern(bool, Flag = {});
// class_pattern:
//     | name_or_attr '(' ')' 
//     | name_or_attr '(' positional_patterns ','? ')' 
//     | name_or_attr '(' keyword_patterns ','? ')' 
//     | name_or_attr '(' positional_patterns ',' keyword_patterns ','? ')' 

std::unique_ptr<Node> expect_positional_pattern(bool, Flag = {});
// positional_patterns:
//     | ','.pattern+ 

std::unique_ptr<Node> expect_keyword_patterns(bool, Flag = {});
// keyword_patterns:
//     | ','.keyword_pattern+

std::unique_ptr<Node> expect_keyword_pattern(bool, Flag = {});
// keyword_pattern:
//     | NAME '=' pattern 

std::unique_ptr<Node> return_stmt(bool, Flag = {});
// return_stmt:
//     | 'return' [star_expressions] 

std::unique_ptr<Node> expect_raise_stmt(bool, Flag = {});
// raise_stmt:
//     | 'raise' expression ['from' expression ] 
//     | 'raise' 

std::unique_ptr<Node> expect_function_def(bool, Flag = {});
// function_def:
//     | decorators function_def_raw 
//     | function_def_raw

std::unique_ptr<Node> expect_function_def_raw(bool, Flag = {});
// function_def_raw:
//     | 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block 
//     | ASYNC 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block 

std::unique_ptr<Node> expect_func_type_comment(bool, Flag = {});
// func_type_comment:
//     | NEWLINE TYPE_COMMENT &(NEWLINE INDENT)   # Must be followed by indented block
//     | TYPE_COMMENT

std::unique_ptr<Node> expect_params(bool, Flag = {});
// params:
//     | parameters

std::unique_ptr<Node> expect_parameters(bool, Flag = {});
// parameters:
//     | slash_no_default param_no_default* param_with_default* [star_etc] 
//     | slash_with_default param_with_default* [star_etc] 
//     | param_no_default+ param_with_default* [star_etc] 
//     | param_with_default+ [star_etc] 
//     | star_etc 

// # Some duplication here because we can't write (',' | &')'),
// # which is because we don't support empty alternatives (yet).
// #
std::unique_ptr<Node> expect_slash_no_default(bool, Flag = {});
// slash_no_default:
//     | param_no_default+ '/' ',' 
//     | param_no_default+ '/' &')

std::unique_ptr<Node> expect_slash_with_default(bool, Flag = {});
// slash_with_default:
//     | param_no_default* param_with_default+ '/' ',' 
//     | param_no_default* param_with_default+ '/' &')' 

std::unique_ptr<Node> expect_star_etc(bool, Flag = {});
// star_etc:
//     | '*' param_no_default param_maybe_default* [kwds] 
//     | '*' ',' param_maybe_default+ [kwds] 
//     | kwds 

std::unique_ptr<Node> expect_kwds(bool, Flag = {});
// kwds: '**' param_no_default 

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
std::unique_ptr<Node> expect_param_no_default(bool, Flag = {});
// param_no_default:
//     | param ',' TYPE_COMMENT? 
//     | param TYPE_COMMENT? &')' 

std::unique_ptr<Node> expect_param_with_default(bool, Flag = {});
// param_with_default:
//     | param default ',' TYPE_COMMENT? 
//     | param default TYPE_COMMENT? &')' 

std::unique_ptr<Node> expect_param_maybe_default(bool, Flag = {});
// param_maybe_default:
//     | param default? ',' TYPE_COMMENT? 
//     | param default? TYPE_COMMENT? &')' 

std::unique_ptr<Node> expect_param(bool, Flag = {});
// param: NAME annotation? 

std::unique_ptr<Node> expect_annotation(bool, Flag = {});
// annotation: ':' expression 
// default: '=' expression 

std::unique_ptr<Node> expect_decorators(bool, Flag = {});
// decorators: ('@' named_expression NEWLINE )+ 

std::unique_ptr<Node> expect_class_def(bool, Flag = {});
// class_def:
//     | decorators class_def_raw 
//     | class_def_raw

std::unique_ptr<Node> expect_class_def(bool, Flag = {});
// class_def_raw:
//     | 'class' NAME ['(' [arguments] ')' ] ':' block 

std::unique_ptr<Node> expect_block(bool, Flag = {});
// block:
//     | NEWLINE INDENT statements DEDENT 
//     | simple_stmts

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
            is_bitwise_or->add_node(expect_not_bitwise_or(true, flags));
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
                is_not_bitwise_or->add_node(expect_in_bitwise_or(true, flags));
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
            is_not_in_bitwise_or->add_node(expect_bitwise_or(true, flags));
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
        list->add_node(expect_star_named_expressions(false));
        sqr_expr = move(list);
    }
    else {
        sqr_expr = expect_named_expression(false);
        
        // list:
        // | '[' [star_named_expressions] ']' 
        if (expect_token(TokenID::Punctuation, {","})) {
            std::unique_ptr<Node> star_named_expressions = expect_star_named_expressions(false, Flag{false, false, false, false, false, true} | flags);
            star_named_expressions->add_node(sqr_expr);
            list->add_node(star_named_expressions);
            sqr_expr = move(list);
        }
        else if (expect_token(TokenID::Identifier, {"for"})) {
            // listcomp:
            //     | '[' named_expression for_if_clauses ']' 
            list->add_node(expect_for_if_clauses(false, Flag{false, true} | flags));
            list->add_node(sqr_expr);
            sqr_expr = move(list);
        }
        else {
            list->add_node(sqr_expr);
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
            star_named_expressions->add_node(par_expr);
            tuple->add_node(star_named_expressions);
            par_expr = move(tuple);
        }
        else if (expect_token(TokenID::Identifier, {"for"})) {
            // genexp:
            // '(' ( assignment_expression | expression !':=') for_if_clauses ')' 
            tuple->add_node(expect_for_if_clauses(false, Flag{false, true} | flags));
            tuple->add_node(par_expr);
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
        dict->add_node(ang_expr);
        ang_expr = move(dict);
    }
    else {
        ang_expr = expect_star_named_expression(false);
        
        // If we have expression
        // If comma is after -> star_named_expressions -> set
        if (expect_token(TokenID::Punctuation, {","})) {
            std::unique_ptr<Node> star_named_expressions = expect_star_named_expressions(false, Flag{false, false, false, false, false, true} | flags);
            star_named_expressions->add_node(ang_expr);
            set->add_node(star_named_expressions);
            ang_expr = move(set);
        }
        // If we have named_expression -> setcomp
        else if (expect_node(ang_expr, NodeKind::OPERATOR_CALL, ":=")) {
            // setcomp->add_node(expect_for_if_clauses(false));
            set->add_node(ang_expr);
            ang_expr = move(set);
        }
        // If colon is after -> dictionary
        else if (expect_token(TokenID::Operator, {":"})) {
            // construct kvpair
            std::unique_ptr<Node> kvpair = expect_kvpair(false, Flag{false, false, false, false, false, true});
            kvpair->add_node(ang_expr);
            std::unique_ptr<Node> more_double_starred_kvpairs = expect_double_starred_kvpairs(false, {false, true, false, false, false, true});
            if (more_double_starred_kvpairs != nullptr) {
                more_double_starred_kvpairs->add_node(kvpair);
                dict->add_node(more_double_starred_kvpairs);
            }
            else dict->add_node(kvpair);
            ang_expr = move(dict);
        }
        else {
            set->add_node(ang_expr);
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
    std::unique_ptr<Node> for_clause = expect_helper(get, TokenID::Identifier, {"in"}, NodeKind::OPERATOR_CALL, &Parser::expect_star_targets, &Parser::expect_disjunction, flags);
    for_clause->add_node(expect_helper(get, TokenID::Identifier, {"if"}, NodeKind::OPERATOR_CALL, &Parser::expect_disjunction, &Parser::expect_disjunction, Flag{false, true, false, false, true, true} | flags));
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
            new_left_val->add_node(right_val);
            if (!flags.recursive) {
                new_left_val->add_node(left_val); 
                left_val = move(new_left_val);
                if (!successful_match) return left_val;
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