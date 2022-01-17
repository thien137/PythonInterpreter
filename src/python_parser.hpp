#include "python_tokens.hpp"
#include <initializer_list>
#include <functional>

// AST 

enum class NodeKind {
    BLOCK, LITERAL, COLLECTION, PAIR, 
    COMPREHENSION, TUPLE, LIST, SET, DICT, NAME, 
    FUNCTION_CALL, OPERATOR_CALL, ATTRIBUTE_CALL, 
    KEYWORD_CALL, FUNCTION_DEF, PARAMETERS,
    CLASS_DEF, FORMALITY
};

struct Node {
    NodeKind kind;
    Token tok;
    std::deque<std::unique_ptr<Node>> block {};
    Node(NodeKind k, Token t) : kind{k}, tok{t} {}

    void add_node_front(std::unique_ptr<Node>&& n) {if (n != nullptr) block.push_front(move(n));}
    void add_node_front(std::unique_ptr<Node>& n) {if (n != nullptr) block.push_front(move(n));}
    void add_node_back(std::unique_ptr<Node>&& n) {if (n != nullptr) block.push_back(move(n));}
    void add_node_back(std::unique_ptr<Node>& n) {if (n != nullptr) block.push_back(move(n));}
    void print(std::string);
};

struct Flag {
    bool recursive = false;
    bool optional = false;
    bool right_optional = false;
    bool right_restricted = false;
    bool oper_required = false;
    bool left_found = false;
    bool once = false;

    Flag operator| (const Flag& other) {
        return Flag{recursive or other.recursive, optional or other.optional, right_optional or other.right_optional, right_restricted or other.right_restricted, oper_required or other.oper_required, left_found or other.left_found, once or other.once};
    }
};

class Parser {
    private:
        TokenStream ts;

        // helpers
        std::unique_ptr<Node> expect_statements(bool, Flag = {});
        // statements: statement+ 
        
        std::unique_ptr<Node> expect_statement(bool, Flag = {});
        // statement: compound_stmt  | simple_stmts 

        std::unique_ptr<Node> expect_simplestatements(bool, Flag = {});
        // simple_stmts:
        //     | simple_stmt !';' NEWLINE  # Not needed, there for speedup
        //     | ';'.simple_stmt+ [';'] NEWLINE 

        std::unique_ptr<Node> expect_compoundstatement(bool, Flag = {});
        // compound_stmt:
        //     | function_def
        //     | if_stmt
        //     | class_def
        //     | with_stmt
        //     | for_stmt
        //     | try_stmt
        //     | while_stmt
        //     | match_stmt

        std::unique_ptr<Node> expect_simplestatement(bool, Flag = {});
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

        std::unique_ptr<Node> expect_assignment(bool, Flag = {});
        // assignment:
        //     | NAME ':' expression ['=' annotated_rhs ] 
        //     | ('(' single_target ')' 
        //          | single_subscript_attribute_target) ':' expression ['=' annotated_rhs ] 
        //     | (star_targets '=' )+ (yield_expr | star_expressions) !'=' [TYPE_COMMENT] 
        //     | single_target augassign ~ (yield_expr | star_expressions)

        std::unique_ptr<Node> expect_augassign(bool, Flag = {}); 
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

        std::unique_ptr<Node> expect_global_stmt(bool, Flag = {});
        // global_stmt: 'global' ','.NAME+

        std::unique_ptr<Node> expect_nonlocal_stmt(bool, Flag = {}); 
        // nonlocal_stmt: 'nonlocal' ','.NAME+ 

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

        std::unique_ptr<Node> expect_import_from(bool, Flag = {});
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

        std::unique_ptr<Node> expect_import_from_as_name(bool, Flag = {});
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

        std::unique_ptr<Node> expect_key_value_pattern(bool, Flag = {});
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

        std::unique_ptr<Node> expect_positional_patterns(bool, Flag = {});
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

        std::unique_ptr<Node> expect_default(bool, Flag = {});
        // default: '=' expression 
        
        std::unique_ptr<Node> expect_decorators(bool, Flag = {});
        // decorators: ('@' named_expression NEWLINE )+ 

        std::unique_ptr<Node> expect_class_def(bool, Flag = {});
        // class_def:
        //     | decorators class_def_raw 
        //     | class_def_raw

        std::unique_ptr<Node> expect_class_def_raw(bool, Flag = {});
        // class_def_raw:
        //     | 'class' NAME ['(' [arguments] ')' ] ':' block 

        std::unique_ptr<Node> expect_block(bool, Flag = {});
        // block:
        //     | NEWLINE INDENT statements DEDENT 
        //     | simple_stmts

        std::unique_ptr<Node> expect_star_expressions(bool, Flag = {});
        // star_expressions:
        //     | star_expression (',' star_expression )+ [','] 
        //     | star_expression ',' 
        //     | star_expression

        std::unique_ptr<Node> expect_star_expression(bool, Flag = {});
        // star_expression:
        //     | '*' bitwise_or 
        //     | expression

        std::unique_ptr<Node> expect_star_named_expressions(bool, Flag = {});
        // star_named_expressions: ','.star_named_expression+ [','] 

        std::unique_ptr<Node> expect_star_named_expression(bool, Flag = {});
        // star_named_expression:
        //     | '*' bitwise_or 
        //     | named_expression

        std::unique_ptr<Node> expect_assignment_expression(bool, Flag = {});
        // assignment_expression:
        //     | NAME ':=' ~ expression 

        std::unique_ptr<Node> expect_named_expression(bool, Flag = {});
        // named_expression:
        //     | assignment_expression
        //     | expression !':='

        std::unique_ptr<Node> expect_expressions(bool, Flag = {});
        // expressions:
        //     | expression (',' expression )+ [','] 
        //     | expression ',' 
        //     | expression

        std::unique_ptr<Node> expect_expression(bool, Flag = {});
        // expression:
        //     | disjunction 'if' disjunction 'else' expression 
        //     | disjunction
        //     | lambdef

        std::unique_ptr<Node> expect_lambdef(bool, Flag = {});
        // lambdef:
        //     | 'lambda' [lambda_params] ':' expression 

        std::unique_ptr<Node> expect_lambdef(bool, Flag = {});
        // lambda_params:
        //     | lambda_parameters

        // # lambda_parameters etc. duplicates parameters but without annotations
        // # or type comments, and if there's no comma after a parameter, we expect
        // # a colon, not a close parenthesis.  (For more, see parameters above.)
        // #

        std::unique_ptr<Node> expect_lambda_parameters(bool, Flag = {});
        // lambda_parameters:
        //     | lambda_slash_no_default lambda_param_no_default* lambda_param_with_default* [lambda_star_etc] 
        //     | lambda_slash_with_default lambda_param_with_default* [lambda_star_etc] 
        //     | lambda_param_no_default+ lambda_param_with_default* [lambda_star_etc] 
        //     | lambda_param_with_default+ [lambda_star_etc] 
        //     | lambda_star_etc 

        std::unique_ptr<Node> expect_lambda_slash_no_default(bool, Flag = {});
        // lambda_slash_no_default:
        //     | lambda_param_no_default+ '/' ',' 
        //     | lambda_param_no_default+ '/' &':' 

        std::unique_ptr<Node> expect_lambda_slash_with_default(bool, Flag = {});
        // lambda_slash_with_default:
        //     | lambda_param_no_default* lambda_param_with_default+ '/' ',' 
        //     | lambda_param_no_default* lambda_param_with_default+ '/' &':' 

        std::unique_ptr<Node> expect_lambda_star_etc(bool, Flag = {});
        // lambda_star_etc:
        //     | '*' lambda_param_no_default lambda_param_maybe_default* [lambda_kwds] 
        //     | '*' ',' lambda_param_maybe_default+ [lambda_kwds] 
        //     | lambda_kwds 

        std::unique_ptr<Node> expect_lambda_kwds(bool, Flag = {});
        // lambda_kwds: '**' lambda_param_no_default 

        std::unique_ptr<Node> expect_lambda_param_no_default(bool, Flag = {});
        // lambda_param_no_default:
        //     | lambda_param ',' 
        //     | lambda_param &':' 

        std::unique_ptr<Node> expect_lambda_param_with_default(bool, Flag = {});
        // lambda_param_with_default:
        //     | lambda_param default ',' 
        //     | lambda_param default &':' 

        std::unique_ptr<Node> expect_lambda_param_maybe_default(bool, Flag = {});
        // lambda_param_maybe_default:
        //     | lambda_param default? ',' 
        //     | lambda_param default? &':' 

        std::unique_ptr<Node> expect_lambda_param(bool, Flag = {});
        // lambda_param: NAME 

        std::unique_ptr<Node> expect_disjunction(bool, Flag = {});
        // disjunction:
        //     | conjunction ('or' conjunction )+ 
        //     | conjunction

        std::unique_ptr<Node> expect_conjunction(bool, Flag = {});
        // conjunction:
        //     | inversion ('and' inversion )+ 
        //     | inversion

        std::unique_ptr<Node> expect_inversion(bool, Flag = {});
        // inversion:
        //     | 'not' inversion 
        //     | comparison

        std::unique_ptr<Node> expect_comparison(bool, Flag = {});
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
        // HELPERS FOR ABOVE FUNCTION :
        std::unique_ptr<Node> expect_is_bitwise_or(bool, Flag = {});
         // is_bitwise_or: 'is' bitwise_or 
        std::unique_ptr<Node> expect_not_bitwise_or(bool, Flag = {});
        // notin_bitwise_or: 'not' bitwise_or
        std::unique_ptr<Node> expect_in_bitwise_or(bool, Flag = {});
        // in_bitwise_or: 'in' bitwise_or 

        std::unique_ptr<Node> expect_bitwise_or(bool, Flag = {});
        // bitwise_or:
        //     | bitwise_or '|' bitwise_xor 
        //     | bitwise_xor

        std::unique_ptr<Node> expect_bitwise_xor(bool, Flag = {});
        // bitwise_xor:
        //     | bitwise_xor '^' bitwise_and 
        //     | bitwise_and

        std::unique_ptr<Node> expect_bitwise_and(bool, Flag = {});
        // bitwise_and:
        //     | bitwise_and '&' shift_expr 
        //     | shift_expr

        std::unique_ptr<Node> expect_shift_expr(bool, Flag = {});
        // shift_expr:
        //     | shift_expr '<<' sum 
        //     | shift_expr '>>' sum 
        //     | sum

        std::unique_ptr<Node> expect_sum(bool, Flag = {});
        // sum:
        //     | sum '+' term 
        //     | sum '-' term 
        //     | term

        std::unique_ptr<Node> expect_term(bool, Flag = {});
        // term:
        //     | term '*' factor 
        //     | term '/' factor 
        //     | term '//' factor 
        //     | term '%' factor 
        //     | term '@' factor 
        //     | factor

        std::unique_ptr<Node> expect_factor(bool, Flag = {});
        // factor:
        //     | '+' factor 
        //     | '-' factor 
        //     | '~' factor 
        //     | power

        std::unique_ptr<Node> expect_power(bool, Flag = {});
        // power:
        //     | await_primary '**' factor 
        //     | await_primary

        std::unique_ptr<Node> expect_primary(bool, Flag = {});
        // primary:
        //     | primary '.' NAME 
        //     | primary genexp 
        //     | primary '(' [arguments] ')' 
        //     | primary '[' slices ']' 
        //     | atom

        std::unique_ptr<Node> expect_atomic(bool, Flag = {});
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

        std::unique_ptr<Node> expect_strings(bool, Flag = {});
        // strings: STRING+ 

        std::unique_ptr<Node> handle_square_brackets(bool, Flag = {});

        std::unique_ptr<Node> expect_list(bool, Flag = {});
        // list:
        //     | '[' [star_named_expressions] ']' 

        std::unique_ptr<Node> expect_listcomp(bool, Flag = {});
        // listcomp:
        //     | '[' named_expression for_if_clauses ']' 

        std::unique_ptr<Node> handle_parentheses(bool, Flag = {});

        std::unique_ptr<Node> expect_tuple(bool, Flag = {});
        // tuple:
        //     | '(' [star_named_expression ',' [star_named_expressions]  ] ')' 

        std::unique_ptr<Node> expect_group(bool, Flag = {});
        // group:
        //     | '(' (yield_expr | named_expression) ')' 

        std::unique_ptr<Node> expect_genexp(bool, Flag = {});
        // genexp:
        //     | '(' ( assignment_expression | expression !':=') for_if_clauses ')' 

        std::unique_ptr<Node> handle_angled_brackets(bool, Flag = {});

        std::unique_ptr<Node> expect_set(bool, Flag = {});
        // set: '{' star_named_expressions '}' 

        std::unique_ptr<Node> expect_setcomp(bool, Flag = {});
        // setcomp:
        //     | '{' named_expression for_if_clauses '}' 

        std::unique_ptr<Node> expect_dict(bool, Flag = {});
        // dict:
        //     | '{' [double_starred_kvpairs] '}' 
        //     | '{' invalid_double_starred_kvpairs '}'

        std::unique_ptr<Node> expect_dictcomp(bool, Flag = {});
        // dictcomp:
        //     | '{' kvpair for_if_clauses '}' 

        std::unique_ptr<Node> expect_double_starred_kvpairs(bool, Flag = {});
        // double_starred_kvpairs: ','.double_starred_kvpair+ [','] 

        std::unique_ptr<Node> expect_double_starred_kvpair(bool, Flag = {});
        // double_starred_kvpair:
        //     | '**' bitwise_or 
        //     | kvpair

        std::unique_ptr<Node> expect_kvpair(bool, Flag = {});
        // kvpair: expression ':' expression 

        std::unique_ptr<Node> expect_for_if_clauses(bool, Flag = {});
        // for_if_clauses:
        //     | for_if_clause+ 

        std::unique_ptr<Node> expect_for_if_clause(bool, Flag = {});
        // for_if_clause:
        //     | ASYNC 'for' star_targets 'in' ~ disjunction ('if' disjunction )* 
        //     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )* 

        std::unique_ptr<Node> expect_yield_expr(bool, Flag = {});
        // yield_expr:
        //     | 'yield' 'from' expression 
        //     | 'yield' [star_expressions] 
        
        std::unique_ptr<Node> expect_arguments(bool, Flag = {});
        // arguments:
        //     | args [','] &')' 

        std::unique_ptr<Node> expect_args(bool, Flag = {});
        // args:
        //     | ','.(starred_expression | ( assignment_expression | expression !':=') !'=')+ [',' kwargs ] 
        //     | kwargs 

        std::unique_ptr<Node> expect_kwargs(bool, Flag = {});
        // kwargs:
        //     | ','.kwarg_or_starred+ ',' ','.kwarg_or_double_starred+ 
        //     | ','.kwarg_or_starred+
        //     | ','.kwarg_or_double_starred+

        std::unique_ptr<Node> expect_starred_expression(bool, Flag = {});
        // starred_expression:
        //     | '*' expression

        std::unique_ptr<Node> expect_kwarg_or_starred(bool, Flag = {}); 
        // kwarg_or_starred:
        //     | NAME '=' expression 
        //     | starred_expression 

        std::unique_ptr<Node> expect_kwarg_or_double_starred(bool, Flag = {});
        // kwarg_or_double_starred:
        //     | NAME '=' expression 
        //     | '**' expression 

        // NOTE: star_targets may contain *bitwise_or, targets may not.

        std::unique_ptr<Node> expect_star_targets(bool, Flag = {});
        // star_targets:
        //     | star_target !',' 
        //     | star_target (',' star_target )* [','] 

        std::unique_ptr<Node> expect_star_targets_list_seq(bool, Flag = {});
        // star_targets_list_seq: ','.star_target+ [','] 

        std::unique_ptr<Node> expect_star_targets_tuple_seq(bool, Flag = {});
        // star_targets_tuple_seq:
        //     | star_target (',' star_target )+ [','] 
        //     | star_target ',' 

        std::unique_ptr<Node> expect_star_target(bool, Flag = {});
        // star_target:
        //     | '*' (!'*' star_target) 
        //     | target_with_star_atom

        std::unique_ptr<Node> expect_target_with_star_atom(bool, Flag = {});
        // target_with_star_atom:
        //     | t_primary '.' NAME !t_lookahead 
        //     | t_primary '[' slices ']' !t_lookahead 
        //     | star_atom

        std::unique_ptr<Node> expect_star_atom(bool, Flag = {});
        // star_atom:
        //     | NAME 
        //     | '(' target_with_star_atom ')' 
        //     | '(' [star_targets_tuple_seq] ')' 
        //     | '[' [star_targets_list_seq] ']' 

        std::unique_ptr<Node> expect_single_target(bool, Flag = {});
        // single_target:
        //     | single_subscript_attribute_target
        //     | NAME 
        //     | '(' single_target ')' 

        std::unique_ptr<Node> expect_single_subscript_attribute_target(bool, Flag = {});
        // single_subscript_attribute_target:
        //     | t_primary '.' NAME !t_lookahead 
        //     | t_primary '[' slices ']' !t_lookahead 

        std::unique_ptr<Node> expect_del_targets(bool, Flag  = {});
        // del_targets: ','.del_target+ [','] 

        std::unique_ptr<Node> expect_del_target(bool, Flag = {});
        // del_target:
        //     | t_primary '.' NAME !t_lookahead 
        //     | t_primary '[' slices ']' !t_lookahead 
        //     | del_t_atom

        std::unique_ptr<Node> expect_del_t_atom(bool, Flag = {});
        // del_t_atom:
        //     | NAME 
        //     | '(' del_target ')' 
        //     | '(' [del_targets] ')' 
        //     | '[' [del_targets] ']' 

        std::unique_ptr<Node> expect_t_primary(bool, Flag = {});
        // t_primary:
        //     | t_primary '.' NAME &t_lookahead 
        //     | t_primary '[' slices ']' &t_lookahead 
        //     | t_primary genexp &t_lookahead 
        //     | t_primary '(' [arguments] ')' &t_lookahead 
        //     | atom &t_lookahead 

        std::unique_ptr<Node> expect_t_primary(bool, Flag = {});
        // t_lookahead: '(' | '[' | '.'

        std::unique_ptr<Node> expect_name(bool, Flag = {});
        // NAME

        std::unique_ptr<Node> expect_dedent(bool, Flag = {});
        // DEDENT

        std::unique_ptr<Node> expect_newline(bool, Flag = {});
        // NEWLINE

        std::unique_ptr<Node> expect_indent(bool, Flag = {});
        // INDENT

        std::unique_ptr<Node> expect_number(bool, Flag = {});
        // NUMBER

        std::unique_ptr<Node> expect_type_comment(bool, FLag = {});
        // TYPE_COMMENT

        // helpers for helpers
        
        //expect certain kinds of identifier tokens
        std::unique_ptr<Node> expect_helper(bool, 
                                            TokenID, 
                                            std::initializer_list<std::string>, 
                                            NodeKind, std::unique_ptr<Node> (Parser::*)(bool, Flag), 
                                            std::unique_ptr<Node> (Parser::*)(bool, Flag), 
                                            Flag = {});
        
        std::unique_ptr<Node> expect_token_get(bool, TokenID, std::string, NodeKind, Flag);
        std::unique_ptr<Node> expect_token_get(bool, TokenID, std::initializer_list<std::string>, NodeKind, Flag); 

        bool expect_token(TokenID, std::initializer_list<std::string>);
        bool expect_token(TokenID, std::string);
        bool expect_token(TokenID);
        bool expect_node(const std::unique_ptr<Node>&, NodeKind, std::string);
        bool expect_node(const std::unique_ptr<Node>&, NodeKind);
        NodeKind token_to_nodekind(const Token&);

        // order of operations

        // Python Model
        /* Python Keywords
        AND, AS, ASSERT, BREAK, CLASS, CONTINUE, DEF,
        DEL, ELIF, ELSE, EXCEPT, FALSE, FINALLY, FOR,
        FROM, GLOBAL, IF, IMPORT, IN, IS, LAMBDA, NONE,
        NONLOCAL, NOT, OR, PASS, RAISE, RETURN, TRUE,
        TRY, WHILE, WITH, YIELD
        */

    public:
        //Node& current() {return curr;}
        Parser(TokenStream& t) : ts{t} {}
        std::unique_ptr<Node> parse_next_line(bool get);
};

