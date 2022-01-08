#include "python_tokens.hpp"

// AST 

enum class NodeKind {
    BLOCK, LITERAL, NAME, FUNCTION_CALL, OPERATOR_CALL, COLLECTION
};

class Node {
    private:
        NodeKind kind;
        Token tok;
        std::deque<std::unique_ptr<Node>> block {};
    public:
        Node(NodeKind k, Token t) : kind{k}, tok{t} {}

        void add_node(std::unique_ptr<Node>&& n) {if (n != nullptr) block.push_front(move(n));}
        void add_node(std::unique_ptr<Node>& n) {if (n != nullptr) block.push_front(move(n));}
        void print(std::string);
};

class Parser {
    private:
        TokenStream ts;
        // helpers
        std::unique_ptr<Node> expect_statement(bool);
        std::unique_ptr<Node> expect_simplestatements(bool);
        std::unique_ptr<Node> expect_compoundstatement(bool);
        std::unique_ptr<Node> expect_expression(bool);
        //std::unique_ptr<Node> expect_identifier(bool);
        std::unique_ptr<Node> expect_assignment(bool);
        std::unique_ptr<Node> expect_sum(bool);
        // sum:
        //     | sum '+' term 
        //     | sum '-' term 
        //     | term
        std::unique_ptr<Node> expect_term(bool);
        // term:
        //     | term '*' factor 
        //     | term '/' factor 
        //     | term '//' factor 
        //     | term '%' factor 
        //     | term '@' factor 
        //     | factor
        std::unique_ptr<Node> expect_factor(bool);
        // factor:
        //     | '+' factor 
        //     | '-' factor 
        //     | '~' factor 
        //     | power
        std::unique_ptr<Node> expect_power(bool);
        // power:
        //     | await_primary '**' factor 
        //     | await_primary
        std::unique_ptr<Node> expect_primary(bool);
        // primary:
        //     | primary '.' NAME 
        //     | primary genexp 
        //     | primary '(' [arguments] ')' 
        //     | primary '[' slices ']' 
        //     | atom
        std::unique_ptr<Node> expect_atomic(bool);
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

