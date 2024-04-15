#include "execute.h"
#include "lexer.h"

using namespace std;

// tree helpers
struct TreeNode 
{
    int expression_operator; // enum type: ID_OPER, PLUS_OPER, MINUS_OPER, DIV_OPER, ARRAY_ELEM_OPER, WHOLE_ARRAY_OPER
    int operator_token_type; 
    string label;
    
    struct {
        string variable_Name;
        int line_number;
    } id;

    struct {
        TreeNode *left_child = nullptr;
        TreeNode *right_child = nullptr;
    } child;

    struct {
        TreeNode *array_expression;
        int line_number;
    } array;
};

// provided tasks
void parse_and_generate_AST();
void parse_and_type_check();
instNode* parse_and_generate_statement_list();

//grammar functions
void parse_program();
void parse_decl_section();
void parse_scalar_decl_section();
void parse_array_decl_section();
Token parse_id_list(TokenType id_type);
void parse_block();
void parse_stmt_list();
void parse_stmt();
void parse_assign_stmt();
void parse_output_stmt();
TreeNode *parse_variable_access();
TreeNode *parse_expr();
void parse_primary();

//parsing helpers and wrapper functions
Token expect(TokenType expected_type, string token = "");
Token peek_symbol();
Token get_symbol();
Token handle_END_OF_EXPR(int line_no);
void printAST(TreeNode *parent);
string operatorLabelHandler(TokenType tokenType);
void syntaxError();
