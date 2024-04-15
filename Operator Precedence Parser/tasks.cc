#include <iostream>
#include <cstdio>
#include <cstdlib>
#include "lexer.h"
#include "execute.h"
#include "tasks.h"
#include <algorithm>
#include <stack>
#include <unordered_map>
#include <queue>

using namespace std;

LexicalAnalyzer lexer;

// creating a boolean to check if the variable access has been accessed
bool in_variable_access = false;

// Defining an enum to hold the actions of the Hash Table
enum Actions
{
    less_pres,
    error,
    greater_pres,
    accept_state,
    equal_pres
};

// creating a enum for TreeNodeType
enum tree_node_type
{
    ARRAY_DEC
};

// Creating an enum to hold the type (terminal or non-terminal)
enum snodeType
{
    term,
    expr
};

using hashMap = unordered_map<TokenType, Actions>;
using HashTable = unordered_map<TokenType, hashMap>;

// Defining the Hash Table
HashTable hashTable = {
    {PLUS, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, less_pres}, {DIV, less_pres}, {LPAREN, less_pres}, {RPAREN, greater_pres}, {LBRAC, less_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, greater_pres}}},
    {MINUS, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, less_pres}, {DIV, less_pres}, {LPAREN, less_pres}, {RPAREN, greater_pres}, {LBRAC, less_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, greater_pres}}},
    {DIV, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, greater_pres}, {DIV, greater_pres}, {LPAREN, less_pres}, {RPAREN, greater_pres}, {LBRAC, less_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, greater_pres}}},
    {MULT, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, greater_pres}, {DIV, greater_pres}, {LPAREN, less_pres}, {RPAREN, greater_pres}, {LBRAC, less_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, greater_pres}}},
    {LPAREN, hashMap{{PLUS, less_pres}, {MINUS, less_pres}, {MULT, less_pres}, {DIV, less_pres}, {LPAREN, less_pres}, {RPAREN, equal_pres}, {LBRAC, less_pres}, {DOT, error}, {RBRAC, less_pres}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, error}}},
    {RPAREN, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, greater_pres}, {DIV, greater_pres}, {LPAREN, error}, {RPAREN, greater_pres}, {LBRAC, greater_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, error}, {ID, error}, {END_OF_FILE, greater_pres}}},
    {LBRAC, hashMap{{PLUS, less_pres}, {MINUS, less_pres}, {MULT, less_pres}, {DIV, less_pres}, {LPAREN, less_pres}, {RPAREN, less_pres}, {LBRAC, less_pres}, {DOT, equal_pres}, {RBRAC, equal_pres}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, error}}},
    {DOT, hashMap{{PLUS, error}, {MINUS, error}, {MULT, error}, {DIV, error}, {LPAREN, error}, {RPAREN, error}, {LBRAC, error}, {DOT, error}, {RBRAC, equal_pres}, {NUM, error}, {ID, error}, {END_OF_FILE, error}}},
    {ID, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, greater_pres}, {DIV, greater_pres}, {LPAREN, error}, {RPAREN, greater_pres}, {LBRAC, greater_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, error}, {ID, error}, {END_OF_FILE, greater_pres}}},
    {RBRAC, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, greater_pres}, {DIV, greater_pres}, {LPAREN, error}, {RPAREN, greater_pres}, {LBRAC, greater_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, error}, {ID, error}, {END_OF_FILE, greater_pres}}},
    {NUM, hashMap{{PLUS, greater_pres}, {MINUS, greater_pres}, {MULT, greater_pres}, {DIV, greater_pres}, {LPAREN, error}, {RPAREN, greater_pres}, {LBRAC, greater_pres}, {DOT, error}, {RBRAC, greater_pres}, {NUM, error}, {ID, error}, {END_OF_FILE, greater_pres}}},
    {END_OF_FILE, hashMap{{PLUS, less_pres}, {MINUS, less_pres}, {MULT, less_pres}, {DIV, less_pres}, {LPAREN, less_pres}, {RPAREN, error}, {LBRAC, less_pres}, {DOT, error}, {RBRAC, error}, {NUM, less_pres}, {ID, less_pres}, {END_OF_FILE, accept_state}}},
};

// creating a queue to hold the tree nodes
queue<TreeNode *> tree_nodes;

// Creating the peek_symbol function
Token peek_symbol()
{
    // creating a token to hold the peeked token
    Token token = lexer.peek(1);

    // handling the END_OF_EXPR
    Token endOfExpr;
    endOfExpr.token_type = END_OF_FILE;
    endOfExpr.lexeme = "$";
    endOfExpr.line_no = token.line_no;

    // creating a token for the END_OF_EXPR
    Token END_OF_EXPR = endOfExpr;

    // checking if the token is a SEMICOLON, EQUAL or END_OF_EXPR
    if (token.token_type == SEMICOLON)
    {
        return END_OF_EXPR;
    }
    else if (token.token_type == EQUAL)
    {
        return END_OF_EXPR;
    }
    else
    {
        return token;
    }
}

// Creating the get_symbol Function
Token get_symbol()
{
    Token token = peek_symbol();

    if (token.token_type != END_OF_FILE)
    {
        return lexer.GetToken();
    }
}

// Creating the expect function to help recursively parse the grammar
Token expect(TokenType expType, string token)
{
    // calling the getToken() method to get the next token
    Token expect_token = lexer.GetToken();
    string errorOutput = "SNYATX EORRR !!!";

    // checking if the token type is equal to the expected token type
    if (expect_token.token_type != expType)
    {
        cout << errorOutput << endl;
        exit(1);
    }

    // returning the token
    return expect_token;
} // end of expect

// creating a stacknode for the storing stack node
struct StackNode
{
    Token token_term;
    snodeType node_type;
    TreeNode *tree_node_expression;
};

// creating a vector to hold the scalar variables
vector<string> scalar_variable_vector;

// creating vector to hold the array variables
vector<string> array_variable_vector;

void syntaxError()
{
    cout << "SNYATX EORRR !!!" << endl;
    exit(1);
}

////////////////////////////////// Start of Task 1 ///////////////////////////////////////

// program -> decl-section block
void parse_program()
{
    // calling declaration section in the grammar
    parse_decl_section();

    // calling block in the grammar
    parse_block();
}

// decl-section -> scalar-decl-section array-decl-section
void parse_decl_section()
{
    // calling scalar decl section in the grammar
    parse_scalar_decl_section();

    // calling the array decl section in the grammar
    parse_array_decl_section();
}

// scalar-decl-section -> SCALAR id-list
void parse_scalar_decl_section()
{
    // expect SCALAR
    expect(SCALAR, "SCALAR");

    // checking if the next token is ID
    if (lexer.peek(1).token_type == ID)
    {
        // calling id list in the grammar
        Token id_list_scalar = parse_id_list(SCALAR);

        // pushing the lexeme of the token to the scalar variables vector
        scalar_variable_vector.push_back(id_list_scalar.lexeme);
    }
    else // else print error message
    {
        syntaxError();
    }
}

// array-decl-section -> ARRAY id-list
void parse_array_decl_section()
{
    //  expect ARRAY
    expect(ARRAY, "ARRAY");

    // checking if the next token is ID
    if (lexer.peek(1).token_type == ID)
    {
        // calling id list in the grammar
        Token id_list_array = parse_id_list(ARRAY);

        // pushing the lexeme of the token to the array variables vector
        array_variable_vector.push_back(id_list_array.lexeme);
    }
    else // else print error message
    {
        syntaxError();
    }
}

// id-list -> ID
// id-list -> ID id-list
Token parse_id_list(TokenType id_type)
{
    //  expect ID and storing it in the id_token
    Token id_token = expect(ID, "ID");

    // if next token is ID, calling id list in the grammar
    if (lexer.peek(1).token_type == ID)
    {
        // calling id list in the grammar
        // if the id type is scalar
        if (id_type == SCALAR)
        {
            // calling id list in the grammar and storing it in the scalar_id_list
            Token scalar_id_list = parse_id_list(SCALAR);

            // pushing the lexeme of the token to the scalar variables vector
            scalar_variable_vector.push_back(scalar_id_list.lexeme);
        }
        // if the id type is array
        else if (id_type == ARRAY)
        {
            // calling id list in the grammar and storing it in the array_id_list
            Token array_id_list = parse_id_list(ARRAY);

            // pushing the lexeme of the token to the array variables vector
            array_variable_vector.push_back(array_id_list.lexeme);
        }
    }

    return id_token;
}

// block -> LBRACE stmt-list RBRACE
void parse_block()
{
    // expect LBRACE
    expect(LBRACE, "LBRACE");

    // calling stmt list in the grammar
    parse_stmt_list();

    // expect RBRACE
    expect(RBRACE, "RBRACE");
}

// stmt-list -> stmt
// stmt-list -> stmt stmt-list
void parse_stmt_list()
{
    // calling stmt in the grammar
    parse_stmt();

    // if next token is ID or OUTPUT, calling stmt list in the grammar
    if (lexer.peek(1).token_type == ID || lexer.peek(1).token_type == OUTPUT)
    {
        parse_stmt_list();
    }
}

// stmt -> assign-stmt
// stmt -> output-stmt
void parse_stmt()
{
    // if next token is ID, calling assign stmt in the grammar
    if (lexer.peek(1).token_type == ID)
    {
        parse_assign_stmt();
    }
    // if next token is OUTPUT, calling output stmt in the grammar
    else if (lexer.peek(1).token_type == OUTPUT)
    {
        parse_output_stmt();
    }
    else
    {
        syntaxError();
    }
}

// assign-stmt -> variable-access EQUAL expr SEMICOLON
void parse_assign_stmt()
{
    // creating a tree node to hold the parent node
    TreeNode *parent_node = new TreeNode();

    // calling the variable access in the grammar
    parent_node->child.left_child = parse_variable_access();

    // expect EQUAL
    expect(EQUAL, "EQUAL");

    parent_node->label = "=";                            // setting the label of the parent node to =
    parent_node->id.line_number = lexer.peek(1).line_no; // setting the line number of the parent node to the line number of the peek token

    // calling expr in the grammar
    parent_node->child.right_child = parse_expr();

    // expect SEMICOLON
    expect(SEMICOLON, "SEMICOLON");

    tree_nodes.push(parent_node);
}

// output-stmt -> OUTPUT variable-access SEMICOLON
void parse_output_stmt()
{
    // expect OUTPUT
    expect(OUTPUT, "OUTPUT");

    // calling variable access in the grammar
    parse_variable_access();

    // expect SEMICOLON
    expect(SEMICOLON, "SEMICOLON");
}

// variable-access -> ID
// variable-access -> ID LBRAC expr RBRAC
// variable-access -> ID LBRAC DOT RBRAC
TreeNode *parse_variable_access()
{
    // creating a tree node for the left hand side
    TreeNode *left_parent_node = new TreeNode();

    if (lexer.peek(1).token_type == ID && lexer.peek(2).token_type == LBRAC) // checking if the token after the ID is a LBRAC
    {
        in_variable_access = true;
        left_parent_node = parse_expr();
        in_variable_access = false;
    }
    // checking if the first token is an ID
    else if (lexer.peek(1).token_type == ID)
    {
        // expect ID and storing it in the token t
        Token t = expect(ID, "ID");

        // if the token t exists in the scalar variables vector
        if (find(scalar_variable_vector.begin(), scalar_variable_vector.end(), t.lexeme) == scalar_variable_vector.end())
        {
            left_parent_node->operator_token_type = ERROR;
        }
        // if the token t exists in the array variables vector
        else
        {
            left_parent_node->operator_token_type = SCALAR;
        }

        // setting the root label of the left parent node to the lexeme of the token
        left_parent_node->label = "ID \"" + t.lexeme + "\"";
    }
    else
    {
        syntaxError();
    }

    return left_parent_node;
}

// expr -> expr PLUS expr
// expr -> expr MINUS expr
// expr -> expr MULT expr
// expr -> expr DIV expr
// expr -> LPAREN expr RPAREN
// expr -> expr LBRAC expr RBRAC
// expr -> expr LBRAC DOT RBRAC
// expr -> primary
TreeNode *parse_expr()
{
    // creating a stack to hold the stack nodes
    stack<StackNode *> expression_stack;

    // checking the first token and storing it in the token variable
    Token checkToken = peek_symbol();

    // if the token is END_OF_FILE, return error
    if (checkToken.token_type == END_OF_FILE)
    {
        syntaxError();
    }

    // creating an initial stack node to hold the stack node
    StackNode *initial_stack_node = new StackNode();

    // handling the END_OF_EXPR
    Token endOfExpr;
    endOfExpr.token_type = END_OF_FILE;
    endOfExpr.lexeme = "$";
    endOfExpr.line_no = checkToken.line_no;

    // setting and pushing the END_OF_EXPR in the stack node
    initial_stack_node->token_term = endOfExpr;
    initial_stack_node->node_type = term;

    // pushing the initial stack node to the stack
    expression_stack.push(initial_stack_node);

    // checking if the top of the stack EOE and then creating while loop
    while (expression_stack.top()->token_term.token_type != END_OF_FILE || checkToken.token_type != END_OF_FILE)
    {
        // creating a stack node to hold the top of the stack
        StackNode *top_of_stack = expression_stack.top();

        // checking the type of the top of the stack
        TokenType checkTokenType = checkToken.token_type;

        // if the top of the stack is a expression
        if (top_of_stack->node_type == expr)
        {
            // creating a temporary stack node to hold the top of the stack
            StackNode *temp_stack = top_of_stack;

            // popping the top of the stack
            expression_stack.pop();

            // setting the token to the top of the stack token
            top_of_stack = expression_stack.top();

            // pushing the temporary stack node back to the stack to return the order of the stack
            expression_stack.push(temp_stack);
        }

        // checking the hash table for lower presedence or equal
        if (hashTable[top_of_stack->token_term.token_type][checkTokenType] == equal_pres || hashTable[top_of_stack->token_term.token_type][checkTokenType] == less_pres)
        {
            // creating a token that is being consumed every time
            Token token_consumed = get_symbol();

            if (token_consumed.token_type == NUM || token_consumed.token_type == ID)
            {
                // creating a tree node
                TreeNode *tree_node = new TreeNode();

                // setting the operator of the tree node to the token type
                tree_node->expression_operator = token_consumed.token_type;

                // checking if checkToken is ID, and token_consumed lexeme is in scalar_variables
                if ((token_consumed.token_type == ID) && (find(scalar_variable_vector.begin(), scalar_variable_vector.end(), token_consumed.lexeme) != scalar_variable_vector.end()))
                {
                    tree_node->label = "ID \"" + token_consumed.lexeme + "\"";
                    tree_node->operator_token_type = SCALAR;
                }
                // checking if checkToken is ID, and token_consumed lexeme is in array_variables
                else if ((token_consumed.token_type == ID) && (find(array_variable_vector.begin(), array_variable_vector.end(), token_consumed.lexeme) != array_variable_vector.end()))
                {
                    tree_node->operator_token_type = ARRAY_DEC;
                    tree_node->label = "ID \"" + token_consumed.lexeme + "\"";
                }
                else if (token_consumed.token_type == NUM)
                {
                    tree_node->operator_token_type = SCALAR;
                    tree_node->label = "NUM \"" + token_consumed.lexeme + "\"";
                }
                else
                {
                    tree_node->operator_token_type = ERROR;
                }

                tree_node->id.line_number = token_consumed.line_no;  // setting the line number to the line number of the token
                tree_node->id.variable_Name = token_consumed.lexeme; // setting the variable name to the lexeme of the token

                // creating a stack node to hold the tree node
                StackNode *temp_stack_node_expression = new StackNode();
                temp_stack_node_expression->node_type = expr;                 // setting the node type to expr
                temp_stack_node_expression->tree_node_expression = tree_node; // setting the tree node to the tree node
                temp_stack_node_expression->token_term.token_type = ERROR;    // setting the token type to error

                // pushing the stack node to the stack
                expression_stack.push(temp_stack_node_expression);
            }
            else
            {
                StackNode *temp_stack_node_term = new StackNode();
                temp_stack_node_term->node_type = term;            // setting the node type to term
                temp_stack_node_term->token_term = token_consumed; // setting the token to the token consumed

                // pushing the stack node to the stack
                expression_stack.push(temp_stack_node_term);
            }
        }
        // checking the hash table for greater precedence
        else if (hashTable[top_of_stack->token_term.token_type][checkTokenType] == greater_pres)
        {
            // creating a stack node to hold the last pop term
            StackNode *last_popped_term = nullptr;

            // creating a stack to hold the stack nodes on the right hand side
            stack<StackNode *> RHS_stack;

            do
            {
                // creaing a stack node to hold the top of the stack for reduction
                StackNode *reduction_stack_node = expression_stack.top();

                // popping the top of the stack
                expression_stack.pop();

                // if the top of the stack is a term, saving it in the last popped term
                if (reduction_stack_node->node_type == term)
                {
                    last_popped_term = reduction_stack_node;
                }

                // pushing the reduction stack node to the RHS stack
                RHS_stack.push(reduction_stack_node);

            } while (expression_stack.top()->node_type != term ||
                     last_popped_term == nullptr ||
                     hashTable[expression_stack.top()->token_term.token_type][last_popped_term->token_term.token_type] != less_pres);

            // creating a parent tree node to hold the tree node
            TreeNode *parent_tree_node = new TreeNode();

            // checking if the top of the RHS stack is a expr
            if (RHS_stack.top()->node_type == expr)
            {
                // creating a temporary stack node to hold the top of the RHS stack
                StackNode *top_stack_node_temp = RHS_stack.top();

                // setting the left child of the parent node to temp stack node tree node
                parent_tree_node->child.left_child = top_stack_node_temp->tree_node_expression;

                // popping the top of the RHS stack
                RHS_stack.pop();

                // checking if the RHS stack is a term
                if (RHS_stack.top()->node_type == term)
                {
                    // checking if the top of the stack is a term token type of +, -, *, /
                    if (RHS_stack.top()->token_term.token_type == LBRAC ||
                        RHS_stack.top()->token_term.token_type == PLUS ||
                        RHS_stack.top()->token_term.token_type == DIV ||
                        RHS_stack.top()->token_term.token_type == MULT ||
                        RHS_stack.top()->token_term.token_type == MINUS)
                    {
                        // setting the operator of the parent tree node to the RHS stack top token type
                        parent_tree_node->expression_operator = RHS_stack.top()->token_term.token_type;

                        // putting the label of the parent tree node to the operator label handler
                        parent_tree_node->label = operatorLabelHandler(RHS_stack.top()->token_term.token_type);

                        // popping the top of the RHS stack
                        RHS_stack.pop();

                        // checking if the RHS stack is a expr
                        if (!RHS_stack.empty() && RHS_stack.top()->node_type == expr)
                        {
                            // putting the left child of the parent tree node to the top of the RHS stack tree node
                            parent_tree_node->child.right_child = RHS_stack.top()->tree_node_expression;

                            // popping the top of the RHS stack
                            RHS_stack.pop();

                            // checking if the left child is a SCALAR and the right child operator_token_type (+, -, *, /) is an SCALAR
                            if (parent_tree_node->child.left_child->operator_token_type == SCALAR && parent_tree_node->child.right_child->operator_token_type == SCALAR && (parent_tree_node->expression_operator == PLUS || parent_tree_node->expression_operator == MINUS || parent_tree_node->expression_operator == MULT || parent_tree_node->expression_operator == DIV))
                            {
                                parent_tree_node->operator_token_type = SCALAR;
                            }
                            // checking is the left child is ARRAY and the right child is also ARRAY
                            else if (parent_tree_node->child.left_child->operator_token_type == ARRAY && parent_tree_node->child.right_child->operator_token_type == ARRAY)
                            {
                                // checking if the operator is a MULT
                                if (parent_tree_node->expression_operator == MULT)
                                {
                                    parent_tree_node->operator_token_type = SCALAR;
                                }
                                // checking if the operator is a PLUS or MINUS
                                else if (parent_tree_node->expression_operator == PLUS || parent_tree_node->expression_operator == MINUS)
                                {
                                    parent_tree_node->operator_token_type = ARRAY;
                                }
                                else
                                {
                                    parent_tree_node->operator_token_type = ERROR;
                                }
                            }
                            // checking if the left child is an ARRAY and the right child is a SCALAR and the opertor type is LBRAC
                            else if (parent_tree_node->child.left_child->operator_token_type == ARRAY && parent_tree_node->child.right_child->operator_token_type == SCALAR && parent_tree_node->expression_operator == LBRAC)
                            {
                                parent_tree_node->operator_token_type = SCALAR;
                            }
                            // checking if the left child is an ARRAY_DEC and the right child is a SCALAR and the operator type is LBRAC
                            else if (parent_tree_node->child.left_child->operator_token_type == ARRAY_DEC && parent_tree_node->child.right_child->operator_token_type == SCALAR && parent_tree_node->expression_operator == LBRAC)
                            {
                                parent_tree_node->operator_token_type = SCALAR;
                            }
                            else
                            {
                                parent_tree_node->operator_token_type = ERROR;
                            }

                            // creating a stack node to hold the parent tree node
                            StackNode *reducing_temp_stack_node = new StackNode();

                            // setting the node type to expr
                            reducing_temp_stack_node->node_type = expr;
                            reducing_temp_stack_node->token_term.token_type = ERROR;

                            // setting the tree node to the parent tree node
                            reducing_temp_stack_node->tree_node_expression = parent_tree_node;

                            // pushing the reducing temp stack node to the RHS stack
                            expression_stack.push(reducing_temp_stack_node);

                            // checking if the RHS stack is not empty
                            if (!RHS_stack.empty())
                            {
                                // checking if the RHS stack top token type is a RBRAC
                                if (RHS_stack.top()->token_term.token_type == RBRAC)
                                {
                                    // pop the top of the rightHandSideStack
                                    RHS_stack.pop();
                                }
                                else
                                {
                                    syntaxError();
                                }
                            }
                        }
                        // checking if the RHS stack is not empty and the token type is a DOT
                        else if (!RHS_stack.empty() && RHS_stack.top()->token_term.token_type == DOT)
                        {
                            // makeing the parent tree node right child to nullptr
                            parent_tree_node->child.right_child = nullptr;

                            // making the parent expression node label [.]
                            parent_tree_node->label = "[.]";

                            // popping the top of the RHS stack
                            RHS_stack.pop();

                            // checking if the parent tree node left child operator token type is an SCALAR
                            if (in_variable_access == true && parent_tree_node->child.left_child->operator_token_type == ARRAY_DEC)
                            {
                                parent_tree_node->operator_token_type = ARRAY;
                            }
                            // checking if the parent tree node left child operator token type is an ARRAY_DEC
                            else if (in_variable_access == true && parent_tree_node->child.left_child->operator_token_type != ARRAY_DEC)
                            {
                                parent_tree_node->operator_token_type = ERROR;
                            }
                            // checking if the parent tree node left child operator token type is an ARRAY
                            else if (parent_tree_node->child.left_child->operator_token_type == SCALAR || parent_tree_node->child.left_child->operator_token_type == ARRAY_DEC)
                            {
                                parent_tree_node->operator_token_type = ARRAY;
                            }
                            else
                            {
                                parent_tree_node->operator_token_type = ERROR;
                            }

                            // create a reduced stack node to hold the parent tree node
                            StackNode *reduced_stack_node = new StackNode();

                            // setting the node type to expr
                            reduced_stack_node->node_type = expr;
                            reduced_stack_node->token_term.token_type = ERROR;

                            // setting the tree node to the parent tree node
                            reduced_stack_node->tree_node_expression = parent_tree_node;

                            // pushing the reduced stack node to the RHS stack
                            expression_stack.push(reduced_stack_node);

                            if (!RHS_stack.empty())
                            {
                                if (RHS_stack.top()->token_term.token_type == RBRAC)
                                {
                                    // pop the top of the rightHandSideStack
                                    RHS_stack.pop();
                                }
                                else
                                {
                                    syntaxError();
                                }
                            }
                        }
                        else
                        {
                            syntaxError();
                        }
                    }
                    else
                    {
                        syntaxError();
                    }
                }
                else
                {
                    syntaxError();
                }
            }
            else if (RHS_stack.top()->token_term.token_type == LPAREN)
            {
                // popping the top of the RHS stack
                RHS_stack.pop();

                // checking if the RHS stack is a expr
                if (RHS_stack.top()->node_type == expr)
                {
                    // pushing the top of RHS Stack to the expression stack
                    expression_stack.push(RHS_stack.top());

                    // popping the top of the RHS stack
                    RHS_stack.pop();

                    // checking if the RHS stack is not empty and the token type is a RPAREN
                    if (RHS_stack.top()->token_term.token_type == RPAREN)
                    {
                        // popping the top of the RHS stack
                        RHS_stack.pop();
                    }
                    else
                    {
                        syntaxError();
                    }
                }
                else
                {
                    syntaxError();
                }
            }
            else
            {
                syntaxError();
            }
        }
        // checking the hash table for the accept state
        else if (hashTable[top_of_stack->token_term.token_type][checkTokenType] == accept_state)
        {
            if (expression_stack.top()->tree_node_expression->operator_token_type == ARRAY_DEC)
            {
                expression_stack.top()->tree_node_expression->operator_token_type = ERROR;
            }
            return expression_stack.top()->tree_node_expression;
        }
        else
        {
            syntaxError();
        }

        // checking the next token to help start the loop again
        checkToken = peek_symbol();
    }

    return nullptr;
}

// primary -> ID
// primary -> NUM
void parse_primary()
{
    // expect ID
    if (lexer.peek(1).token_type == ID)
    {
        expect(ID, "ID");
    }
    // expect NUM
    else if (lexer.peek(1).token_type == NUM)
    {
        expect(NUM, "NUM");
    }
    else
    {
        syntaxError();
    }
}

// creating a function to handle the operator label
string operatorLabelHandler(TokenType tokenType)
{
    // if plus, return +
    if (tokenType == PLUS)
    {
        return "+";
    }
    // if mult, return *
    else if (tokenType == MULT)
    {
        return "*";
    }
    // if LBRAC, return []
    else if (tokenType == LBRAC)
    {
        return "[]";
    }
    // if minus, return -
    else if (tokenType == MINUS)
    {
        return "-";
    }
    // if div, return /
    else if (tokenType == DIV)
    {
        return "/";
    }
    else
    {
        return "ERROR";
    }
}

void printAST(TreeNode *parent)
{
    // Checking if the parent node is null
    if (parent == nullptr)
    {
        return;
    }

    // creating a queue to push and pop and print the nodes of the AST
    queue<TreeNode *> q;

    // pushing the parent node to the queue
    q.push(parent);

    // while the queue is not empty
    while (!q.empty())
    {
        // creating a temporary node to hold the front of the queue
        TreeNode *temp = q.front();

        // popping the front of the queue
        q.pop();

        // printing the root label of the node
        cout << temp->label << " ";

        // if the left child of the node is not null, pushing it to the queue
        if (temp->child.left_child != nullptr)
        {
            q.push(temp->child.left_child);
        }

        // if the right child of the node is not null, pushing it to the queue
        if (temp->child.right_child != nullptr)
        {
            q.push(temp->child.right_child);
        }
    }
    cout << endl;
}

////////////////////////////////////////////////////////////////////////////////////////

// Task 1
void parse_and_generate_AST()
{

    // calling the program in the grammar
    parse_program();

    // getting the first node to from the tree node
    TreeNode *parent_node = tree_nodes.front();

    // printing the AST
    printAST(parent_node);
}

////////////////////////////////////////////////////////////////////////////////////////

// Task 2
void parse_and_type_check()
{
    // calling the program in the grammar
    parse_program();

    bool errorFound = false;

    // getting the first node to from the tree node
    TreeNode *parent_node = tree_nodes.front();

    // creating a while loop to iterate through the tree nodes
    while (!tree_nodes.empty())
    {
        if (parent_node->child.left_child->operator_token_type == ERROR || parent_node->child.right_child->operator_token_type == ERROR)
        {
            if (!errorFound)
            {
                cout << "Disappointing expression type error :(" << endl
                     << endl;
                errorFound = true;
            }
            cout << "Line " << parent_node->id.line_number << endl;
        }
        else if (parent_node->child.left_child->operator_token_type == SCALAR && parent_node->child.right_child->operator_token_type != SCALAR)
        {
            if (!errorFound)
            {
                cout << "The following assignment(s) is/are invalid :(" << endl
                     << endl;
                errorFound = true;
            }
            cout << "Line " << parent_node->id.line_number << endl;
        }

        tree_nodes.pop();

        parent_node = tree_nodes.front();
    }

    if (!errorFound)
    {
        cout << "Amazing! No type errors here :)" << endl;
    }
}

////////////////////////////////////////////////////////////////////////////////////////

// Task 3
instNode *parse_and_generate_statement_list()
{
    cout << "3" << endl;

    // The following is the hardcoded statement list
    // generated for a specific program
    // you should replace this code with code that parses the
    // input and generayes a statement list
    //
    // program
    // SCALAR a b c d
    // ARRAY x y z
    // a = 1;
    // b = 2;
    // c = 3;
    // d = (a+b)*(b+c);
    // OUTPUT d;
    // x[a+b] = d;
    // OUTPUT x[3];
    //
    //  a will be at location 0
    //  b will be at location 1
    //  c will be at location 2
    //  d will be at location 3
    //  x will be at location 4 - 13
    //  y will be at location 14 - 23
    //  z will be at location 24 - 33
    //  t1 will be at location 34 : intermediate value for (a+b)
    //  t2 will be at location 35 : intermediate value for (b+c)
    //  t3 will be at location 36 : intermediate value (a+b)*(b+c)
    //  t4 will be at location 37 : intermediate value for a+b index of array
    //  t5 will be at location 38 : intermediate value for addr of x[a+b] =
    //                              address_of_x + value of a+b =
    //                              4 + value of a+b
    //  t6 will be at location 39 : intermediate value for addr of x[3] =
    //                              address_of_x + value of 3 =
    //                              4 + value of 3 (computation is not done at
    //                              compile time)
    //
    instNode *i01 = new instNode();
    i01->lhsat = DIRECT;
    i01->lhs = 0;             // a
    i01->iType = ASSIGN_INST; // =
    i01->op1at = IMMEDIATE;
    i01->op1 = 1;        // 1
    i01->oper = OP_NOOP; // no operator

    instNode *i02 = new instNode();
    i02->lhsat = DIRECT;
    i02->lhs = 1;             // b
    i02->iType = ASSIGN_INST; // =
    i02->op1at = IMMEDIATE;
    i02->op1 = 2;        // 2
    i02->oper = OP_NOOP; // no operator

    i01->next = i02;

    instNode *i03 = new instNode();
    i03->lhsat = DIRECT;
    i03->lhs = 2;             // c
    i03->iType = ASSIGN_INST; // =
    i03->op1at = IMMEDIATE;
    i03->op1 = 3;        // 3
    i03->oper = OP_NOOP; // no operator

    i02->next = i03;

    instNode *i1 = new instNode();
    i1->lhsat = DIRECT;
    i1->lhs = 34;            // t1
    i1->iType = ASSIGN_INST; // =
    i1->op1at = DIRECT;
    i1->op1 = 0;        // a
    i1->oper = OP_PLUS; // +
    i1->op2at = DIRECT;
    i1->op2 = 1; // b

    i03->next = i1;

    instNode *i2 = new instNode();
    i2->lhsat = DIRECT;
    i2->lhs = 35;            // t2
    i2->iType = ASSIGN_INST; // =
    i2->op1at = DIRECT;
    i2->op1 = 1;        // b
    i2->oper = OP_PLUS; // +
    i2->op2at = DIRECT;
    i2->op2 = 2; // c

    i1->next = i2;

    instNode *i3 = new instNode();
    i3->lhsat = DIRECT;
    i3->lhs = 36;            // t3
    i3->iType = ASSIGN_INST; // =
    i3->op1at = DIRECT;
    i3->op1 = 34;       // t1
    i3->oper = OP_MULT; // *
    i3->op2at = DIRECT;
    i3->op2 = 35; // t2

    i2->next = i3; // i3 should be after i1 and i2

    instNode *i4 = new instNode();
    i4->lhsat = DIRECT;
    i4->lhs = 3;             // d
    i4->iType = ASSIGN_INST; // =
    i4->op1at = DIRECT;
    i4->op1 = 36;       // t3
    i4->oper = OP_NOOP; // no operator

    i3->next = i4;

    instNode *i5 = new instNode();
    i5->iType = OUTPUT_INST; // OUTPUT
    i5->op1at = DIRECT;
    i5->op1 = 3; // d

    i4->next = i5;

    instNode *i6 = new instNode();
    i6->lhsat = DIRECT;
    i6->lhs = 37;            // t4
    i6->iType = ASSIGN_INST; // =
    i6->op1at = DIRECT;
    i6->op1 = 0;        // a
    i6->oper = OP_PLUS; // +
    i6->op2at = DIRECT;
    i6->op2 = 1; // b
    i5->next = i6;

    instNode *i7 = new instNode();
    i7->lhsat = DIRECT;
    i7->lhs = 38;            // t5
    i7->iType = ASSIGN_INST; // =
    i7->op1at = IMMEDIATE;
    i7->op1 = 4;        // address of x = 4 available
                        // at compile time
    i7->oper = OP_PLUS; // +
    i7->op2at = DIRECT;
    i7->op2 = 37; // t5 (contains value of a+b

    i6->next = i7;

    instNode *i8 = new instNode();
    i8->lhsat = INDIRECT;
    i8->lhs = 38;            // x[a+b]
    i8->iType = ASSIGN_INST; // =
    i8->op1at = DIRECT;
    i8->op1 = 3; // d
    i8->oper = OP_NOOP;

    i7->next = i8;

    instNode *i9 = new instNode();
    i9->lhsat = DIRECT;
    i9->lhs = 39;            // t6 will contain address of x[3]
    i9->iType = ASSIGN_INST; // =
    i9->op1at = IMMEDIATE;
    i9->op1 = 4;        // address of x = 4 available
                        // at compile time
    i9->oper = OP_PLUS; // +
    i9->op2at = IMMEDIATE;
    i9->op2 = 3; // 3

    i8->next = i9;

    instNode *i10 = new instNode();
    i10->iType = OUTPUT_INST; // OUTPUT
    i10->op1at = INDIRECT;
    i10->op1 = 39; // x[3] by providing its
                   // address indirectly through
                   // t6

    i9->next = i10;

    instNode *code = i01;

    return code;
}

////////////////////////////////////////////////////////////////////////////////////////
