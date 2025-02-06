#include <assert.h>
#include <fstream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include <iostream>

#include "ASTNode.hpp"
#include "lexer.hpp"
#include "SymbolTable.hpp"
#include "TokenQueue.hpp"
#include "tools.hpp"    
#include "helpers.hpp"

class Tubular {
private:
    TokenQueue tokens;
    SymbolTable st;
    std::vector<std::unique_ptr<ASTNode>> functions;
    struct OpInfo { size_t precedence; char associativity; };
    std::unordered_map<std::string, OpInfo> op_table;
    WASMContext ctx;
    bool inside_loop = false;
    
public:
    using NodePtr = std::unique_ptr<ASTNode>;
    Tubular(std::string filename) : ctx(st) {    
        std::ifstream in_file(filename);              // Load the input file
        if (in_file.fail()) {
            std::cerr << "ERROR: Unable to open file '" << filename << "'." << std::endl;
            exit(1);
        }

        tokens.Load(in_file);  // Load all tokens from the file.

        op_table = {
            {"*", {1, 'l'}}, {"/", {1, 'l'}}, {"%", {1, 'l'}},
            {"+", {2, 'l'}}, {"-", {2, 'l'}},
            {">", {3, 'n'}}, {"<", {3, 'n'}}, {">=", {3, 'n'}}, {"<=", {3, 'n'}},
            {"==", {4, 'n'}}, {"!=", {4, 'n'}},
            {"&&", {5, 'l'}},
            {"||", {6, 'l'}},
            {"=", {7, 'r'}}
        };
    }

    void Parse()
    {
        // Outer layer can only be function definitions.
        st.PushScope();

        for(auto& i : AVAILABLE_INTRINSICS)
        {
            if(i.calling_info.has_value())
            {   
                st.AddIntrinsicFunction(i.calling_info.value());
            }
        }

        while (tokens.Any())
        {
            functions.push_back(ParseFunction());
        }
        st.PopScope();
    }

    NodePtr ParseFunction()
    {
        tokens.Use(emplex::Lexer::ID_FUNCTION, "Code outside of function definition");
        emplex::Token id_tok = tokens.Use(emplex::Lexer::ID_IDENT, "Improper function definition; No function name given");
        tokens.Use('(');

        std::vector<std::pair<emplex::Token, emplex::Token>> params;
        while(tokens.Any() && !tokens.Is(')'))
        {
            emplex::Token type_tok = tokens.Use(emplex::Lexer::ID_TYPE, "Improper parameter specification; type expected");
            emplex::Token param_id_tok = tokens.Use(emplex::Lexer::ID_IDENT, "Improper parameter specification; identifier expected");
            params.push_back(std::make_pair(type_tok, param_id_tok));
            tokens.UseIf(',');
        }
        tokens.Use(')');
        tokens.Use(':', "Improper function definition; return type expected");
        Type return_type = Types::Parse(tokens.Use(emplex::Lexer::ID_TYPE, "Improper function definition; return type expected"));

        size_t fn_id = st.AddFunction(id_tok.lexeme, return_type);
        if(fn_id == SymbolTable::NO_ID)
        {
            Error(id_tok.line_id, "Redeclaration of function \"", id_tok.lexeme, "\".");
        }

        st.PushScope();
        for(auto& [type_tok, param_id_tok] : params)
        {
            size_t param_id = st.AddVar(Types::Parse(type_tok), param_id_tok.lexeme, true);
            st.GetFunction(fn_id).params.push_back(param_id);
        }

        NodePtr body = ParseBlock();
        Type body_type = body->HasReturn(st);
        if(body_type == Type::NONE)
        {
            Error(tokens.CurLine(), "Functions must end in a return");
        }
        if(body_type != return_type)
        {
            // IMO, this should be an error, but test case 18 allows return an int to a char function.
            // Error(tokens.CurLine(), "Return type does not match function signature (", body_type, " does not match ", return_type, ")");
        }
        st.PopScope();
        
        return std::make_unique<FunctionNode>(id_tok.line_id, fn_id, std::move(body));
    }

    NodePtr ParseBlock()
    {
        tokens.Use('{');
        st.PushScope();

        auto block_node = std::make_unique<BlockNode>(tokens.CurLine());
        bool returns = false;
        while(!tokens.Is('}'))
        {
            if(returns) { Error(tokens.CurLine(), "Code continues past return statement"); }
            NodePtr statement_node = ParseStatement();
            if(statement_node->HasReturn(st) != Type::NONE) { returns = true; }
            block_node->AddChild(std::move(statement_node));
        }

        st.PopScope();
        tokens.Use('}');
        return block_node;
    }

    NodePtr ParseStatement()
    {
        switch(tokens.Peek())
        {
            case emplex::Lexer::ID_TYPE:
                return ParseVar();
            case emplex::Lexer::ID_IF:
                return ParseIf();
            case emplex::Lexer::ID_WHILE:
                return ParseWhile();
            case '{':
                return ParseBlock();
            case emplex::Lexer::ID_BREAK:
                return ParseBreak();
            case emplex::Lexer::ID_CONTINUE:
                return ParseContinue();
            case emplex::Lexer::ID_RETURN:
                return ParseReturn();
            default:
                {
                    NodePtr return_node = ParseExpr();
                    tokens.Use(';');
                    return return_node;
                } 
        }
    }

    NodePtr ParseVar()
    {
        emplex::Token type_tok = tokens.Use(emplex::Lexer::ID_TYPE);
        emplex::Token id_tok = tokens.Use(emplex::Lexer::ID_IDENT);
        size_t var_id = st.AddVar(Types::Parse(type_tok), id_tok.lexeme);
        if(var_id == SymbolTable::NO_ID)
        {
            Error(id_tok.line_id, "Redeclaration of variable \"", id_tok.lexeme, "\".");
        }

        if (tokens.UseIf(';'))
        {
            return std::make_unique<ASTNode>();
        }
        else
        {
            tokens.Use('=');
            auto id_node = std::make_unique<IdentNode>(id_tok.line_id, var_id);
            auto bin_op_node = MakeBinaryOpNode(tokens.CurLine(), '=', std::move(id_node), std::move(ParseExpr()));
            tokens.Use(';');
            return bin_op_node;
        }
    }

    NodePtr ParseIf()
    {
        emplex::Token if_tok = tokens.Use(emplex::Lexer::ID_IF);
        tokens.Use('(');
        NodePtr cond = ParseExpr();
        if(cond->GetType(st) != Type::INT) { Error(if_tok.line_id, "Conditions to if statements must only be integers"); }
        tokens.Use(')');
        NodePtr then = ParseStatement();

        auto if_node = std::make_unique<IfNode>(if_tok.line_id, std::move(cond), std::move(then));

        if(tokens.UseIf(emplex::Lexer::ID_ELSE))
        {
            if_node->AddChild(ParseStatement());
        }

        return if_node;
    }

    NodePtr ParseWhile()
    {
        emplex::Token while_tok = tokens.Use(emplex::Lexer::ID_WHILE);
        tokens.Use('(');
        NodePtr cond = ParseExpr();
        if(cond->GetType(st) != Type::INT) { Error(while_tok.line_id, "Conditions to while statements must only be integers"); }
        tokens.Use(')');
        bool inside_loop = this->inside_loop;

        this->inside_loop = true;
        NodePtr body = ParseStatement();

        this->inside_loop = inside_loop;

        return std::make_unique<WhileNode>(while_tok.line_id, std::move(cond), std::move(body));
    }

    NodePtr ParseBreak()
    {
        if(!inside_loop) { Error(tokens.CurLine(), "A break statement is only allowed inside a loop"); }
        emplex::Token break_tok = tokens.Use(emplex::Lexer::ID_BREAK);
        auto break_node = std::make_unique<BreakNode>(break_tok.line_id);
        tokens.Use(';');
        return break_node;
    }

    NodePtr ParseContinue()
    {
        if(!inside_loop) { Error(tokens.CurLine(), "A continue statement is only allowed inside a loop"); }
        emplex::Token cont_tok = tokens.Use(emplex::Lexer::ID_CONTINUE);
        auto cont_node = std::make_unique<ContinueNode>(cont_tok.line_id);
        tokens.Use(';');
        return cont_node;
    }

    NodePtr ParseReturn()
    {
        emplex::Token ret_tok = tokens.Use(emplex::Lexer::ID_RETURN);
        auto ret_node = std::make_unique<ReturnNode>(ret_tok.line_id, std::move(ParseExpr()));
        tokens.Use(';');

        Type ret_type = ret_node->HasReturn(st);
        Type fn_type = st.GetCurrentFunction().return_type;

        // IMO, this should be an error, but test case 18 allows return an int to a char function.
        //if(ret_type != fn_type) { Error(ret_tok.line_id, "Return statement does not match type of current function (", ret_type , " does not match ", fn_type, ")"); }
        return ret_node;
    }

    NodePtr ParseExpr(size_t precedence_level = SIZE_MAX)  
    {
        NodePtr current_node = ParseUnary();
        size_t precedence_level_to_skip = SIZE_MAX;
        while(tokens.Any())
        {
            emplex::Token op_tok = tokens.Peek();

            if(op_table.count(op_tok.lexeme) == 0) break;
            OpInfo op_info = op_table[op_tok.lexeme];

            if(op_info.precedence > precedence_level) break;
            if(op_info.precedence == precedence_level_to_skip)
            {
                Error(op_tok.line_id, "Operator \'", op_tok.lexeme, "\' is non-associative");
            }
            
            // Consume `next_tok` in the stream, which is holding the binary operator we want to use
            tokens.Use();

            size_t next_precedence_level = op_info.precedence;
            if(op_info.associativity != 'r')
            {
                --next_precedence_level;
            }

            NodePtr rhs = ParseExpr(next_precedence_level);

            current_node = MakeBinaryOpNode(op_tok.line_id, op_tok, std::move(current_node), std::move(rhs));
            precedence_level_to_skip = (op_info.associativity == 'n') ? op_info.precedence : SIZE_MAX; 
        }
        return current_node;
    }

    void OpError(int line_num, Type lhs, int op, Type rhs)
    {
        return Error(line_num, "Operator ", char(op), " is not defined between ", Types::Display(lhs), " and ", Types::Display(rhs));
    }
    NodePtr MakeBinaryOpNode(size_t line_num, int op, NodePtr&& lhs, NodePtr&& rhs)
    {
        Type lhs_type = lhs->GetType(st);
        Type rhs_type = rhs->GetType(st);
        
        if(Types::IsNumerical(lhs_type) && Types::IsNumerical(rhs_type) && (lhs_type != Type::CHAR || rhs_type != Type::INT || op != '*'))
        {
            return MakeBinaryOpNode_Numerical(line_num, op, std::move(lhs), std::move(rhs));
        }
        switch(op)
        {
            case '*':
                if(!Types::IsTextual(lhs_type) || rhs_type != Type::INT)
                {
                    OpError(line_num, lhs_type, op, rhs_type);
                }
                else if(lhs_type == Type::CHAR && rhs_type == Type::INT)
                {
                    PromoteTo(st, lhs, Type::STRING);
                }
                break;
            case '+':
                if(!Types::IsTextual(lhs_type) || !Types::IsTextual(rhs_type))
                {
                    OpError(line_num, lhs_type, op, rhs_type);
                }
                if(lhs_type == Type::CHAR)
                {
                    PromoteTo(st, lhs, Type::STRING);
                }
                if(rhs_type == Type::CHAR)
                {
                    PromoteTo(st, rhs, Type::STRING);
                }
                break;
            case '=':
                if(lhs_type != Type::STRING || !Types::IsTextual(rhs_type))
                {
                    OpError(line_num, lhs_type, op, rhs_type);
                }
                if(rhs_type == Type::CHAR)
                {
                    PromoteTo(st, rhs, Type::STRING);
                }
                break;
            case emplex::Lexer::ID_EQUAL:
            case emplex::Lexer::ID_INEQUAL:
                if(lhs_type != Type::STRING || rhs_type != Type::STRING)
                {
                    OpError(line_num, lhs_type, op, rhs_type);
                }
                break;
            default:
                OpError(line_num, lhs_type, op, rhs_type);
                break;
        }
        return std::make_unique<BinaryOpNode>(line_num, op, std::move(lhs), std::move(rhs));
    }

    NodePtr MakeBinaryOpNode_Numerical(size_t line_num, int op, NodePtr&& lhs, NodePtr&& rhs)
    {
        Type lhs_type = lhs->GetType(st);
        Type rhs_type = rhs->GetType(st);

        if((op == '*' || op == '/') && (lhs_type == Type::CHAR || rhs_type == Type::CHAR))
        {
            Error(line_num, "Operation ", char(op), " cannot be used with chars");
        }
        else if(op == '%' && (lhs_type != Type::INT || rhs_type != Type::INT))
        {
            Error(line_num, "Modulus can only be used on integers");
        }
        else if(op == emplex::Lexer::ID_LOGICAL_AND && (lhs_type != Type::INT || rhs_type != Type::INT))
        {
            Error(line_num, "And can only be used on integers");
        }
        else if(op == emplex::Lexer::ID_LOGICAL_OR && (lhs_type != Type::INT || rhs_type != Type::INT))
        {
            Error(line_num, "Or can only be used on integers");
        }
        else if(op == '=' && lhs_type < rhs_type)
        {
            Error(line_num, "Narrowing assignment from ", Types::Display(rhs_type), " to ", Types::Display(lhs_type));
        }
        CastPair(st, lhs, rhs);
        
        return std::make_unique<BinaryOpNode>(line_num, op, std::move(lhs), std::move(rhs));
    }

    NodePtr ParseUnary()
    {
        NodePtr ret_node;
        size_t line_num = tokens.CurLine();
        auto unary_tok = tokens.UseIf('-', '!');
        ret_node = ParseTerm();

        if(tokens.UseIf(':'))
        {
            ret_node = std::make_unique<CastNode>(tokens.CurLine(), std::move(ret_node), Types::Parse(tokens.Use(emplex::Lexer::ID_TYPE)));
        }
        if(tokens.UseIf('['))
        {
            NodePtr index = ParseExpr();
            if(index->GetType(st) == Type::STRING || index->GetType(st) == Type::DOUBLE)
            {
                Error(line_num, "Index value must be an integer type");
            }
            ret_node = std::make_unique<IndexNode>(line_num, std::move(ret_node), std::move(index));
            tokens.Use(']');
        }
    
        if(unary_tok)
        {
            if(unary_tok == '-' && ret_node->GetType(st) == Type::CHAR)
            {
                Error(line_num, "Mathematical negation cannot be applied to chars");
            }
            if(unary_tok == '!' && ret_node->GetType(st) != Type::INT)
            {
                Error(line_num, "Boolean negation can only be applied to integers");
            }
        
            ret_node = std::make_unique<UnaryOpNode>(line_num, unary_tok, std::move(ret_node));
        }

        return ret_node;
    }

    NodePtr ParseTerm()
    {
        emplex::Token tok = tokens.Use();
        switch(tok)
        {
            case emplex::Lexer::ID_IDENT:
                {
                    size_t var_id = st.GetId(tok.lexeme);
                    if(var_id == SymbolTable::NO_ID)
                    {
                        Error(tok.line_id, "Unknown identifier ", tok.lexeme, ".");
                    }

                    if(tokens.Peek() == '(')
                    {
                        tokens.Rewind();
                        return ParseCall();
                    }
                    
                    return std::make_unique<IdentNode>(tok.line_id, var_id);
                }
            case '(':
                {
                    auto ret = ParseExpr();
                    tokens.Use(')');
                    return ret;
                }
            case emplex::Lexer::ID_SQRT:
                {
                    tokens.Use('(');
                    NodePtr term = ParseExpr();
                    tokens.Use(')');
                    PromoteTo(st, term, Type::DOUBLE);
                    return std::make_unique<UnaryOpNode>(tok.line_id, emplex::Lexer::ID_SQRT, std::move(term));
                }
            case emplex::Lexer::ID_LITERAL_CHAR:
                return std::make_unique<LiteralNode>(tok.line_id, Type::CHAR, tok.lexeme[1]);
            case emplex::Lexer::ID_LITERAL_INT:
                return std::make_unique<LiteralNode>(tok.line_id, Type::INT, std::stoi(tok.lexeme));
            case emplex::Lexer::ID_LITERAL_DOUBLE:
                return std::make_unique<LiteralNode>(tok.line_id, Type::DOUBLE, std::stod(tok.lexeme));
            case emplex::Lexer::ID_LITERAL_STRING:
                return std::make_unique<LiteralNode>(tok.line_id, Type::STRING, ctx.RegisterStaticString(tok.lexeme));
            default:
                Error(tok.line_id, "Unexpected token \"", tok.lexeme, "\". Expected identifier, number, string, or \"(\".");
                return std::make_unique<ASTNode>();
                break;
        }
    }

    NodePtr ParseCall()
    {
        size_t fn_id = st.GetId(tokens.Use().lexeme);
        SymbolTable::FnData& fn_data = st.GetFunction(fn_id);
        tokens.Use('(');

        std::unique_ptr<CallNode> call_node = std::make_unique<CallNode>(tokens.CurLine(), fn_id);
        int param_count = 0;
        while(tokens.Any() && tokens.Peek() != ')')
        {     
            NodePtr arg = ParseExpr();
            Type argument_type = arg->GetType(st);

            if(param_count + 1 > fn_data.params.size())
            {
                Error(tokens.CurLine(), std::to_string(param_count + 1), " parameter(s) passed to a function (", fn_data.name, ") that takes ", std::to_string(fn_data.params.size()));
            }

            Type parameter_type = st.GetVar(fn_data.params[param_count]).t;
            if(!Types::CanAssign(parameter_type, argument_type))
            {
                Error(tokens.CurLine(), "Cannot assign value of type ", Types::Display(argument_type), " to parameter of type ", Types::Display(parameter_type), " in function ", fn_data.name);
            }
            call_node->AddChild(std::move(arg));
            param_count += 1;
            tokens.UseIf(',');
        }
        tokens.Use(')');

        if(param_count != fn_data.params.size())
        {
            Error(tokens.CurLine(), "Not enough arguments provided to function ", fn_data.name);
        }
        return call_node;
    }

    void ToWASM(std::ostream& out_file) {
        ctx.AddLine("(module");
        ctx.indent++;
        ctx.AddLine("(memory (export \"memory\") 1)");
        ctx.AddStatics();
        ctx.AddIntrinsics();
        for(auto& func : functions)
        {
            func->ToWASM(ctx);
        }

        ctx.indent--;
        ctx.AddLine(")");

        for(auto& line : ctx.program)
        {
            for(auto i = 0; i < line.indent; i++) { out_file << '\t'; }
            out_file << line.code << std::endl;
        }
    }

    void DebugPrint()
    {
        for(auto& func : functions)
        {
            func->DebugPrint();
        }
    }
};


int main(int argc, char * argv[])
{
    if (argc != 2) {
        std::cout << "Format: " << argv[0] << " [filename]" << std::endl;
        exit(1);
    }

    std::ofstream out_file(argv[2]);
    Tubular prog(argv[1]);
    prog.Parse();
    //prog.DebugPrint();
    prog.ToWASM(std::cout);
}