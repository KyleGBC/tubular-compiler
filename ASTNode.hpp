#pragma once

#include <algorithm>
#include <cmath>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include "Types.hpp"
#include <typeinfo>
#include <optional>
#include "SymbolTable.hpp"
#include "WASMContext.hpp"

class ASTNode {
protected:
    size_t line_num;
public:
    using NodePtr = std::unique_ptr<ASTNode>;
    ASTNode() : line_num(SIZE_MAX) {}
    ASTNode(size_t line_num) : line_num(line_num) {}
    
    virtual Type GetType(SymbolTable& st) const { return Type::NONE; }
    virtual Type HasReturn(SymbolTable& st) const { return Type::NONE; }
    size_t GetLine() { return line_num; }
    virtual std::string ToString() const { return "Empty"; }

    virtual bool CanAssign(SymbolTable& st, Type rhs) const
    {
        return false;
    }
    virtual void Assign(WASMContext& ctx, ASTNode* node)
    {
        assert(false);
    }
    virtual void DebugPrint(int indent = 0) const
    {         
        for(auto i = 0; i < indent; i++) { std::cout << '\t'; }
        std::cout << ToString() << std::endl;
        int x = 1;
    }
    virtual void ToWASM(WASMContext& ctx) const {}
};

class BlockNode : public ASTNode {
protected:
    std::vector<NodePtr> children;
public:
    template <typename... Node_Ts>
    BlockNode(size_t line_num, Node_Ts&&... nodes) : ASTNode(line_num)
    {
        (AddChild(std::move(nodes)), ...);
    }
    void AddChild(NodePtr&& n) { children.push_back(std::move(n)); }


    virtual Type HasReturn(SymbolTable& st) const override
    {
        Type ret = Type::NONE;
        for(auto& child : children)
        {
            if(ret == Type::NONE) { ret = child->HasReturn(st); }
            else if(child->HasReturn(st) != Type::NONE && child->HasReturn(st) != ret) { assert(false); }
        }
        return ret;
    }

    virtual std::string ToString() const override { return "Block"; }
    virtual void DebugPrint(int indent = 0) const override
    {
        ASTNode::DebugPrint(indent);
        for(const auto& node : children) {
            node->DebugPrint(indent + 1);
        }
    }

    virtual void ToWASM(WASMContext& ctx) const override
    {   
        ctx.indent++;
        for(auto& child : children)
        {
            child->ToWASM(ctx);
        }
        ctx.indent--;
    }
};

class FunctionNode : public BlockNode {
protected:
    size_t fn_id;
public:
    FunctionNode(size_t line_num, size_t fn_id, NodePtr&& body) : BlockNode(line_num, body), fn_id(fn_id) {}

    virtual std::string ToString() const override { return "Function"; }

    Type GetType(SymbolTable& st) const override
    {
        return st.GetFunction(fn_id).return_type;
    }

    void ToWASM(WASMContext& ctx) const override
    {
        auto fn_info = ctx.st.GetFunction(fn_id);
        ctx.current_function = fn_info.name;

        std::string sig = "(func $" + ctx.current_function;
        for(size_t param_id : fn_info.params)
        {
            sig += " (param $" + std::to_string(param_id) + " " + Types::ToWASM(ctx.st.GetVar(param_id).t) + ")";
        }
        if(fn_info.return_type != Type::NONE)
        {
            sig += " (result " + Types::ToWASM(fn_info.return_type) + ")";
        }

        ctx.AddLine(sig);
        ctx.indent++;

        for(size_t local : fn_info.locals)
        {
            auto& var_info = ctx.st.GetVar(local);
            ctx.AddLine("(local $" + std::to_string(local) + " " + Types::ToWASM(var_info.t) + ")");
        }

        ctx.AddLine("(block $" + ctx.current_function + "_exit (result " + Types::ToWASM(fn_info.return_type) + ")");
        children.at(0)->ToWASM(ctx);
        ctx.AddLine(")");
        ctx.indent--;
        ctx.AddLine(")");

        ctx.AddLine("(export \"" + ctx.current_function + "\" (func $" + ctx.current_function + "))");
    }
};

class CallNode : public BlockNode
{
protected:
    size_t fn_id;
public:
    CallNode(size_t line_num, size_t fn_id) : fn_id(fn_id), BlockNode(line_num) {}
    virtual std::string ToString() const override { return "Call (" + emplex::Lexer::TokenName(fn_id) + ")"; }
    virtual Type GetType(SymbolTable& st) const override
    {
        return st.GetFunction(fn_id).return_type;
    }

    virtual void ToWASM(WASMContext& ctx) const override
    {
        for(auto&& child : children)
        {
            child->ToWASM(ctx);
        }
        ctx.AddLine("(call $" + ctx.st.GetFunction(fn_id).name + ")");
    }
};

class IdentNode : public ASTNode {
protected: 
    size_t id;
public:
    IdentNode(size_t line_num, size_t var_id) : ASTNode(line_num), id(var_id) {}
    Type GetType(SymbolTable& st) const override { return st.GetVar(id).t; }
    virtual std::string ToString() const override { return "Ident (" + std::to_string(id) + ")"; }
    size_t GetId() const { return id; }
    virtual void ToWASM(WASMContext& ctx) const override
    {
        ctx.AddLine("(local.get $" + std::to_string(id) + ")");
    }
    virtual bool CanAssign(SymbolTable& st, Type rhs)
    {
        return Types::CanAssign(st.GetVar(id).t, rhs);
    }
    virtual void Assign(WASMContext& ctx, ASTNode* node) override
    {
        bool outer_needs_result = ctx.needs_result;
        ctx.needs_result = true;
        node->ToWASM(ctx);
        ctx.needs_result = outer_needs_result;

        ctx.AddLine("(local.set $" + std::to_string(id) + ")");
        if(outer_needs_result)
        {
            ctx.AddLine("(local.get $" + std::to_string(id) + ")");
        }
    }
};

class UnaryOpNode : public BlockNode {
    int op_id;
public:
    UnaryOpNode(size_t line_num, int op_id, NodePtr&& term) : BlockNode(line_num, term), op_id(op_id) {}
    virtual std::string ToString() const override { return "UnaryOp (" + emplex::Lexer::TokenName(op_id) + ")"; }

    virtual Type GetType(SymbolTable& st) const override
    {
        switch (op_id)
        {
            case '-':
                return children.at(0)->GetType(st);
            case '!':
                return Type::INT;
            case emplex::Lexer::ID_SQRT:
                return Type::DOUBLE;
            default:
                assert("false");
                return Type::NONE;
        }
    }

    virtual void ToWASM(WASMContext& ctx) const override
    {   
        assert(children.size() == 1);
        switch(op_id)
        {
            case '-':
                if(children.at(0)->GetType(ctx.st) == Type::INT)
                {
                    ctx.AddLine("(i32.const 0)");
                    children.at(0)->ToWASM(ctx);
                    ctx.AddLine("(i32.sub)");
                }
                else
                {
                    children.at(0)->ToWASM(ctx);
                    ctx.AddLine("(f64.neg)");
                }
                break;
            case '!':
                children.at(0)->ToWASM(ctx);
                ctx.AddLine("(i32.eqz)");
                break;
            case emplex::Lexer::ID_SQRT:
                children.at(0)->ToWASM(ctx);
                ctx.AddLine("(f64.sqrt)");
                break;
        }
    }
};

class BinaryOpNode : public BlockNode {
    int op_id;
public: 
    BinaryOpNode(size_t line_num, int op_id, NodePtr&& lhs, NodePtr&& rhs) : BlockNode(line_num, lhs, rhs), op_id(op_id) {}
    virtual std::string ToString() const override { return "BinaryOp (" + emplex::Lexer::TokenName(op_id) + ")"; }

    virtual Type GetType(SymbolTable& st) const override
    {
        switch(op_id)
        {
            case '<':
            case '>':
            case emplex::Lexer::ID_LESS_EQUAL:
            case emplex::Lexer::ID_GREATER_EQUAL:
            case emplex::Lexer::ID_EQUAL:
            case emplex::Lexer::ID_INEQUAL:
            case emplex::Lexer::ID_LOGICAL_AND:
            case emplex::Lexer::ID_LOGICAL_OR:
                return Type::INT;
            default:
                return children.at(0)->GetType(st);
        }
    }

    void NumberMath(WASMContext& ctx) const
    {
        Type lhs_type = children.at(0)->GetType(ctx.st);
        children.at(0)->ToWASM(ctx);
        children.at(1)->ToWASM(ctx);
        std::string wasm_type = Types::ToWASM(lhs_type);
        std::string op = "(" + wasm_type + ".";
        switch(op_id)
        {
            case '*':
                op += "mul";
                break;
            case '/':
                op += "div";
                if(lhs_type != Type::DOUBLE) { op += "_s"; }
                break;
            case '%':
                op += "rem";
                if(lhs_type != Type::DOUBLE) { op += "_s"; }
                break;
            case '+':
                op += "add";
                break;
            case '-':
                op += "sub";
                break;
            case '<':
                op += "lt";
                if(lhs_type != Type::DOUBLE) { op += "_s"; }
                break;
            case emplex::Lexer::ID_LESS_EQUAL:
                op += "le";
                if(lhs_type != Type::DOUBLE) { op += "_s"; }
                break;
            case '>':
                op += "gt";
                if(lhs_type != Type::DOUBLE) { op += "_s"; }
                break;
            case emplex::Lexer::ID_GREATER_EQUAL:
                op += "ge";
                if(lhs_type != Type::DOUBLE) { op += "_s"; };
                break;
            case emplex::Lexer::ID_EQUAL:
                op += "eq";
                break;
            case emplex::Lexer::ID_INEQUAL:
                op += "ne";
                break;
            }

            ctx.AddLine(op + ")");
            return;
    }

    virtual void ToWASM(WASMContext& ctx) const override
    {
        assert(children.size() == 2);
        if(op_id == '=')
        {  
            children.at(0)->Assign(ctx, children.at(1).get());
        }
        else if(op_id == emplex::Lexer::ID_LOGICAL_AND)
        {   
            AndOr(ctx, false);
        }
        else if(op_id == emplex::Lexer::ID_LOGICAL_OR)
        {
            AndOr(ctx, true);
        }
        else 
        {
            Type lhs_type = children.at(0)->GetType(ctx.st);
            Type rhs_type = children.at(1)->GetType(ctx.st);

            if(lhs_type == Type::STRING)
            {
                children.at(0)->ToWASM(ctx);
                children.at(1)->ToWASM(ctx);
                if(op_id == '+' && rhs_type == Type::STRING)
                {
                    ctx.AddLine("(call $_str_cat)");
                }
                else if(op_id == '*' && rhs_type == Type::INT)
                {
                    ctx.AddLine("(call $_str_repeat)");
                }
                else if(op_id == emplex::Lexer::ID_EQUAL && rhs_type == Type::STRING)
                {
                    ctx.AddLine("(call $_str_eq)");
                }
                else if(op_id == emplex::Lexer::ID_INEQUAL && rhs_type == Type::STRING)
                {
                    ctx.AddLine("(call &_str_eq)");
                    ctx.AddLine("(i32.eqz)");
                }
            }
            else if(Types::IsNumerical(lhs_type) && Types::IsNumerical(rhs_type))
            {
                return NumberMath(ctx);
            }
        }   
    }

    void AndOr(WASMContext& ctx, bool is_or) const
    {
        children.at(0)->ToWASM(ctx);
        ctx.AddLine("(if (result i32)");
        ctx.indent++;

        ctx.AddLine("(then");
        ctx.indent++;
        if(is_or) { ctx.AddLine("(i32.const 1)"); }
        else 
        { 
            children.at(1)->ToWASM(ctx);
            ctx.AddLine("(i32.eqz)");
            ctx.AddLine("(i32.eqz)");
        }
        ctx.indent--;
        ctx.AddLine(")");

        ctx.AddLine("(else");
        ctx.indent++;
        if(is_or)
        {
            children.at(1)->ToWASM(ctx);
            ctx.AddLine("(i32.eqz)");
            ctx.AddLine("(i32.eqz)");
        } 
        else { ctx.AddLine("(i32.const 0)"); }
        ctx.indent--;
        ctx.AddLine(")");

        ctx.indent--;
        ctx.AddLine(")");
    }
    
};

class IfNode : public BlockNode {
public: 
    IfNode(size_t line_num, NodePtr&& cond, NodePtr&& then) : BlockNode(line_num, cond, then) {}
    IfNode(size_t line_num, NodePtr&& cond, NodePtr&& then, NodePtr&& otherwise) : BlockNode(line_num, cond, then, otherwise) {}
    virtual std::string ToString() const override { return "If"; }

    virtual Type HasReturn(SymbolTable& st) const override
    {
        assert(children.size() == 2 || children.size() == 3);  
        if(children.size() == 3 && children.at(1)->HasReturn(st) == children.at(2)->HasReturn(st)) 
        { 
            return children.at(1)->HasReturn(st); 
        }
        return Type::NONE;
    }

    virtual void ToWASM(WASMContext& ctx) const override 
    {
        assert(children.size() == 2 || children.size() == 3);
        children.at(0)->ToWASM(ctx);

        Type child_return_type = HasReturn(ctx.st);
        if(child_return_type != Type::NONE)
        {
            ctx.AddLine("(if (result " + Types::ToWASM(child_return_type) + ")");
        }
        else 
        {
            ctx.AddLine("(if ");
        }
        
        ctx.indent++;

        ctx.AddLine("(then");
        ctx.indent++;
        children.at(1)->ToWASM(ctx);
        ctx.indent--;
        ctx.AddLine(")");

    
        if(children.size() == 3)
        {
            ctx.AddLine("(else");
            ctx.indent++;
            children.at(2)->ToWASM(ctx);
            ctx.indent--;
            ctx.AddLine(")");
        }  

        ctx.indent--;
        ctx.AddLine(")");
    }
};

class WhileNode : public BlockNode {
public:
    WhileNode(size_t line_num, NodePtr&& cond, NodePtr&& body) : BlockNode(line_num, cond, body) {}
    virtual std::string ToString() const override { return "While"; }

    virtual void ToWASM(WASMContext& ctx) const override 
    {
        assert(children.size() == 2);
        ctx.StartBlock();
        ctx.StartLoop();
        
        children.at(0)->ToWASM(ctx);
        ctx.AddLine("(if");
        ctx.indent++;

        ctx.AddLine("(then)");
        ctx.AddLine("(else");
        ctx.indent++;
        ctx.AddLine("(br $block_" + std::to_string(ctx.block_stack.back()) + ")");
        ctx.indent--;
        ctx.AddLine(")");

        ctx.indent--;
        ctx.AddLine(")");
     
        children.at(1)->ToWASM(ctx);

        ctx.AddLine("(br $loop_" + std::to_string(ctx.loop_stack.back()) + ")");

        ctx.EndLoop();
        ctx.EndBlock();
    }
};

class BreakNode : public ASTNode {
public:
    BreakNode(size_t line_num) : ASTNode(line_num) {}
    virtual std::string ToString() const override { return "Break"; }
    virtual void ToWASM(WASMContext& ctx) const override 
    {
        ctx.AddLine("(br $block_" + std::to_string(ctx.block_stack.back()) + ")");
    }
};

class ContinueNode : public ASTNode {
public:
    ContinueNode(size_t line_num) : ASTNode(line_num) {}
    virtual std::string ToString() const override { return "Continue"; }
    virtual void ToWASM(WASMContext& ctx) const override 
    {
        ctx.AddLine("(br $loop_" + std::to_string(ctx.loop_stack.back()) + ")");
    }
};

class ReturnNode : public BlockNode {
public:
    ReturnNode(size_t line_num, NodePtr&& expr) : BlockNode(line_num, expr) {}
    virtual std::string ToString() const override { return "Return"; }
    virtual Type HasReturn(SymbolTable& st) const override
    {
        return children.at(0)->GetType(st);
    }

    virtual void ToWASM(WASMContext& ctx) const override
    {
        assert(children.size() == 1);
        children.at(0)->ToWASM(ctx);
        ctx.AddLine("(br $" + ctx.current_function + "_exit)");
    }
};

class LiteralNode : public ASTNode {
protected: 
    Type t;
    TypeVal lit;
public:
    LiteralNode(size_t line_num, Type t, TypeVal value) : ASTNode(line_num), lit(value), t(t) {}
    virtual std::string ToString() const override { return "Literal"; }
    Type GetType(SymbolTable& st) const override { return t; }
    virtual void ToWASM(WASMContext& ctx) const override
    {
        ctx.AddLine("(" + Types::ToWASM(t) + ".const " + Types::ValueOnStack(t, lit) + ")");
    }
};

class CastNode : public BlockNode {
protected:
    Type t;
public: 
    CastNode(size_t line_num, NodePtr&& term, Type type) : BlockNode(line_num, term), t(type) {}
    Type GetType(SymbolTable& st) const override { return t; }
    virtual std::string ToString() const override { return "Cast (" + std::to_string(int(t)) + ")"; }
    void CastNumber(WASMContext& ctx) const
    {
        Type input_type = children.at(0)->GetType(ctx.st);

        children.at(0)->ToWASM(ctx);

        switch(t)
        {
            case Type::CHAR:
            case Type::INT:
                if (input_type == Type::DOUBLE)
                {
                    ctx.AddLine("(i32.trunc_f64_s)");
                }
                break;
            case Type::DOUBLE:
                if (input_type != Type::DOUBLE)
                {
                    ctx.AddLine("(f64.convert_i32_s)");
                }
                break;
        }
        
    }
    virtual void ToWASM(WASMContext& ctx) const override 
    {
        assert(children.size() == 1);
        Type input_type = children.at(0)->GetType(ctx.st);

        if(Types::IsNumerical(input_type) && Types::IsNumerical(t))
        {
            return CastNumber(ctx);
        }
        else if(input_type == Type::CHAR && t == Type::STRING)
        {
            ctx.AddLine("(i32.const 1)");
            ctx.AddLine("(call $_alloc_str)");
            ctx.AddLine("(call $_i32_dup)");
            children.at(0)->ToWASM(ctx);
            ctx.AddLine("(i32.store8)");
        }
    }
};

class IndexNode : public BlockNode 
{
public:
    IndexNode(size_t line_num, NodePtr&& target, NodePtr&& index) : BlockNode(line_num, target, index) {}
    Type GetType(SymbolTable& st) const override
    {
        return Types::IndexType(children.at(0)->GetType(st));
    }
    void ComputeAddress(WASMContext& ctx) const
    {
        children.at(0)->ToWASM(ctx);
        children.at(1)->ToWASM(ctx);
        // Add the base address plus the index to the get the address of the indexed value
        ctx.AddLine("(i32.add)");
    }
    virtual void ToWASM(WASMContext& ctx) const override
    {
        ComputeAddress(ctx);
        ctx.AddLine("(i32.load8_u)");
    }
    virtual bool CanAssign(SymbolTable& st, Type rhs)
    {
        return Types::CanAssign(GetType(st), rhs);
    }
    virtual void Assign(WASMContext& ctx, ASTNode* node) override
    {
        bool outer_needs_result = ctx.needs_result;
        ctx.needs_result = true;
        ComputeAddress(ctx);
        node->ToWASM(ctx);
        ctx.needs_result = outer_needs_result;

        ctx.AddLine("(i32.store8)");
        if(outer_needs_result)
        {
            ToWASM(ctx);
        }
    }
};

void CastPair(SymbolTable& st, std::unique_ptr<ASTNode>& lhs, std::unique_ptr<ASTNode>& rhs)
{
    Type higher_precision = std::max(lhs->GetType(st), rhs->GetType(st));
    if(lhs->GetType(st) != higher_precision)
    {
        lhs = std::make_unique<CastNode>(lhs->GetLine(), std::move(lhs), higher_precision);
    }
    else if(rhs->GetType(st) != higher_precision)
    {
        rhs = std::make_unique<CastNode>(rhs->GetLine(), std::move(rhs), higher_precision);
    }
}

void PromoteTo(SymbolTable& st, std::unique_ptr<ASTNode>& term, Type t)
{
    if(term->GetType(st) > t) { assert(false); }

    term = std::make_unique<CastNode>(term->GetLine(), std::move(term), t);
}
