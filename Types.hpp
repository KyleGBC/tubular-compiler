#pragma once
#include <variant>
#include <cassert>
#include "lexer.hpp"
#include "ASTNode.hpp"
#include <iostream>

enum class Type { NONE, CHAR, INT, DOUBLE, STRING };
using TypeVal = std::variant<char, int, double>;

namespace Types {
    std::string Display(Type t)
    {
        switch (t)
        {
            case Type::CHAR:
                return "char";
            case Type::INT:
                return "int";
            case Type::DOUBLE:
                return "double";
            case Type::STRING:
                return "string";
            case Type::NONE:
            default:
                return "none";
        }
    }

    Type Parse(emplex::Token tok)
    {
        assert(tok.id == emplex::Lexer::ID_TYPE);

        if (tok.lexeme == "int") return Type::INT;
        if (tok.lexeme == "char") return Type::CHAR;
        if (tok.lexeme == "double") return Type::DOUBLE;
        if (tok.lexeme == "string") return Type::STRING;

        assert(false);
        return Type::NONE;
    }

    bool IsNumerical(Type t)
    {
        switch(t)
        {
            case(Type::CHAR):
            case(Type::INT):
            case(Type::DOUBLE):
                return true;
            case(Type::STRING):
                return false;
            default:
                assert(false);
                return false;
        }
    }

    bool IsTextual(Type t)
    {
        switch(t)
        {
            case(Type::CHAR):
            case(Type::STRING):
                return true;
            case(Type::INT):
            case(Type::DOUBLE):
                return false;
            default:
                assert(false);
                return false;
        }
    }

    std::string ToWASM(Type t)
    {
        switch(t)
        {
            case Type::CHAR:
            case Type::INT:
            case Type::STRING:
                return "i32";
            case Type::DOUBLE:
                return "f64";
            default:
                assert(false);
                return "error-type";
        }
    }

    std::string ValueOnStack(Type t, TypeVal tv)
    {
        switch(t)
        {
            case Type::CHAR:
                return std::to_string(std::get<char>(tv));
            case Type::INT:
            case Type::STRING:
                return std::to_string(std::get<int>(tv));
            case Type::DOUBLE:
                return std::to_string(std::get<double>(tv));

            default:
                assert(false);
                return "error-value";
        }
    }

    bool CanAssign(Type lvalue, Type rvalue)
    {
        if(IsNumerical(lvalue) && IsNumerical(rvalue))
        {
            return int(lvalue) >= int(rvalue);
        }
        else if(lvalue == Type::STRING && IsTextual(rvalue))
        {
            return true;
        }
        return false;
    }

    Type IndexType(Type t)
    {
        switch(t)
        {
            case(Type::STRING):
                return Type::CHAR;
            default:
                return Type::NONE;
        }
    }
};

std::ostream& operator<<(std::ostream& os, const Type& t)
{
    os << Types::Display(t);
    return os;
}