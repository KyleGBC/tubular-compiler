#pragma once

#include <assert.h>
#include <string>
#include <unordered_map>
#include <vector>
#include <variant>
#include "Types.hpp"
#include "helpers.hpp"

class SymbolTable {
public:
    class VarData
    {
    public:
        Type t;
        TypeVal value;
        bool initialized;
    };
    class FnData
    {
    public:
        std::string name;
        std::vector<size_t> params;
        Type return_type;
        std::vector<size_t> locals;
        FnData(std::string name, Type return_type) : name(name), return_type(return_type) {};
    };
    using Symbol = std::variant<VarData, FnData>;
    static constexpr size_t NO_ID = -1;
private:
    std::vector<Symbol> symbols;
    size_t current_function = NO_ID;
    std::vector<std::unordered_map<std::string, size_t>> scopes;
public:

    // Initiate the global scope in constructor
    SymbolTable() { scopes.emplace_back(); };

    void PushScope()
    { 
        scopes.emplace_back();
    }
    void PopScope()
    {
        scopes.pop_back();
    }

    size_t GetId(std::string name)
    {
        for(auto it = scopes.rbegin(); it != scopes.rend(); ++it)
        {
            if(it->find(name) != it->end())
            {
                return it->at(name);
            }
        }
        return NO_ID;
    }

    size_t AddVar(Type type, std::string name, bool as_param = false)
    { 
        if(scopes.back().find(name) != scopes.back().end())
        {
            return NO_ID;
        }

        TypeVal placeholder;
        switch(type)
        {
            case Type::CHAR:
                placeholder = char(0);
                break;
            case Type::INT:
                placeholder = 0;
                break;
            case Type::DOUBLE:
                placeholder = double(0);
                break;
            case Type::NONE:
                assert(false);
        }

        symbols.push_back( VarData{type, placeholder, false} );
        scopes.back()[name] = symbols.size() - 1;
        
        if (current_function != NO_ID && !as_param)
        {
            GetFunction(current_function).locals.push_back(symbols.size() - 1);
        }
        
        return symbols.size() - 1;
    }

    size_t AddFunction(std::string name, Type return_type)
    {
        std::unordered_map<std::string, size_t>& entry_scope = scopes.at(scopes.size() - 1);
        if(entry_scope.find(name) != entry_scope.end())
        {
            return NO_ID;
        }

        symbols.push_back( FnData(name, return_type) );
        entry_scope[name] = symbols.size() - 1;
        current_function = symbols.size() - 1;
        return symbols.size() - 1;
    }

    size_t AddIntrinsicFunction(CallingInfo c)
    {
        auto fn_id = AddFunction(c.name, c.return_type);

        int param_count = 0;
        PushScope();
        for(Type t : c.params)
        {
            size_t p = AddVar(t, std::to_string(param_count), true);
            GetFunction(fn_id).params.push_back(p);
            param_count++;
        }
        PopScope();

        return fn_id;
    }

    VarData& GetVar(size_t id)
    {
        assert(id < symbols.size());
        auto& data = symbols.at(id);
        assert(std::holds_alternative<VarData>(data));
        return std::get<VarData>(data);
    }

    FnData& GetFunction(size_t id)
    {
        assert(id < symbols.size());
        auto& data = symbols.at(id);
        assert(std::holds_alternative<FnData>(data));
        return std::get<FnData>(data);
    }

    FnData& GetCurrentFunction()
    {
        return GetFunction(current_function);
    }
};