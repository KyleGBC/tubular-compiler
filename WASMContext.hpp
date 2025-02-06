#include <vector>
#include "SymbolTable.hpp"
#include "helpers.hpp"

struct WASMLine
{
    int indent;
    std::string code;
};

struct WASMContext {
    int indent = 0;
    SymbolTable& st;
    size_t loop_count = 0;
    size_t block_count = 0;
    std::vector<size_t> loop_stack{};
    std::vector<size_t> block_stack{};
    std::string current_function;
    std::vector<WASMLine> program;
    std::vector<std::pair<int, std::string>> statics;
    bool needs_result = false;

    int heap_start = 0;

    WASMContext(SymbolTable& st) : st(st) {}

    int RegisterStaticString(std::string s)
    {
        s = s.substr(1, s.size() - 2);
        int start_address = heap_start;
        statics.push_back(std::make_pair(start_address, s));
        heap_start += s.length() + 1;
        return start_address;
    }
    void AddStatics()
    {
        for(auto& [i, s] : statics)
        {
            AddLine("(data (i32.const " + std::to_string(i) + ") \"" + s + "\\00\")");
        }
        AddLine("(global $free_mem (mut i32) (i32.const " + std::to_string(heap_start) + "))");
    }
    void AddIntrinsics()
    {
        int _indent = indent;
        indent = 0;
        for(auto& i : AVAILABLE_INTRINSICS)
        {
            AddLine(i.wasm);
        }
        indent = _indent;
    }



    void AddLine(std::string line) { program.push_back( WASMLine{indent, line} ); }
    void StartBlock()
    { 
        AddLine("(block $block_" + std::to_string(block_count));
        block_stack.push_back(block_count++);
        indent++;
    }
    void EndBlock()
    { 
        indent--;
        AddLine(")"); 
        block_stack.pop_back();
    }
    void StartLoop()
    { 
        AddLine("(loop $loop_" + std::to_string(loop_count));
        loop_stack.push_back(loop_count++);
        indent++;
    }
    void EndLoop()
    {
        indent--;
        AddLine(")");
        loop_stack.pop_back();
        
    }
};
