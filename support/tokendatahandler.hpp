#ifndef TOKENDATAHANDLER_HPP
#define TOKENDATAHANDLER_HPP

#include <list>
#include <unordered_map>
#include <cstring>

#include "token.hpp"
#include <symboltable.hpp>

class TokenDataHandler {
public:
    const char* add_string(const char* text, size_t len);
    ~TokenDataHandler();
    TokenDataHandler(SymbolTable* symbol_table);

    std::vector<Token> tokens;
    SymbolTable* symbol_table;

private:
    std::list<char*> str_literals;
};

#endif
