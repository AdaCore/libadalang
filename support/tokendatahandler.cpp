#include "tokendatahandler.hpp"

TokenDataHandler::TokenDataHandler(SymbolTable* symbol_table) {
    this->symbol_table = symbol_table;
}

const char* TokenDataHandler::add_string(const char* text, size_t len) {
    char* buffer = new char[len + 1];
    strncpy(buffer, text, len);
    buffer[len] = 0;
    this->str_literals.push_back(buffer);
    return buffer;
}

TokenDataHandler::~TokenDataHandler() {
    for (auto str : this->str_literals)
        delete[] str;
}
