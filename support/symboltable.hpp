#ifndef SYMBOLTABLE_HPP
#define SYMBOLTABLE_HPP

#include <cstring>
#include <unordered_map>

struct EqStr {
    bool operator() (const char* s1, const char* s2) const {
        return strcmp(s1, s2) == 0;
    }
};

struct CharHash {
    uint32_t operator() (const char * const string) const;
};

class SymbolTable {
public:
    const char* get(const char* text, size_t len);
    ~SymbolTable();
private:
    std::unordered_map<char*, char*, CharHash, EqStr> hmap;
};

#endif
