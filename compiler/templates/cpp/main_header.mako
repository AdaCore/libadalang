#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include "ast.hpp"
#include "lexer.hpp"
#include "packrat.hpp"

using namespace std;
extern long current_pos;

/*----------------------
-- Types declarations --
----------------------*/

% for el in map(unicode.strip, _self.types_declarations):
${el}
% endfor

/*-------------------------
-- Function declarations --
-------------------------*/

class Parser {
public:
    Parser(const char* string, const size_t len);
    Parser(const std::string file_name);
    ASTNode* parse();
    virtual ~Parser();

    % for el in map(unicode.strip, _self.fns_decls):
    ${el};
    % endfor

private:
    Lexer* lexer;

};

/*---------------------------
-- Value Types definitions --
---------------------------*/

% for el in map(unicode.strip, _self.val_types_definitions):
${el}

% endfor

/*---------------------
-- Types definitions --
---------------------*/

% for el in map(unicode.strip, _self.types_definitions):
${el}

% endfor

void print_diagnostics();
void clean_all_memos();

class AnalysisUnit {
public:
    AnalysisUnit(const std::string file_name);
    virtual ~AnalysisUnit();

    void print();
    void print_json();

    ASTNode*    ast_root;
    Parser*     parser;
    std::string file_name;
};

class AnalysisContext {
public:
    AnalysisUnit* create_from_file(const std::string file_name);
    void remove(const std::string file_name);
    virtual ~AnalysisContext();
private:
    std::unordered_map<std::string, AnalysisUnit*> units_map;
};

#endif
