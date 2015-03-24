## vim: filetype=makocpp

#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include "ast.hpp"
#include "lexer.hpp"
#include "packrat.hpp"
#include "tokendatahandler.hpp"

#include "${_self.c_api_settings.lib_name}.h"

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
    Parser(const char* string, const size_t len, TokenDataHandler* token_data);
    Parser(const std::string file_name, TokenDataHandler* token_data);
    ASTNode* parse();
    virtual ~Parser();

    Token max_token() {
       return lexer->max_token;
    }

    % for el in map(unicode.strip, _self.fns_decls):
    ${el};
    % endfor

private:
    Lexer* lexer;
    TokenDataHandler* token_data;
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

class AnalysisUnit;

class AnalysisContext {
public:
    AnalysisContext();
    AnalysisUnit* create_from_file(const std::string file_name);
    void remove(const std::string file_name);
    virtual ~AnalysisContext();

    /* Get a C API value wrapping this unit.  */
    ${capi.analysis_context_type.tagged_name} wrap() {
        return static_cast<${capi.analysis_context_type.tagged_name}>(this);
    }

    std::unordered_map<std::string, AnalysisUnit*> units_map;
    SymbolTable* symbol_table;
};

class AnalysisUnit {
public:
    AnalysisUnit(AnalysisContext* context, const std::string file_name);
    virtual ~AnalysisUnit();

    void print();
    void print_json();

    /* Get a C API value wrapping this context.  */
    ${capi.analysis_unit_type.tagged_name} wrap() {
        return static_cast<${capi.analysis_unit_type.tagged_name}>(this);
    }

    ASTNode*    ast_root;
    Parser*     parser;
    std::string file_name;
    TokenDataHandler* token_data_handler;
    AnalysisContext* context;
};

#endif
