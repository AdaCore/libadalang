## vim: filetype=makocpp

#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include "ast.hpp"
#include "lexer.hpp"
#include "packrat.hpp"
#include "tokendatahandler.hpp"
#include "diagnostic.hpp"

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

    struct FailInfo {
        long pos = -1;
        int expected_token_id, found_token_id;
    } last_fail;

    % for el in map(unicode.strip, _self.fns_decls):
    ${el};
    % endfor

    vector<Diagnostic> diagnostics;

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
    virtual ~AnalysisContext();

    /* Create a new AnalysisUnit for "file_name", register it and return a new
       reference to it: the caller must decrease the ref count once done with
       it.  */
    AnalysisUnit* create_from_file(const std::string file_name);

    /* Remove the corresponding AnalysisUnit from this context.  If someone
       still owns a reference to it, it remains available but becomes
       context-less.  */
    void remove(const std::string file_name);

    /* Get a C API value wrapping this unit.  */
    ${analysis_context.tagged} wrap() {
        return static_cast<${analysis_context.tagged}>(this);
    }

    /* This map owns a reference for each AnalysisUnit in it.  Note that users
       may reference it too.  Once we remove this reference (on AnalysisContext
       destruction or on unit removing), we must reset its "context" attribute
       to avoid dangling pointers.  */
    std::unordered_map<std::string, AnalysisUnit*> units_map;
    SymbolTable* symbol_table;
};

class AnalysisUnit {
public:
    AnalysisUnit(AnalysisContext* context, const std::string file_name);
    virtual ~AnalysisUnit();

    void inc_ref() { ++ref_count; }
    void dec_ref() {
        if (--ref_count == 0)
            delete this;
    }

    void print();
    void print_json();

    /* Get a C API value wrapping this context.  */
    ${analysis_unit.tagged} wrap() {
        return static_cast<${analysis_unit.tagged}>(this);
    }

    /* A back-link to the embedding context.  Beware that even though all units
       are created within a context, this back-link may become a null pointer
       if the context is destroyed while someone still owns a reference to this
       unit.  */
    AnalysisContext* context;

    ASTNode*    ast_root;
    Parser*     parser;
    std::string file_name;
    TokenDataHandler* token_data_handler;
    vector<Diagnostic> diagnostics;

private:
    unsigned ref_count;
};

#endif
