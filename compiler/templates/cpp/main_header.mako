#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include "ast.hpp"
#include "lexer.hpp"
#include "packrat.hpp"
#include "context.hpp"

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

% for el in map(unicode.strip, _self.fns_decls):
${el};

% endfor

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

AnalysisUnit* parse_file(std::string file_name);

#endif
