#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include "ast.hpp"
#include "cffi_interface.h"

using namespace std;
extern long current_pos;

/*----------------------
-- Types declarations --
----------------------*/

template <typename T> class ASTList : ASTNode {
public:
    std::vector<T> vec;

    virtual std::string repr() {
        return vec_repr(this->vec);
    };
};

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

#endif
