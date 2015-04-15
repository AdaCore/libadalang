## vim: filetype=makocpp

#include "parse.hpp"
#include <unordered_map>
#include "tokendatahandler.hpp"

using boost::property_tree::ptree;

std::unordered_map<int, std::string> token_texts = {
    % for tok_name, _ in token_map.tokens.items():
        {${token_map.TOKEN_PREFIX + tok_name}, "${token_map.names_to_str.get(tok_name, tok_name.lower())}"}
        % if (not loop.last):
            ,
        % endif
    % endfor
};


template< typename T, class Allocator > void shrink_capacity(std::vector<T,Allocator>* v)
{
   std::vector<T,Allocator>(v->begin(),v->end()).swap(*v);
}

long current_pos;
std::string indent_str("");

/*-------------------------
-- Functions definitions --
-------------------------*/

% for el in map(unicode.strip, _self.body):
${el}

% endfor

void print_diagnostics() {
% for t in _self.diag_types:
    // printf("Number of instantiations for ${t.name()}: %ld\n", ${t.name().lower}_counter);
% endfor
}

void clean_all_memos() {
    % for fn in _self.fns:
#if DEBUG_MODE
        printf("CLEANING MEMO FOR ${fn}\n");
        fflush(stdout);
#endif
        ${fn}_memo.clear();
    % endfor
}

Parser::Parser(const char* string, const size_t len, TokenDataHandler* token_data) {
    this->lexer = new Lexer(string, len, token_data);
    this->token_data = token_data;
}

Parser::Parser(const std::string file_name, TokenDataHandler* token_data) {
    this->lexer = new Lexer(file_name.c_str(), nullptr, token_data);
    this->token_data = token_data;
}

Parser::~Parser() {
     delete this->lexer;
}

ASTNode* Parser::parse() {
    auto res = this->${_self.rules_to_fn_names[_self.main_rule_name].gen_fn_name} (0);

    ## If the result is null, we want to generate a sane diagnostic, informing
    ## the user of where parsing failed, what token was expected, and what token
    ## was found

    if (!res) {
        auto diag = Diagnostic(
            "Expected \"" + token_texts[last_fail.expected_token_id]
            + "\", got \"" + token_texts[last_fail.found_token_id] + "\"",
            lexer->get(last_fail.pos).sloc_range
        );
        this->diagnostics.push_back(diag);
    } else {
        res->inc_ref();
    }
    clean_all_memos();
    return res;
}

AnalysisUnit::AnalysisUnit(AnalysisContext *context,
                           const std::string file_name)
  : ref_count(1) {
    this->file_name = file_name;
    this->context = context;
    this->token_data_handler = new TokenDataHandler(context->symbol_table);
    this->parser = new Parser(file_name, this->token_data_handler);
    this->ast_root = this->parser->parse();
    this->diagnostics.insert(
        diagnostics.end(),
        parser->diagnostics.begin(), parser->diagnostics.end()
    );
    delete this->parser;
}

AnalysisUnit::~AnalysisUnit() {
    if (this->ast_root) {
        this->ast_root->dec_ref();
    }
    delete this->token_data_handler;
}

void AnalysisUnit::print() {
    if (this->ast_root)
        this->ast_root->print_node();
    else
        printf("<empty analysis unit>\n");
}

void AnalysisUnit::print_json() {
    assert(this->ast_root);
    write_json(std::cout, this->ast_root->get_property_tree());
}

AnalysisContext::AnalysisContext() {
    this->symbol_table = new SymbolTable;
}

AnalysisContext::~AnalysisContext() {
    for (auto pair : units_map) {
        pair.second->context = nullptr;
        pair.second->dec_ref();
    }
    delete this->symbol_table;
}

AnalysisUnit* AnalysisContext::create_from_file(std::string file_name) {
    AnalysisUnit* aunit = new AnalysisUnit(this, file_name);
    /* AnalysisUnit automatically have one reference upon creation.
       "units_map" is supposed to own this reference, so there's no need to
       increase the ref-count.  */
    units_map[file_name] = aunit;
    return aunit;
}

void AnalysisContext::remove(std::string file_name) {
    /* We remove the corresponding analysis unit from this context but users
       could keep references on it, so make sure it can live independently.  */
    AnalysisUnit *unit = units_map[file_name];
    unit->context = nullptr;
    unit->dec_ref();

    units_map.erase(file_name);
}
