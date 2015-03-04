## vim: filetype=cpp

#include "${header_name}"
#include <unordered_map>

using boost::property_tree::ptree;

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
    // printf("Number of instantiations for ${t.name()}: %ld\n", ${t.name().lower()}_counter);
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

Parser::Parser(const char* string, const size_t len) {
    this->lexer = make_lexer_from_string(string, len);
}

Parser::Parser(const std::string file_name) {
    this->lexer = make_lexer_from_file(file_name.c_str(), nullptr);
}

Parser::~Parser() {
     free_lexer(this->lexer);
}

ASTNode* Parser::parse() {
    return this->${_self.rules_to_fn_names[_self.main_rule_name].gen_fn_name} (0);
}

AnalysisUnit::AnalysisUnit(const std::string file_name) {
    this->file_name = file_name;
    this->parser = new Parser(file_name);
    this->ast_root = this->parser->parse();
}

AnalysisUnit::~AnalysisUnit() {
    delete this->ast_root;
    delete this->parser;
}

void AnalysisUnit::print() {
    assert(this->ast_root);
    this->ast_root->print_node();
}

void AnalysisUnit::print_json() {
    assert(this->ast_root);
    write_json(std::cout, this->ast_root->get_property_tree());
}

AnalysisContext::~AnalysisContext() {
    for (auto kv : units_map) delete kv.second;
}

AnalysisUnit* AnalysisContext::create_from_file(std::string file_name) {
    AnalysisUnit* aunit = new AnalysisUnit(file_name);
    this->units_map[file_name] = aunit;
    return aunit;
}

void AnalysisContext::remove(std::string file_name) {
    delete this->units_map[file_name];
    this->units_map.erase(file_name);
}
