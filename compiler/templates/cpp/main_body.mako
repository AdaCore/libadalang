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

AnalysisUnit::AnalysisUnit(std::string file_name) {
    this->file_name = file_name;
    this->lexer = make_lexer_from_file(file_name.c_str(), nullptr);
    this->parser = new Parser();
    this->ast_root = parser->${_self.rules_to_fn_names[_self.main_rule_name].gen_fn_name}(this->lexer, 0);
}

AnalysisUnit::~AnalysisUnit() {
    delete ast_root;
    free(this->parser);
    free_lexer(this->lexer);
}

void AnalysisUnit::print() {
    this->ast_root->print_node();
}

void AnalysisUnit::print_json() {
    write_json(std::cout, ast_root->get_property_tree());
}

AnalysisContext::~AnalysisContext() {
    for (auto kv : units_map) delete kv.second;
}

AnalysisUnit* AnalysisContext::create_from_file(std::string file_name) {
    AnalysisUnit* el = new AnalysisUnit(file_name);
    this->units_map[file_name] = el;
    return el;
}

void AnalysisContext::remove(std::string file_name) {
    delete this->units_map[file_name];
    this->units_map.erase(file_name);
}
