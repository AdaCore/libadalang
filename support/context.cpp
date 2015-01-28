#include "context.hpp"
#include "lexer.hpp"

AnalysisUnit::AnalysisUnit(std::string file_name, ParseFunction parse_function) {
    this->file_name = file_name;
    this->parse_function = parse_function;
    this->lexer = make_lexer_from_file(file_name.c_str(), nullptr);
    this->ASTRoot = this->parse_function(this->lexer, 0);
}

void AnalysisUnit::print() {
    this->ASTRoot->print_node();
}
