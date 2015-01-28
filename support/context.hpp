#include "ast.hpp"
#include <functional>

class AnalysisUnit {
public:

    void inc_ref() {
        ref++;
    }

    int dec_ref() {
        ref--;
        if (ref <= 0) {
            delete this;
            return true;
        }
        return false;
    }

    virtual ~AnalysisUnit() {
        delete ASTRoot;
    }

    AnalysisUnit(std::string file_name, ParseFunction parse_function);

    void print();
    void print_json();

private:
    int ref = 0;
    std::string file_name;
    ParseFunction parse_function;
    ASTNode* ASTRoot;
    Lexer* lexer;
};
