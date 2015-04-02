#ifndef INDENT_ENGINE_HPP
#define INDENT_ENGINE_HPP

#include <string>
#include <vector>

#include "ast.hpp"


class IndentEngine {
public:
    IndentEngine(ASTNode* root,
                 std::vector<std::string>* lines)
      : root_(root), lines_(lines) {
    }

    /* Compute indentation levels for all lines.  */
    void process();

    /* Get the indentation level for one line.  */
    int get_indent(unsigned line) const;

private:
    /* Root ASTNode for the tree to indent.  Used for sloc-based node
       lookups. */
    ASTNode* root_;

    /* Input source lines.  */
    std::vector<std::string>* lines_;

    /* For each processed line, computed indentation level for it.  */
    std::vector<short> lines_indent_;
};

#endif
