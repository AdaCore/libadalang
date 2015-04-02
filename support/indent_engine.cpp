#include "indent_engine.hpp"


void IndentEngine::process()
{
    root_->compute_indent_level();
    ASTNode::Visitor<IndentEngine*> visitor = visit_node;
    root_->visit_all_children(visitor, this);
}

int IndentEngine::get_indent(unsigned line) const
{
    if (line < 1 || line > lines_indent_.size())
        return 0;
    else
        return lines_indent_[line - 1];
}

ASTNode::VisitStatus IndentEngine::visit_node(ASTNode* node,
                                              IndentEngine* engine) {
    engine->process_node(node);
    return ASTNode::VisitStatus::Into;
}

void IndentEngine::process_node(ASTNode* node) {
    int nstart = node->get_sloc_range().get_start().line;

    /* If we skipped lines since the last ASTnode we processed, compute their
       indentation level.  */
    for (int i = last_line() + 1; i < nstart; i++) {
        const std::string& line = (*lines_)[i - 1];

        /* Lookup the token that covers the first non-blank character on
           this line.  In order to do this, what we do is first determining the
           index of the first non-blank character and then we lookup an AST
           node with the sloc we get.

           TODO: This is sub-optimal:
           1. The snap zone should probably end 1 char before to be practical.
           2. Rather than finding the first token manually this way, we should
              iterate on the list of tokens here, to avoid doing some more
              processing.  */
        int start_col = line.find_first_not_of(" ");

        /* If the line is blank, we don't want to indent the line.  */
        if (start_col == -1) {
            lines_indent_.push_back(0);
            continue;
        }

        /* Add 1 to the column number for 1-based indexing, and add 1 more to
           be past the end of the snap zone.  */
        auto lookup_pos = SourceLocation(i, start_col + 2);
        ASTNode* lookup_node = root_->lookup(lookup_pos, /*snap=*/true);

#if DEBUG_MODE
        if (lookup_node != nullptr) {
            cout << "LOOKUPED NODE FOR LINE " << i << " : "
                << lookup_node->kind_name()
                << "[" << lookup_node->get_sloc_range(true).repr() << "]"
                << " lookup_pos " << lookup_pos.repr()
                << " LINE " << line << endl;
        }
#endif
        lines_indent_.push_back(lookup_node != nullptr
                                ? lookup_node->indent_level : 0);
    }

    /* Now take care of the first line for this ASTNode if no one already
       did.  */
    if (last_line() < nstart) {
#if DEBUG_MODE
        cout << "CURRENT NODE FOR LINE " << nstart << " : "
            << node->kind_name()
            << "[" << node->get_sloc_range().repr() << "]"
            << endl;
#endif

        lines_indent_.push_back(node->indent_level);
    }
}
