## vim: filetype=makocpp

#ifndef AST_H
#define AST_H

#include <string>
#include <utility>
#include <vector>
#include <iostream>
#include <boost/property_tree/ptree.hpp>
#include <functional>
#include "lexer.hpp"
#include "${capi.lib_name}.h"

enum class IndentType { None, Anchor, Relative };

struct IndentProperties {
    IndentType indent_type;
    union {
        short relative_level;
        Token* anchor;
    };
};

class ASTNode {
public:
    virtual ~ASTNode() {}
    int ref = 0;

    virtual std::string repr() { return "not impl"; }
    virtual std::string __name() { return "ASTNode"; }

    void inc_ref() { ref++; }
    int dec_ref() {
        ref--;
        if (ref <= 0) {
            delete this;
            return true;
        }
        return false;
    }

    short indent_level = 0;

    virtual void compute_indent_level() = 0;

    /* Return the bottom-most AST node from this one that contains SLOC, or
       nullptr if there is none.   */
    ASTNode *lookup(const SourceLocation &sloc, bool snap=false)
    {
        return lookup_relative(sloc, snap).second;
    }

    /* Implementation helper for the other lookup method.  First compare SLOC
       with this node's sloc range.  If SLOC is inside, return (IN, <node>)
       where <node> is the bottom-most AST node that contains SLOC.  Return
       (<pos>, nullptr) otherwise, where <pos> is SLOC's position with respect
       to this nodes' sloc range.  */
    std::pair<RelativePosition, ASTNode *>
    lookup_relative(const SourceLocation &sloc, bool snap=false)
    {
        const RelativePosition result = get_sloc_range(snap).compare(sloc);

        /* Parents' sloc range always contain their childrens'.  Thus if SLOC
           is outside the sloc range that covers this node, then no node under
           this will contain it.  */
        if (result != IN)
            return std::make_pair(result, nullptr);

        /* Past this point, we *know* that SLOC is inside this node's sloc
           range (result == IN).  */
        return std::make_pair(IN, lookup_children(sloc, snap));
    }

    RelativePosition compare(const SourceLocation &sloc, bool snap=false) const {
        return get_sloc_range(snap).compare(sloc);
    }

    TokenDataHandler* token_data;
    TokenId token_start, token_end;

    SourceLocationRange get_sloc_range(bool snap=false) const {
        SourceLocation sloc_start, sloc_end;
        if (snap) {
            sloc_start = token_data->tokens[std::max<long>(token_start - 1, 0)].sloc_range.get_end();
            sloc_end = token_data->tokens[std::min<long>(token_end + 1, token_data->tokens.size() - 1)].sloc_range.get_start();
        } else {
            sloc_start = token_data->tokens[token_start].sloc_range.get_start();
            sloc_end = token_data->tokens[token_end].sloc_range.get_end();
        }
        return SourceLocationRange(sloc_start, sloc_end);
    }

    /* Get a C API value wrapping this node.  */
    ${capi.node_type.tagged_name} wrap() {
        return static_cast<${capi.node_type.tagged_name}>(this);
    }

public:
    /* Implementation helper for the previous lookup method.  Assumes that SLOC
       is included in this node's sloc range.  Behaves just like lookup
       otherwise.  */
    virtual ASTNode *lookup_children(const SourceLocation &sloc, bool snap=false) = 0;

    ASTNode* _parent;

    ASTNode* parent() {
       return this->_parent;
    }

    void setParent (ASTNode *parent) {
       this->_parent = parent;
    }

    /* Method invoked in the testing phase which provides support to verify
       the decoration of the AST nodes. Currently it is overriden in the
       derived classes to check the decoration of the _parent attribute
       but it is a hook for adding further checks */
    virtual void validate() = 0;

    /* (debugging) Tree output */
    virtual void print_node(int level = 0) = 0;

    virtual boost::property_tree::ptree get_property_tree() = 0;

    /* Subsidiary routine of print_node overriden in ASTList; used to avoid
       the need of performing a dynamic cast to identify tree components
       containing a non-empty lists of nodes */
    virtual bool is_empty_list() {
       return false;
    }


    /* Get a value that identifies the kind of this node.  Each concrete
       subclass must override this to provide the appropriate value.  */
    virtual ${capi.node_kind_type.tagged_name} kind() = 0;

    virtual std::vector<ASTNode*> get_children() = 0;

    enum class VisitStatus { Into, Over, Stop };

    template <typename VisitContext>
        using Visitor = std::function<VisitStatus (ASTNode*, VisitContext)>;

    template <typename VisitContext>
    void visit_all_children (
        Visitor<VisitContext> visitor,
        VisitContext context
    ) {
        for (auto child : this->get_children()) {

            if (!child)
                continue;

            auto status = visitor(child, context);

            if (status == VisitStatus::Into)
                child->visit_all_children(visitor, context);
            else if (status == VisitStatus::Stop)
                return;

        }
    }

protected:
    /* Subsidiary routine of print_node used to visualize the deep level of
       the tree components in the left margin of the output */
    void print_tab(int level) {
       for (int j=0; j<level; j++) {
          printf("| ");
       }
    }
};

template <typename T> class ASTList : public ASTNode {
protected:
    virtual ASTNode *lookup_children(const SourceLocation &sloc, bool snap=false);

public:
    std::vector<T> vec;
    std::string repr() { return get_repr(vec); };
    ~ASTList() {
        #if DEBUG_MODE
        printf("DELETING VECTOR\n");
        #endif
        vec_dec_ref (vec);
    }

    void validate();
    void print_node(int level = 0);
    boost::property_tree::ptree get_property_tree();
    virtual bool is_empty_list();

    virtual ${capi.node_kind_type.tagged_name} kind() {
        return ${capi.get_name("list")};
    }

    virtual std::vector<ASTNode*> get_children() {
        std::vector<ASTNode*> result;
        for (auto node : vec)
            result.push_back(node);
        return result;
    }

    virtual std::string __name() { return "ASTList"; }
    void compute_indent_level() {
        for (auto el : vec) {
            el->indent_level = this->indent_level;
            el->compute_indent_level();
        }
    }
};

template <typename T> ASTNode *
ASTList<T>::lookup_children(const SourceLocation &sloc, bool snap)
{
    for (T child : vec) {
        auto sub_lookup = child->lookup_relative(sloc, snap);
        switch (sub_lookup.first) {
            case BEFORE:
                return this;

            case IN:
                return sub_lookup.second;

            case AFTER:
                break;
        }
    }
    return this;
}

template <typename T> inline void vec_free (std::vector<T>& vec) { for (auto el : vec) el.free(); }
template <typename T> inline void vec_free (std::vector<T*>& vec) { for (auto el : vec) el->free(); }

template <typename T> inline std::string get_repr (std::vector<T>& vec) {
    std::string res;
    for (auto el : vec) {
        if (res != "") res.append(", ");
        res.append(el.repr());
    }
    res.insert(0, "[");
    res.append("]");
    return res;
}

template <typename T> inline std::string get_repr (std::vector<T*>& vec) {
    std::string res;
    for (auto el : vec) {
        if (res != "") res.append(", ");
        res.append(el->repr());
    }
    res.insert(0, "[");
    res.append("]");
    return res;
}

template <typename T> inline std::string get_repr (T node) { return node.repr(); }
template <typename T> inline std::string get_repr (T* node) { return node ? node->repr() : "None"; }

template <typename T> inline void vec_dec_ref (std::vector<T*>& vec) { for (auto el : vec) el->dec_ref(); }
template <typename T> inline void vec_dec_ref (std::vector<T>& vec) { for (auto el : vec) el.dec_ref(); }

inline std::string get_repr (int el) { return el ? "True" : "False"; }
boost::property_tree::ptree get_ptree (int el);

template <typename T> void dec_ref (T& el) {
    el.dec_ref();
}

template <typename T> void dec_ref (T*& el) {
    if (el) {
        el->dec_ref();
        el = nullptr;
    }
}

inline std::string get_repr (Token node) { return std::string(node.text); }
boost::property_tree::ptree get_ptree (Token node);

template <typename T> void
ASTList<T>::validate() {
   for (typename std::vector<T>::iterator it = vec.begin();
        it != vec.end(); ++it)
   {
      ASTNode *node = (ASTNode*) *it;
      if (node) {
         assert(node->parent() == this && "wrong parent in list component");
         node->validate();
      }
   }
}

template <typename T> void
ASTList<T>::print_node(int level) {
   if (vec.empty())
      return;

   for (auto node : vec) {
      if (node) {
         node->print_node(level);
      }
   }
}

template <typename T> bool
ASTList<T>::is_empty_list() {
   return vec.empty();
}

typedef ASTNode*(*ParseFunction)(Lexer*, long);

template <typename T> boost::property_tree::ptree ASTList<T>::get_property_tree() {
    boost::property_tree::ptree result;
    for (auto node : vec) {
        if (node) result.push_back(std::make_pair("", node->get_property_tree()));
    }
    return result;
}

#endif
