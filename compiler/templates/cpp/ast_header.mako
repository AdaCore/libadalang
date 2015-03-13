## vim: filetype=cpp

#ifndef AST_H
#define AST_H

#include <string>
#include <utility>
#include <vector>
#include <iostream>
#include <boost/property_tree/ptree.hpp>
#include "lexer.hpp"
#include "${capi.lib_name}.h"

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

    /* Return the bottom-most AST node from this one that contains SLOC, or
       nullptr if there is none.   */
    ASTNode *lookup(const SourceLocation &sloc)
    {
        return lookup_relative(sloc).second;
    }

    /* Implementation helper for the other lookup method.  First compare SLOC
       with this node's sloc range.  If SLOC is inside, return (IN, <node>)
       where <node> is the bottom-most AST node that contains SLOC.  Return
       (<pos>, nullptr) otherwise, where <pos> is SLOC's position with respect
       to this nodes' sloc range.  */
    std::pair<RelativePosition, ASTNode *>
    lookup_relative(const SourceLocation &sloc)
    {
        const RelativePosition result = sloc_range_.compare(sloc);

        /* Parents' sloc range always contain their childrens'.  Thus if SLOC
           is outside the sloc range that covers this node, then no node under
           this will contain it.  */
        if (result != IN)
            return std::make_pair(result, nullptr);

        /* Past this point, we *know* that SLOC is inside this node's sloc
           range (result == IN).  */
        return std::make_pair(IN, lookup_children(sloc));
    }

    RelativePosition compare(const SourceLocation &sloc) const {
        return sloc_range_.compare(sloc);
    }

    SourceLocationRange sloc_range_;

    /* Get a C API value wrapping this node.  */
    ${capi.node_type.tagged_name} wrap() {
        return static_cast<${capi.node_type.tagged_name}>(this);
    }

public:
    /* Implementation helper for the previous lookup method.  Assumes that SLOC
       is included in this node's sloc range.  Behaves just like lookup
       otherwise.  */
    virtual ASTNode *lookup_children(const SourceLocation &sloc) = 0;

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

    virtual std::vector<ASTNode*> get_children() = 0;

    /* Get a value that identifies the kind of this node.  Each concrete
       subclass must override this to provide the appropriate value.  */
    virtual ${capi.node_kind_type.tagged_name} kind() = 0;

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
    virtual ASTNode *lookup_children(const SourceLocation &sloc);

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
};

template <typename T> ASTNode *
ASTList<T>::lookup_children(const SourceLocation &sloc)
{
    for (T child : vec) {
        auto sub_lookup = child->lookup_relative(sloc);
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
