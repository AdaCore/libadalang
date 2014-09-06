#ifndef AST_H
#define AST_H

#include <string>
#include <utility>
#include <vector>
#include <iostream>
#include "lexer.hpp"

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

public:
    /* Implementation helper for the previous lookup method.  Assumes that SLOC
       is included in this node's sloc range.  Behaves just like lookup
       otherwise.  */
    virtual ASTNode *lookup_children(const SourceLocation &sloc) = 0;
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

#endif
