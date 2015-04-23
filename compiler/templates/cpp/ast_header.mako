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

/* Data structures for ASTNode extension: see in ASTNode below.  */
class ASTNode;
typedef void ASTNodeExtension;
typedef std::function<void(ASTNode*, ASTNodeExtension*)> ASTNodeExtensionDestructor;

class ASTNode {
public:
    virtual ~ASTNode() {
        /* Detroy all extensions.  */
        for (auto& slot : extensions)
            slot.dtor(this, slot.value);
    }
    int ref = 0;

    /* Get a value that identifies the kind of this node.  Each concrete
       subclass must override this to provide the appropriate value.  */
    virtual ${node_kind_type} kind() = 0;

    /* Get a string that identifies the kind of this node.  Each concrete
       subclass must override this to provide the appropriate value.  */
    virtual std::string kind_name() = 0;

    virtual std::string repr() { return "not impl"; }

    void inc_ref() { ref++; }
    int dec_ref() {
        ref--;
        if (ref <= 0) {
            delete this;
            return true;
        }
        return false;
    }

    /* Get an already registered extension for "ext_id" (see extensions.hpp).
       If there's such an extension, return a pointer to it. If there's not,
       create one, associate the "dtor" destructor to it and return a pointer
       to it (the pointed value is initialized to nullptr in such a case).

       Note that returned pointers are not guaranteed to stay valid after other
       calls to get_extension are made.  */
    ASTNodeExtension** get_extension(unsigned ext_id,
                                     ASTNodeExtensionDestructor dtor) {
        for (auto& slot : extensions) {
            if (slot.extension_id == ext_id)
                return &slot.value;
        }
        ASTNodeExtensionSlot new_slot = {ext_id, nullptr, dtor};
        extensions.push_back(new_slot);
        return &extensions[extensions.size() - 1].value;
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
    ${node_type} wrap() {
        return static_cast<${node_type}>(this);
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

    /* Helpers to ease visitors' implementation  */

    /* Return the number of children for this node.  */
    virtual unsigned get_child_count() const = 0;

    /* If "index" references a valid child (i.e. if it's between 0
       (included) and the number of children (excluded), return true and
       put the Nth child for this node in "result".  Just return false
       otherwise.  */
    virtual bool get_child(unsigned index, ASTNode*& result) = 0;

    /* Return the Nth child for this node or nullptr if it does not
       exist.  Note that this can return nullptr even if the child exist:
       some children can be null.  */
    ASTNode *get_child(unsigned index) {
        ASTNode *result = nullptr;
        get_child(index, result);
        return result;
    }

    enum class VisitStatus { Into, Over, Stop };

    template <typename VisitContext>
        using Visitor = std::function<VisitStatus (ASTNode*, VisitContext)>;

    template <typename VisitContext>
    void visit_all_children (
        Visitor<VisitContext> visitor,
        VisitContext context
    ) {
        const unsigned child_count = get_child_count();
        for (unsigned i = 0; i < child_count; ++i) {
            ASTNode *child;
            if (!get_child(i, child))
                break;
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

private:
    /* Holder for extensions.  */
    struct ASTNodeExtensionSlot {
        unsigned extension_id;
        ASTNodeExtension *value;
        ASTNodeExtensionDestructor dtor;
    };
    std::vector<ASTNodeExtensionSlot> extensions;
};

template <typename T> class ASTList : public ASTNode {
protected:
    virtual ASTNode *lookup_children(const SourceLocation &sloc, bool snap=false);

public:
    std::vector<T> vec;
    std::string repr() {
        std::string res;
        for (auto el : vec) {
            if (res != "") res.append(", ");
            res.append(el->repr());
        }
        res.insert(0, "[");
        res.append("]");
        return res;
    };
    ~ASTList() {
        #if DEBUG_MODE
        printf("DELETING VECTOR\n");
        #endif
         for (auto el : vec) el->dec_ref();
    }

    void validate();
    void print_node(int level = 0);
    boost::property_tree::ptree get_property_tree();
    virtual bool is_empty_list();

    virtual ${node_kind_type} kind() {
        return ${capi.get_name(Name("List"))};
    }

    virtual std::string kind_name() { return "ASTList"; }

    unsigned get_child_count() const {
        return vec.size();
    }
    bool get_child(unsigned index, ASTNode*& result) {
        if (index >= vec.size())
            return false;
        else
        {
            result = vec[index];
            return true;
        }
    }

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

inline std::string get_repr (ASTNode* node) { return node ? node->repr() : "None"; }
inline std::string get_repr (int el) { return el ? "True" : "False"; }

boost::property_tree::ptree get_ptree (int el);

inline void dec_ref (ASTNode* el) {
    if (el)
        el->dec_ref();
}

inline std::string get_repr (Token node) { return std::string(node.text); }
boost::property_tree::ptree get_ptree (Token node);

template <typename T> void ASTList<T>::validate() {
   for (auto node : vec) {
      if (node) {
         assert(node->parent() == this && "wrong parent in list component");
         node->validate();
      }
   }
}

template <typename T> void ASTList<T>::print_node(int level) {
   if (vec.empty())
      return;

   for (auto node : vec) {
      if (node) {
         node->print_node(level);
      }
   }
}

template <typename T> bool ASTList<T>::is_empty_list() {
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
