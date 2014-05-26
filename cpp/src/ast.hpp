#ifndef AST_H
#define AST_H

#include <string>
#include <vector>

class ASTNode {
public:
    virtual std::string repr() { return "not impl"; }
    virtual std::string __name() { return "ASTNode"; }
};

template <typename T> std::string vec_repr (std::vector<T>& vec) {
    std::string res;
    for (auto el : vec) {
        if (res != "") res.append(", ");
        res.append(el.repr());
    }
    res.insert(0, "[");
    res.append("]");
    return res;
}

template <typename T> std::string vec_repr (std::vector<T*>& vec) {
    std::string res;
    for (auto el : vec) {
        if (res != "") res.append(", ");
        res.append(el->repr());
    }
    res.insert(0, "[");
    res.append("]");
    return res;
}

#endif
