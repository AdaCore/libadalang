#include "ast.hpp"

using boost::property_tree::ptree;

ptree get_ptree (int el) {
    ptree result;
    if (el) result.put("", true);
    else result.put("", false);
    return result;
}

ptree get_ptree (Token node) {
    ptree result;
    result.put("", std::string(node.text));
    return result;
}
