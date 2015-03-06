#include "token.hpp"

/******************
 * SourceLocation *
 *****************/

using boost::property_tree::ptree;

boost::property_tree::ptree SourceLocation::get_property_tree() {
    ptree result;
    result.put("", this->repr());
    return result;
}

boost::property_tree::ptree SourceLocationRange::get_property_tree() {
    ptree result;
    result.put("", this->repr());
    return result;
}

RelativePosition SourceLocationRange::compare(const SourceLocation &sloc) const {
    assert(not sloc.is_null() && not is_null());
    switch (get_start().compare(sloc)) {
        case BEFORE:
            return BEFORE;
        case IN:
            return IN;
        case AFTER:
            if (get_end().compare(sloc) == AFTER)
                return AFTER;
            else
                return IN;
    }
}

RelativePosition SourceLocation::compare(const SourceLocation &sloc) const {
    assert (!is_null() && !sloc.is_null());
    if (sloc.line < line)
        return BEFORE;
    else if (line < sloc.line)
        return AFTER;
    /* In the following cases, both slocs are on the same line.  */
    else if (sloc.column < column)
        return BEFORE;
    else if (column < sloc.column)
        return AFTER;
    else
        return IN;
}

