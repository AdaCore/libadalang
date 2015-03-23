#ifndef DIAGNOSTIC_HPP
#define DIAGNOSTIC_HPP

#include "token.hpp"

class Diagnostic {
public:
    Diagnostic(std::string raw_message, SourceLocationRange sloc_range) {
        this->raw_message = raw_message;
        this->sloc_range = sloc_range;
    }

    std::string get_pretty_message() {
        std::ostringstream ss;
        ss << "Line " << sloc_range.get_start().line <<
              ", column " << sloc_range.get_start().column <<
              ": " << get_raw_message();
        return ss.str();
    }


    std::string get_raw_message() {
        return this->raw_message;
    }

    std::string raw_message;
    SourceLocationRange sloc_range;
};

#endif
