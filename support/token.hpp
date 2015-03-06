#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <cstdint>
#include <cassert>
#include <string>
#include <sstream>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

enum RelativePosition {
    BEFORE,
    IN,
    AFTER
};

struct SourceLocation {
    uint32_t line;
    uint16_t column;

    SourceLocation() : line(0), column(0) {}
    SourceLocation(uint32_t line, uint16_t column)
      : line(line), column(column)
    {
        /* (0, 0) stands for the "null" value, but any other use of 0 is
           invalid otherwise.  */
        assert((line == 0) == (column == 0));
    }
    bool is_null() const { return line == 0; }

    /* Compute the relative position of SLOC with respect to this sloc.  */
    RelativePosition compare(const SourceLocation &sloc) const;
    std::string repr() const {
        std::ostringstream oss;
        oss << line << ":" << column;
        return oss.str();
    }

    boost::property_tree::ptree get_property_tree();
};


struct SourceLocationRange {
    uint32_t start_line, end_line;
    uint16_t start_column, end_column;

    SourceLocationRange()
      : start_line(0),
        end_line(0),
        start_column(0),
        end_column(0)
    {}
    SourceLocationRange(SourceLocation start, SourceLocation end)
      : start_line(start.line),
        end_line(end.line),
        start_column(start.column),
        end_column(end.column)
    {
        assert(start.is_null() == end.is_null());
    }
    SourceLocationRange(uint32_t start_line, uint32_t end_line,
                        uint16_t start_column, uint16_t end_column)
      : start_line(start_line),
        end_line(end_line),
        start_column(start_column),
        end_column(end_column)
    {}

    SourceLocation get_start() const {
        return SourceLocation(start_line, start_column);
    }

    SourceLocation get_end() const {
        return SourceLocation(end_line, end_column);
    }

    bool is_null() const { return start_line == 0; }

    /* Compute the relative position of SLOC with respect to this sloc
       range.  */
    RelativePosition compare(const SourceLocation &sloc) const;

    std::string repr() const {
        return get_start().repr() + "-" + get_end().repr();
    }

    boost::property_tree::ptree get_property_tree();
};


struct Token {
    uint16_t id;
    const char* text;
    SourceLocationRange sloc_range;
} __attribute__((packed));

#endif
