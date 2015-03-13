## vim: filetype=cpp

#ifndef C_UTILS_HPP
#define C_UTILS_HPP

#include "lexer.hpp"

/* Unwrap a generic pointer (in practice: a pointer used in the C API) into the
   appropriate C++ type.  */
template <typename T>
T*
unwrap (void *object) {
    return static_cast<T*>(object);
}

/* Cast a SourceLocation value into the corresponding type for use in the C
   API.  */
${capi.sloc_type.tagged_name}
wrap(const SourceLocation& sloc) {
    return {sloc.line, sloc.column};
}

/* Likewise SourceLocationRange.  */
${capi.sloc_range_type.tagged_name}
wrap(const SourceLocationRange& sloc_range) {
    return {
        wrap(sloc_range.get_start()),
        wrap(sloc_range.get_end()),
    };
}

/* Cast a C API source location into SourceLocation.  */
SourceLocation
unwrap(const ${capi.sloc_type.tagged_name}& sloc) {
    return SourceLocation(sloc.line, sloc.column);
}

/* Likewise for SourceLocationRange.  */
SourceLocationRange
unwrap(const ${capi.sloc_range_type.tagged_name}& sloc_range) {
    return SourceLocationRange(unwrap(sloc_range.start),
                               unwrap(sloc_range.end));
}

#endif
