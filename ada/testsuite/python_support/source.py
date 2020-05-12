def src_slice(src_lines, sloc_range):
    """
    Given "src_lines", a list of strings representing a source file, return a
    string slice that represents the area that "sloc_range" covers.
    """
    result = []
    for line in range(sloc_range.start.line,
                      sloc_range.end.line + 1):
        low = (sloc_range.start.column - 1
               if line == sloc_range.start.line else 0)
        high = (sloc_range.end.column - 1
                if line == sloc_range.end.line else -1)
        result.append(src_lines[line - 1][low:high])

    return '\n'.join(result)
