import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    whole_src = u.text.splitlines()

    def extract_sloc(sloc_range):
        f = sloc_range.start
        l = sloc_range.end
        return (f.line, f.column), (l.line, l.column)

    def format_output(s_l, s_c, e_l, e_c):
        lines = whole_src[s_l - 1:e_l]
        underline_starts = [s_c - 1] + [0] * (len(lines) - 1)
        underline_ends = [len(l) for l in lines[:-1]] + [e_c - 1]
        out = list(lines)
        for i in range(len(lines)):
            out.insert(i * 2 + 1, "{}{}".format(
                " " * underline_starts[i],
                "^" * (underline_ends[i] - underline_starts[i])
            ))

        return "\n".join(out)

    print('')
    for n in u.root.findall(lal.Name):
        if n.p_is_call:
            (s_l, s_c), (e_l, e_c) = extract_sloc(n.sloc_range)
            print(format_output(s_l, s_c, e_l, e_c))

    print('')

print('Done')
