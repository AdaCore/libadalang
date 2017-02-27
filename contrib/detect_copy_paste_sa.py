#! /usr/bin/env python

"""
This script will detect copy-pastes in the input Ada sources, taking as
input list of files and directories

It starts by turning the text of the Ada sources into a string of hashes,
roughly one per logical line of code.
"""

import argparse
import itertools
import libadalang as lal
from os import walk
import os.path
import datetime


# Suffix array computation in linear time using Karkkainen Sanders skew
# algorithm. This is the exact implementation found at the end of the
# original paper (in C++ in the paper).
# Original paper can be found at:
#   https://pdfs.semanticscholar.org/8dfc/1a49894632a27a88490db18441180a215fe2.pdf
#
# For note this algorithm was to first to propose linear time SA construction. A
# faster one has been discoveed since:
# Nong, Ge; Zhang, Sen; Chan, Wai Hong (2009). Linear Suffix Array Construction
# by Almost Pure Induced-Sorting. 2009 Data Compression Conference

def radix_pass(a, b, r, n, k):
    c = [0] * (k + 1)
    for i in range(n):
        c[r[a[i]]] += 1
    s = 0
    for i in range(k + 1):
        t = c[i]
        c[i] = s
        s += t
    for i in range(n):
        b[c[r[a[i]]]] = a[i]
        c[r[a[i]]] += 1


def suffix_array(s, k=256, n=None):
    SA = []
    if n is None:
        n = len(s)
        s += [0, 0, 0]
    # print 'sa size: %s' % n
    n0 = (n + 2) / 3
    n1 = (n + 1) / 3
    n2 = n / 3
    n02 = n0 + n2

    s12 = [0] * (n02 + 3)
    SA12 = [0] * (n02 + 3)
    s0 = [0] * n0
    SA0 = [0] * n0
    j = 0
    for i in range(n + (n0 - n1)):
        if i % 3 != 0:
            s12[j] = i
            j += 1

    # print s12
    radix_pass(s12, SA12, s[2:], n02, k)
    radix_pass(SA12, s12, s[1:], n02, k)
    radix_pass(s12, SA12, s, n02, k)
    # print SA12

    name = 0
    c0 = c1 = c2 = -1
    for i in range(n02):
        if s[SA12[i]] != c0 or s[SA12[i] + 1] != c1 or s[SA12[i] + 2] != c2:
            name += 1
            c0 = s[SA12[i]]
            c1 = s[SA12[i] + 1]
            c2 = s[SA12[i] + 2]
        if SA12[i] % 3 == 1:
            s12[SA12[i] / 3] = name
        else:
            s12[SA12[i] / 3 + n0] = name

    # print name, n02
    # print s12
    if name < n02:
        SA12 = suffix_array(s12, name, n02)
    else:
        for i in range(n02):
            SA12[s12[i] - 1] = i
    # print SA12

    j = 0
    for i in range(n02):
        if SA12[i] < n0:
            s0[j] = 3 * SA12[i]
            j += 1
    radix_pass(s0, SA0, s, n0, k)
    p = 0
    t = n0 - n1
    for k in range(n):
        i = SA12[t] * 3 + 1 \
            if SA12[t] < n0 else (SA12[t] - n0) * 3 + 2
        j = SA0[p]
        if (SA12[t] < n0 and (s[i], s12[SA12[t] + n0]) <=
                (s[j], s12[j/3])) or \
                (s[i], s[i+1], s12[SA12[t] - n0 + 1]) <= \
                (s[j], s[j+1], s12[j/3+n0]):
            SA.append(i)
            t += 1
            if t == n02:
                k += 1
                while p < n0:
                    SA.append(SA0[p])
                    p += 1
                break
        else:
            SA.append(j)
            p += 1
            if p == n0:
                while t < n02:
                    SA.append(SA12[t] * 3 + 1 if SA12[t] < n0
                              else (SA12[t] - n0) * 3 + 2)
                    t += 1
                break

    return SA


small_subp_body_limit = 10


def location(node):
    return (node.token_start._sloc_range.start.line,
            node.token_start._sloc_range.start.column)


def line_range(node):
    return (node.token_start._sloc_range.start.line,
            node.token_end._sloc_range.end.line)


def start_line(node):
    return node.token_start._sloc_range.start.line


def end_line(node):
    return node.token_end._sloc_range.end.line


def collect_local_names(locnames, node):
    """
    Collect all the names defined locally in the subtree rooted at node.

    :type node: lal.AdaNode
    :type locnames: set
    """
    if node is None:
        return

    if node.is_a(lal.BaseTypeDecl):
        if node.f_type_id is not None:
            locnames.add(node.f_type_id.text)
    elif node.is_a(lal.EntryDecl):
        if node.f_entry_id is not None:
            locnames.add(node.f_entry_id.text)
    elif node.is_a(lal.EnumLiteralDecl):
        if node.f_enum_identifier is not None:
            locnames.add(node.f_enum_identifier.text)
    elif hasattr(node, 'f_id'):
        if node.f_id is not None:
            locnames.add(node.f_id.text)
    elif hasattr(node, 'f_ids'):
        for ident in node.f_ids:
            locnames.add(ident.text)

    for sub in node:
        collect_local_names(locnames, sub)


class Code(object):
    """
    Define a 'code' for a construct rooted at a given node, which consists in 3
    fields:
    - a hash encoding a construct
    - a node representing this construct
    - the name of the file containing the node
    """
    __slots__ = ('h', 'node', 'filename')

    def __init__(self, h, node, filename):
        self.h = h
        self.node = node
        self.filename = filename

    @property
    def line(self):
        return self.node.token_start._sloc_range.start.line


def encode(f, locnames, node):
    """
    Return the 'codes' for the subtree rooted at the argument 'node'.

    This function is critical to obtain good copy-pastes later, as the Suffix
    Tree is based on the list of hashes produced here.

    There are three benefits from using hashes rather than directly a list of
    tokens:
    (1) The list obtained is much smaller, allowing for quicker analysis time.
    (2) Irrelevant tokens can be ignored (e.g. end tokens for declarations),
        allowing for better copy-pastes (e.g. avoid starting a copy-paste on
        the "end Proc;" line of a procedure body).
    (3) Various adjustments can be made to decrease the relative importance of
        some constructs. For example, lists of subprogram declarations and
        object declarations without initialization expression are represented
        by a single hash, to avoid getting irrelevant copy-pastes of lists of
        declarations (due to our abstraction of identifiers as '$').

    This technique allows to get the speed of token based techniques based on
    Suffix Tree, with the quality of the copy-pastes obtained by syntactic
    methods. In particular, using hashes rather than tokens may make it useless
    to post-process the copy-pastes to exclude irrelevant tokens at the start
    or end, as done in "Clone Detection Using Abstract Syntax Suffix Trees" by
    Rainer Koschke, Raimar Falke and Pierre Frenzel, available at
    https://www.informatik.uni-bremen.de/st/papers/astclones-wcre06.pdf

    :type f: string
    :param set|None locnames: Set of locally defined names in the file or None
                              when all identifiers should be ignored.
    :type node: lal.AdaNode
    :rtype: [(int, lal.AdaNode, string)]
    """
    def strcode(node):
        """
        Return a string that encodes the argument 'node'. All tokens in the
        subtree rooted at 'node' are abstracted by their kind, except that:

        - local identifiers are explicitly abstracted away by '$'
          (shorted than using explicit kind + field f_tok)
        - global identifiers are not abstracted away
        - string and character literals are not abstracted away
        - component names are not abstracted away. Without semantic
          information, this applies in fact to all names appearing as suffix
          in a DottedName, so we may capture here qualified names (like
          Package.Function) and prefix call notation for tagged typed (like
          Object.Function). To be improved when we have semantic information.

        :type node: lal.AdaNode
        :rtype: string
        """
        if node is None:
            return ''
        # Only abstract away the names defined locally in the file, unless
        # locnames is None to indicate that all identifiers should be ignored.
        elif node.is_a(lal.Identifier):
            if locnames is None or node.text in locnames:
                return '$'
            else:
                return node.text
        # Do not abstract the component names
        elif node.is_a(lal.DottedName):
            return strcode(node.f_prefix) + '.' + node.f_suffix.text
        # Do not abstract away literals
        elif node.is_a(lal.StringLiteral,
                       lal.CharLiteral,
                       lal.IntLiteral,
                       lal.RealLiteral):
            return node.text
        else:
            return ' '.join([node.token_start.kind] +
                            [strcode(sub) for sub in node])

    def strcodes(nodes):
        """
        Return a string that encodes the argument 'nodes'.

        :type node: [lal.AdaNode]
        :rtype: string
        """
        assert (len(nodes) > 0)
        return ' '.join([strcode(node) for node in nodes])

    def hashcode(node):
        """
        Return a hash that encodes the argument 'node'.

        :type node: lal.AdaNode
        :rtype: int
        """
        return hash(strcode(node))

    def enc(node):
        if node is None:
            return []
        elif node.is_a(lal.CompilationUnit):
            return enc(node.f_body)
        elif node.is_a(lal.DeclarativePart):
            return enc(node.f_decls)
        elif node.is_a(lal.HandledStmts):
            return enc(node.f_stmts) + enc(node.f_exceptions)
        elif node.is_a(lal.BlockStmt):
            return enc(node.f_decls) + enc(node.f_stmts)
        elif node.is_a(lal.IfStmt):
            return ([Code(hash("if " + strcode(node.f_cond_expr)), node, f)] +
                    enc(node.f_then_stmts) +
                    enc(node.f_alternatives) +
                    enc(node.f_else_stmts))
        elif node.is_a(lal.ElsifStmtPart):
            return ([Code(hash("elsif " + strcode(node.f_cond_expr)),
                     node, f)] +
                    enc(node.f_stmts))
        elif node.is_a(lal.CaseStmt):
            return ([Code(hash("case " + strcode(node.f_case_expr)),
                     node, f)] +
                    enc(node.f_case_alts))
        elif node.is_a(lal.CaseStmtAlternative):
            return ([Code(hash("when " + strcode(node.f_choices)), node, f)] +
                    enc(node.f_stmts))
        elif node.is_a(lal.LoopStmt):
            return ([Code(hash("loop " + strcode(node.f_spec)), node, f)] +
                    enc(node.f_stmts))

        # Base case, where we encode a construct as a single hash. This is
        # naturally the case for expressions, but we also do the same for a
        # number of constructs which would be otherwise encoded as many hashes
        # (because they have many subnodes in the AST), thus over-emphasizing
        # their size for the detection of copy-pastes.
        elif node.is_a(lal.Expr,
                       lal.ObjectDecl,
                       lal.SubpSpec,
                       lal.SubpDecl,
                       lal.ParamSpec,
                       lal.ComponentDecl,
                       lal.GenericInstantiation,
                       lal.SimpleStmt,
                       lal.BaseTypeDecl,
                       lal.PragmaNode):
            return [Code(hashcode(node), node, f)]

        # Hash together sequences of subprogram declarations, or object
        # declarations without initializing expression, or small subprogram
        # bodies, to avoid getting spurious copy-pastes consisting mostly of
        # these.
        elif node.is_a(lal.AdaNodeList):
            res = []
            acc = []
            for sub in node:
                if (sub.is_a(lal.SubpDecl) or
                    (sub.is_a(lal.ObjectDecl) and
                        sub.f_default_expr is None) or
                    (sub.is_a(lal.SubpBody) and
                        end_line(sub) - start_line(sub) <
                        small_subp_body_limit)):
                    acc.append(sub)
                else:
                    if len(acc) > 0:
                        res.append(Code(hash(strcodes(acc)), acc[0], f))
                        acc = []
                    res += enc(sub)
            if len(acc) > 0:
                res.append(Code(hash(strcodes(acc)), acc[0], f))
                acc = []
            return res

        # It would be nicer to ignore the final identifier f_end_id of various
        # constructs when present, to avoid starting a copy-paste on an ending
        # line. We currently only do it for subprogram bodies, by explicitly
        # listing all the other fields that we take into account.
        elif node.is_a(lal.SubpBody):
            subs = [node.f_overriding, node.f_subp_spec, node.f_aspects,
                    node.f_decls, node.f_stmts]
            return ([Code(hash(node.token_start.kind), node, f)] +
                    list(itertools.chain.from_iterable(
                        [enc(sub) for sub in subs])))

        # Do not produce a hash for a list, only for its subnodes, in
        # particular to avoid getting a spurious hash for an empty list, which
        # would lead to copy-pastes starting on an irrelevant line.
        elif node.is_a(lal.AdaList):
            return (list(itertools.chain.from_iterable(
                         [enc(sub) for sub in node])))

        # Default case, where we hash the kind of the first token for the node,
        # followed by encodings for its subnodes.
        else:
            return ([Code(hash(node.token_start.kind), node, f)] +
                    list(itertools.chain.from_iterable(
                        [enc(sub) for sub in node])))

    # Call the encoding function on the node
    return enc(node)


def do_files(files, args):
    """
    Analyze a list of files. Issue messages on longer copy-pastes, either
    inside the same file, or between different files.
    """
    start_time = datetime.datetime.now()

    def show_time(start_time, msg, reset=True):
        now = datetime.datetime.now()
        print '%3.3f: %s' % ((now - start_time).total_seconds(), msg)
        if reset:
            start_time = now
        return start_time

    contexts = [(f, lal.AnalysisContext()) for f in files]
    units = [(f, c.get_from_file(f)) for (f, c) in contexts]

    # For the analysis of multiple files, issue a message for files that are
    # not parsable, and proceed with others.
    for (f, unit) in units:
        if unit.root is None:
            print 'Could not parse {}:'.format(f)

    units = [(f, unit) for (f, unit) in units if unit.root is not None]
    start_time = show_time(start_time,
                           'libadalang analysis (%s units)' % len(units))

    # Intersperse the codes of each file with a "code" using a different
    # integer as hash (starting from 0 and counting upwards), which serves as a
    # likely unique terminator, to avoid detecting copy-pastes that would start
    # on a file and end up in another.
    codes = []
    for i, (f, unit) in enumerate(units):
        if args.ignore_ids:
            locnames = None
        else:
            locnames = set()
            collect_local_names(locnames, unit.root)
        codes += encode(f, locnames, unit.root) + [Code(i, unit.root, f)]
    start_time = show_time(start_time,
                           'encode ast (code size: %s)' % len(codes))

    # rerank all hashes to lower k (and thus allow fast radix sort with
    # buckets)
    rank = 1
    ranks = {}
    ranked_code = []
    for code in codes:
        if code.h not in ranks:
            ranks[code.h] = rank
            rank += 1
        ranked_code.append(ranks[code.h])

    result = suffix_array(ranked_code, k=rank)

    start_time = show_time(start_time, 'compute suffix array')

    # Now build the lcp. could be build during sa build.

    copy_pastes = {}

    for index, suffix1 in enumerate(result):
        if index == len(result) - 1:
            break
        suffix2 = result[index + 1]

        same = 0
        while True:
            if ranked_code[suffix1 + same] == ranked_code[suffix2 + same]:
                same += 1
            else:
                break

        # Two suffixes with similarities lasting more than 20 lines
        if same > 0:
            code1_filename = codes[suffix1].filename
            code1_range = (start_line(codes[suffix1].node),
                           start_line(codes[suffix1 + same].node))

            code2_filename = codes[suffix2].filename
            code2_range = (start_line(codes[suffix2].node),
                           start_line(codes[suffix2 + same].node))

            if code1_range[1] - code1_range[0] >= args.min_size:
                if str(code1_filename) == str(code2_filename) and (
                        (code2_range[0] <= code1_range[1] <= code2_range[1]) or
                        (code2_range[0] <= code1_range[0] <= code2_range[1])):
                    continue

                if code1_filename not in copy_pastes:
                    copy_pastes[code1_filename] = [(code1_range,
                                                    code2_filename,
                                                    code2_range)]
                else:
                    found = False
                    for index, k in enumerate(copy_pastes[code1_filename]):
                        if code1_range[0] <= k[0][0] and \
                                code1_range[1] >= k[0][1] and \
                                code2_range[0] <= k[2][0] and \
                                code2_range[1] >= k[2][1] and \
                                code2_filename == k[1]:
                            copy_pastes[code1_filename][index] = (
                                code1_range,
                                code2_filename,
                                code2_range)
                            found = True
                            break
                        elif code1_range[0] >= k[0][0] and \
                                code1_range[1] <= k[0][1] and \
                                code2_range[0] >= k[2][0] and \
                                code2_range[1] <= k[2][1] and \
                                code2_filename == k[1]:
                            found = True
                            break
                    if not found:
                        copy_pastes[code1_filename].append((code1_range,
                                                            code2_filename,
                                                            code2_range))

    for filename, l in copy_pastes.iteritems():
        for v in l:
            print '%4d: %-32s (%4d,%4d) ~= %-32s (%4d, %4d)' % \
                (v[0][1] - v[0][0],
                 os.path.relpath(filename, args.rel_path),
                 v[0][0], v[0][1],
                 os.path.relpath(v[1], args.rel_path),
                 v[2][0], v[2][1])
    show_time(start_time, 'find copy/paste code')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('files', help='The directory or files to analyze',
                        type=str, nargs='+', metavar='F')
    parser.add_argument(
        '--ignore-ids',
        action='store_true',
        default=False,
        help='ignore all identifiers when comparing code snippets')
    parser.add_argument(
        '--min-size', type=int,
        default=20,
        help='minimum size of reported copy-paste (default: 20 lines)')
    parser.add_argument(
        '--rel-path', default='')
    args = parser.parse_args()

    if len(args.files) == 1:
        args.rel_path = args.files[0]

    file_list = []
    for path in args.files:
        if os.path.isfile(path):
            file_list.append(path)
        elif os.path.isdir(path):
            for root, dirs, files in os.walk(path):
                file_list += [os.path.join(root, f) for f in files
                              if f.endswith('.adb')]
        else:
            print 'error: %s does not exist' % path
    do_files(file_list, args)

if __name__ == '__main__':
    main()
