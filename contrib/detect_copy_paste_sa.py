#! /usr/bin/env python

"""
This script will detect copy-pastes in the input Ada sources, taking as
input list of files and directories.

It starts by turning the text of the Ada sources into a string of hashes,
roughly one per logical line of code.
"""

import argparse
import datetime
import os

import libadalang as lal


# Suffix array computation in linear time using Karkkainen Sanders skew
# algorithm. This is the exact implementation found at the end of the
# original paper (in C++ in the paper).
# Original paper can be found at:
# <https://pdfs.semanticscholar.org/8dfc/1a49894632a27a88490db18441180a215fe2.pdf>.
#
# For note this algorithm was to first to propose linear time SA
# construction. A faster one has been discoveed since:
# Nong, Ge; Zhang, Sen; Chan, Wai Hong (2009). Linear Suffix Array Construction
# by Almost Pure Induced-Sorting. 2009 Data Compression Conference.
#
# Also in 2009 a new algorithm was found to handle in linear time dynamic
# suffix array (compute a suffix array after insertion/deletion of part of
# the input).

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

    # radix sort of s12 triples
    radix_pass(s12, SA12, s[2:], n02, k)
    radix_pass(SA12, s12, s[1:], n02, k)
    radix_pass(s12, SA12, s, n02, k)

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

    if name < n02:
        SA12 = suffix_array(s12, name, n02)
    else:
        for i in range(n02):
            SA12[s12[i] - 1] = i

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
                (s[j], s12[j / 3])) or \
                (s[i], s[i + 1], s12[SA12[t] - n0 + 1]) <= \
                (s[j], s[j + 1], s12[j / 3 + n0]):
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


class Code(object):
    """
    Define a 'code' for a construct rooted at a given node, which consists in 3
    fields:
    - a hash encoding a construct;
    - a node representing this construct;
    - the name of the file containing the node.
    """
    __slots__ = ('h', 'node', 'filename')

    def __init__(self, h, node, filename):
        self.h = h
        self.node = node
        self.filename = filename

    @property
    def line(self):
        return self.node.token_start.sloc_range.start.line

    @property
    def end_line(self):
        return self.node.token_end.sloc_range.end.line


class Encoder(object):

    JOKER = 1

    def __init__(self):
        self.rank = 4
        self.rank_dict = {}

    def set_local_names(self, node, reset=True):
        """Collect local names for a given subtree.

        :type node: lal.AdaNode
        :return: the list of local names
        :rtype: set[str]
        """

        if reset:
            self.local_names = set()

        if node.is_a(lal.BaseTypeDecl):
            if node.f_type_id is not None:
                self.local_names.add(node.f_type_id.text)
        elif node.is_a(lal.EntryDecl):
            if node.f_entry_id is not None:
                self.local_names.add(node.f_entry_id.text)
        elif node.is_a(lal.EnumLiteralDecl):
            if node.f_enum_identifier is not None:
                self.local_names.add(node.f_enum_identifier.text)
        elif hasattr(node, 'f_id'):
            if node.f_id is not None:
                self.local_names.add(node.f_id.text)
        elif hasattr(node, 'f_ids'):
            for ident in node.f_ids:
                self.local_names.add(ident.text)

        for sub in node:
            if sub is not None:
                self.set_local_names(sub, reset=False)

    def encode(self, filename, node, ignore_ids=False):
        if ignore_ids:
            self.local_names = set()
        else:
            self.set_local_names(node, reset=True)

        result = list(self.encode_internal(filename, node))

        # Add marker unique to the file to avoid cross file boundaries matches
        result.append(Code(self.rank, node, filename))
        self.rank += 1
        return result

    def encode_internal(self, filename, node):
        # Skip declaration as we are usually interested in bodies
        if node.is_a(lal.SubpDecl, lal.ObjectDecl, lal.BaseTypeDecl):
            pass
        # Handle constructs with children
        elif len(node) > 0:
            result = []
            for sub in node:
                if sub is not None:
                    result += list(self.encode_internal(filename, sub))
            if len(result) == 1:
                yield result[0]
            elif len(result) > 1:
                for r in result:
                    yield r
        # Ignore empty lists
        elif node.is_a(lal.AdaNodeList, lal.AdaList, lal.PragmaNodeList):
            pass
        # Local names are made 'anonymous'
        elif node.text in self.local_names:
            yield Code(Encoder.JOKER, node, filename)
        else:
            # Finaly take care of other entities. Include kind of the entities
            # in the name to avoid collisions.
            s = str(node.kind_name) + ':' + node.text
            if s not in self.rank_dict:
                self.rank_dict[s] = self.rank
                self.rank += 1
            yield Code(self.rank_dict[s], node, filename)


class CodeChunk(object):
    """Class that represent code chunks."""

    __slots__ = ('path', 'begin', 'end', 'size')

    def __init__(self, path, begin, end, size):
        """Initialize a code chunk.

        :param path: Filename containing the source code.
        :type path: str
        :param begin: First line of the code chunk.
        :type begin: int
        :param end: Last line of the code chunk.
        :type end: int
        :param size: Number of significative syntactic elements.
        :type size: int
        """
        self.path = path
        self.begin = begin
        self.end = end
        self.size = size

    def __len__(self):
        """Return length in lines of the code chunk."""
        result = self.end - self.begin
        if result < 0:
            print('invalid chunk %s, %s %s' % (self.path, self.begin,
                                               self.end))
            # Invalid chunk...
            return 0
        else:
            return result

    def __str__(self):
        return '%s:%s:%s' % (self.path, self.begin, self.end)

    def intersect_with(self, cr):
        """Check if two chunks do intersect.

        :param cr: Code chunk to compare with.
        :type cr: CodeChunk
        :return: True if the two chunk share some lines of code,
            False otherwise.
        :rtype: bool
        """
        return self.path == cr.path and (cr.begin <= self.begin <= cr.end or
                                         cr.begin <= self.end <= cr.end)

    def is_wider_than(self, cr):
        """Check if current chunk is a superset of another chunk.

        :param cr: Code chunk to compare with.
        :type cr: CodeChunk
        :return: True if current chunk is a superset, False otherwise.
        :rtype: bool
        """
        return (self.path == cr.path and
                self.begin <= cr.begin and
                self.end >= cr.end)


def do_files(files, args):
    """
    Analyze a list of files. Issue messages on longer copy-pastes, either
    inside the same file, or between different files.
    """
    overall_start_time = datetime.datetime.now()
    start_time = datetime.datetime.now()

    def show_time(start_time, msg, reset=True):
        now = datetime.datetime.now()
        print('%-60s [%3.3fs]' % (msg, (now - start_time).total_seconds()))
        if reset:
            start_time = now
        return start_time

    contexts = [(f, lal.AnalysisContext()) for f in files]
    units = [(f, c.get_from_file(f)) for (f, c) in contexts]

    # For the analysis of multiple files, issue a message for files that are
    # not parsable, and proceed with others.
    for (f, unit) in units:
        if unit.root is None:
            print('Could not parse {}:'.format(f))

    units = [(f, unit) for (f, unit) in units if unit.root is not None]
    start_time = show_time(start_time,
                           'libadalang analysis (%s units)' % len(units))

    # All the units have been parsed correctly. Now encode the code into
    # a list of 'hashes'.
    codes = []
    encoder = Encoder()
    for i, (f, unit) in enumerate(units):
        codes += encoder.encode(f, unit.root, args.ignore_ids)
    start_time = show_time(start_time,
                           'encode ast (code size: %s)' % len(codes))

    ranked_code = [code.h for code in codes]
    result = suffix_array(ranked_code, k=encoder.rank)
    start_time = show_time(start_time,
                           'compute suffix array (rank:%s)' % encoder.rank)

    # Copy/Paste results arranged by paths
    copy_pastes = {}

    # Keep track of some stats
    stats = {'skipped': 0, 'prefix': 0, 'no_prefix': 0}

    # Iterate over the suffixes. Chunk of duplicate code will necessary be
    # prefixes of adjacent suffixes.
    for index in range(len(result) - 1):
        # Get the next two suffixes
        suffix = (result[index], result[index + 1])

        # Discard if nothing in common
        if ranked_code[suffix[0]] != ranked_code[suffix[1]]:
            stats['no_prefix'] += 1
            continue

        # Check if a longuer prefix exist in the suffix array. Analyse
        # only the longuest prefixes.
        if suffix[0] > 0 and suffix[1] > 0 and \
                codes[suffix[0] - 1].h == codes[suffix[1] - 1].h:
            stats['skipped'] += 1
            continue

        # Find the length of the common prefix between the two suffixes.
        # The following code is a bit naive and can be removed in case the
        # LCP table is computed at the same time as the suffix array.
        # With skew algorithm this is possible (keeping the linear property).

        # Size of the common prefix
        prefix_length = 0

        while ranked_code[suffix[0] + prefix_length] == \
                ranked_code[suffix[1] + prefix_length]:
            prefix_length += 1

        stats['prefix'] += 1
        # Two suffixes with similarities lasting more than min_size "items"
        if prefix_length + 1 >= args.min_size:

            code = (CodeChunk(codes[suffix[0]].filename,
                              codes[suffix[0]].line,
                              codes[suffix[0] + prefix_length - 1].line,
                              prefix_length),
                    CodeChunk(codes[suffix[1]].filename,
                              codes[suffix[1]].line,
                              codes[suffix[1] + prefix_length - 1].line,
                              prefix_length))
            if code[0].path > code[1].path or \
                    (code[0].path == code[1].path and
                     code[0].begin > code[1].begin):
                # By ordering in lexicograhic order the paths we avoid
                # duplicates.
                code = (code[1], code[0])

            if len(code[0]) >= args.min_lines:

                if code[0].intersect_with(code[1]):
                    # A code cannot be a copy of itself
                    print('ingore %s vs %s' % (code[0], code[1]))
                    continue

                if code[0].path not in copy_pastes:
                    copy_pastes[code[0].path] = []

                found = False
                # This search is unoptimized but efficient as we usually
                # deal with few elements per file (TODO: to be improved).
                for index, elt in enumerate(copy_pastes[code[0].path]):
                    if code[0].is_wider_than(elt[0]) and \
                            code[1].is_wider_than(elt[1]):
                        # Code duplication is a superset of a previous one, so
                        # replace.
                        copy_pastes[code[0].path][index] = code
                        found = True
                        break
                    elif elt[0].is_wider_than(code[0]) and \
                            elt[1].is_wider_than(code[1]):
                        # Code duplication is a subset of a previous one, so
                        # ignore.
                        found = True
                        break
                if not found:
                    # New code chunk
                    copy_pastes[code[0].path].append(code)
            else:
                pass
                # print 'discard %s' % code[0]
    show_time(start_time,
              'find copy/paste code (skipped: %(skipped)s, '
              'with prefix: %(prefix)s, with no prefix: %(no_prefix)s' % stats)

    # Display the results
    if copy_pastes:
        print('%4s %4s: %s' % ('LINE', 'SIZE', 'CHUNKS'))
    for chunks in copy_pastes.itervalues():
        for chunk in sorted(chunks, key=lambda x: x[0].size):
            print('%4d %4d: %-40s (%4d,%4d) ~= %-40s (%4d, %4d)' % (
                len(chunk[0]),
                chunk[0].size,
                os.path.relpath(chunk[0].path, args.rel_path),
                chunk[0].begin, chunk[0].end,
                os.path.relpath(chunk[1].path, args.rel_path),
                chunk[1].begin, chunk[1].end))
    show_time(overall_start_time, 'Overall')


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
        help='minimum size of reported copy-paste '
        '(default: 20 significative syntactic elements)')
    parser.add_argument(
        '--min-lines', type=int,
        default=20,
        help='minimum size in lines of reported copy-paste '
        '(default: 20)')
    parser.add_argument(
        '--dump-code', action='store_true', default=False)
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
            print('error: %s does not exist' % path)
    do_files(file_list, args)


if __name__ == '__main__':
    main()
