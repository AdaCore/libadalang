#! /usr/bin/env python

"""
This script will detect copy-pastes in the input Ada sources, taking as
input either a list of files, or a single directory.

It starts by turning the text of the Ada sources into a string of hashes,
roughly one per logical line of code.

It applies the algorithm of Ukkonnen to build the Suffix Tree of the above
string, and uses this structure to get the longest repeated substrings. If this
corresponds to a valid copy-paste (i.e. no overlap between the corresponding
slocs) and one of interest (i.e. corresponding to a minimum number of slocs),
then we report it. The copy-paste may be between 2 locations or more, either in
the same file or between different files.

The above algorithm only reports the longest copy-paste. To find all
copy-pastes of a minimal length, we repeatedly apply this algorithm to a
shrinking string of hashes, from which we remove the hashes for the previously
found copy-paste (whether it is reported or not).

The implementation of Ukkonnen's algorithm is adapted from the C code at
<http://www.geeksforgeeks.org/suffix-tree-application-3-longest-repeated-substring/>.
"""

import argparse
import itertools
import libadalang as lal
import os.path


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The directory or files to analyze',
                    type=str, nargs='+', metavar='F')
parser.add_argument('--debug', dest='debug',
                    action='store_const', const=True, default=False,
                    help='issue debugging information')
parser.add_argument('--ignore-ids', dest='ignore_ids',
                    action='store_const', const=True, default=False,
                    help='ignore all identifiers when comparing code snippets')
parser.add_argument(
    '--size-min', dest='size_min',
    type=int, default=20,
    help='minimum size of reported copy-paste (default: 20 lines)')

# Global variables
debug = False  # Debugging or not
size_min = 20  # Minimum size of copy-paste
ignore_ids = False  # Whether to ignore all identifiers or not

# Global constants
small_subp_body_limit = 10
# Limit under which subprogram bodies are collapsed with other declarations


def location(node):
    return (node.token_start.sloc_range.start.line,
            node.token_start.sloc_range.start.column)


def line_range(node):
    return (node.token_start.sloc_range.start.line,
            node.token_end.sloc_range.end.line)


def start_line(node):
    return node.token_start.sloc_range.start.line


def end_line(node):
    return node.token_end.sloc_range.end.line


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
    - a hash encoding a construct;
    - a node representing this construct;
    - the name of the file containing the node.
    """
    def __init__(self, h, node, filename):
        self.h = h
        self.node = node
        self.filename = filename


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
    <https://www.informatik.uni-bremen.de/st/papers/astclones-wcre06.pdf>.

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
            return ' '.join([node.token_start.kind]
                            + [strcode(sub) for sub in node])

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
        elif node.is_a(lal.DeclBlock):
            return enc(node.f_decls) + enc(node.f_stmts)
        elif node.is_a(lal.IfStmt):
            return ([Code(hash("if " + strcode(node.f_cond_expr)), node, f)]
                    + enc(node.f_then_stmts)
                    + enc(node.f_alternatives)
                    + enc(node.f_else_stmts))
        elif node.is_a(lal.ElsifStmtPart):
            return ([Code(hash("elsif " + strcode(node.f_cond_expr)), node, f)]
                    + enc(node.f_stmts))
        elif node.is_a(lal.CaseStmt):
            return ([Code(hash("case " + strcode(node.f_expr)), node, f)]
                    + enc(node.f_alternatives))
        elif node.is_a(lal.CaseStmtAlternative):
            return ([Code(hash("when " + strcode(node.f_choices)), node, f)]
                    + enc(node.f_stmts))
        elif node.is_a(lal.BaseLoopStmt):
            return ([Code(hash("loop " + strcode(node.f_spec)), node, f)]
                    + enc(node.f_stmts))

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
                if (sub.is_a(lal.SubpDecl)
                    or (sub.is_a(lal.ObjectDecl) and
                        sub.f_default_expr is None)
                    or (sub.is_a(lal.SubpBody) and
                        end_line(sub) - start_line(sub)
                        < small_subp_body_limit)):
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

####################################################################
# Start of the Python program to implement Ukkonen's Suffix Tree   #
# Construction and then find Longest Repeated Substring            #
####################################################################

# This code is an adaptation in Python of the implementation of Ukkonnen's
# algorithm in C presented at
# <http://www.geeksforgeeks.org/suffix-tree-application-3-longest-repeated-substring/>.

# The comments of the C implementation have been kept, including the acronyms
# in the comments, which refer to specific steps of the algorithm:
# - APCFALZ: activePoint change for Active Length ZERO;
# - APCFWD: activePoint change for walk down;
# - APCFER3: activePoint change for extension rule 3;
# - APCFER2C1: activePoint change for extension rule 2, case 1;
# - APCFER2C2: activePoint change for extension rule 2, case 2.

# These are explained in details in the series of 6 blog posts detailing
# Ukkonnen's algorithm, found at
# <http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-1/>
# <http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-2/>
# <http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-3/>
# <http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-4/>
# <http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-5/>
# <http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-6/>.


class IntPtr(object):
    def __init__(self, i):
        self.i = i

    def get(self):
        return self.i

    def set(self, i):
        self.i = i


class Node(object):

    serial_generator = itertools.count(0)

    def __init__(self, root, start, end):
        """
        :type start: int
        :type end: IntPtr
        :rtype: Node
        """
        self.serial = next(self.serial_generator)

        self.children = {}

        # For root node, suffixLink will be set to NULL.  For internal nodes,
        # suffixLink will be set to root by default in current extension and
        # may change in next extension.

        # Pointer to other node via suffix link
        self.suffixLink = root

        # (start, end) interval specifies the edge, by which the node is
        # connected to its parent node. Each edge will connect two nodes, one
        # parent and one child, and (start, end) interval of a given edge will
        # be stored in the child node. Lets say there are two nods A and B
        # connected by an edge with indices (5, 8) then this indices (5, 8)
        # will be stored in node B.
        self.start = start
        self.end = end

        # suffixIndex will be set to -1 by default and actual suffix index will
        # be set later for leaves at the end of all phases.

        # For leaf nodes, it stores the index of suffix for the path from root
        # to leaf.
        self.suffixIndex = -1

    def __lt__(self, other):
        return self.serial < other.serial


def find_copy_pastes(codes, num_hash_limit, num_line_limit):
    """
    Main function to detect the longest copy-paste until no more is detected
    of a "hash length" and "line length" greater than some fixed limit.

    Currently use hash length of size_min and line length of size_min.
    """
    # codes is a list of pairs of (hashcode,node)

    text = []  # Text to analyze

    class Glob(object):
        root = None  # Pointer to root node

        # lastNewNode will point to newly created internal node, waiting for
        # it's suffix link to be set, which might get a new suffix link (other
        # than root) in next extension of same phase. lastNewNode will be set
        # to NULL when last newly created internal node (if there is any) got
        # it's suffix link reset to new internal node created in next extension
        # of same phase.

        lastNewNode = None
        activeNode = None

        # activeEdge is represeted as input string character index (not the
        # character itself).

        activeEdge = -1
        activeLength = 0

        # remainingSuffixCount tells how many suffixes yet to be added in tree
        remainingSuffixCount = 0
        leafEnd = IntPtr(-1)
        rootEnd = None
        splitEnd = None
        size = -1  # Length of input string

    def edgeLength(n):
        """
        :type n: Node
        :rtype: int
        """
        if n == Glob.root:
            return 0
        return n.end.get() - n.start + 1

    def walkDown(currNode):
        """
        :type currNode: Node
        :rtype: bool
        """
        # activePoint change for walk down (APCFWD) using Skip/Count Trick
        # (Trick 1). If activeLength is greater than current edge length, set
        # next internal node as activeNode and adjust activeEdge and
        # activeLength accordingly to represent same activePoint.
        if Glob.activeLength >= edgeLength(currNode):
            Glob.activeEdge += edgeLength(currNode)
            Glob.activeLength -= edgeLength(currNode)
            Glob.activeNode = currNode
            return True
        else:
            return False

    def extendSuffixTree(pos):
        """
        :type pos: int
        """
        # Extension Rule 1, this takes care of extending all leaves created so
        # far in tree.
        Glob.leafEnd.set(pos)

        # Increment remainingSuffixCount indicating that a new suffix added to
        # the list of suffixes yet to be added in tree.
        Glob.remainingSuffixCount += 1

        # Set lastNewNode to NULL while starting a new phase, indicating there
        # is no internal node waiting for it's suffix link reset in current
        # phase.
        Glob.lastNewNode = None

        # Add all suffixes (yet to be added) one by one in tree
        while Glob.remainingSuffixCount > 0:

            if Glob.activeLength == 0:
                Glob.activeEdge = pos  # APCFALZ

            # There is no outgoing edge starting with activeEdge from
            # activeNode.
            if text[Glob.activeEdge] not in Glob.activeNode.children:
                # Extension Rule 2 (A new leaf edge gets created)
                Glob.activeNode.children[text[Glob.activeEdge]] = \
                    Node(Glob.root, pos, Glob.leafEnd)

                # A new leaf edge is created in above line starting from an
                # existing node (the current activeNode), and if there is any
                # internal node waiting for it's suffix link get reset, point
                # the suffix link from that last internal node to current
                # activeNode. Then set lastNewNode to NULL indicating no more
                # node waiting for suffix link reset.
                if Glob.lastNewNode:
                    Glob.lastNewNode.suffixLink = Glob.activeNode
                    Glob.lastNewNode = None

            # There is an outgoing edge starting with activeEdge from
            # activeNode.
            else:
                # Get the next node at the end of edge starting with activeEdge
                nxt = Glob.activeNode.children[text[Glob.activeEdge]]
                if walkDown(nxt):  # Do walkdown
                    # Start from next node (the new activeNode)
                    continue

                # Extension Rule 3 (current character being processed is
                # already on the edge).
                if text[nxt.start + Glob.activeLength] == text[pos]:
                    # If a newly created node waiting for it's suffix link to
                    # be set, then set suffix link of that waiting node to
                    # current active node.
                    if Glob.lastNewNode and Glob.activeNode != Glob.root:
                        Glob.lastNewNode.suffixLink = Glob.activeNode
                        Glob.lastNewNode = None

                    # APCFER3
                    Glob.activeLength += 1
                    # STOP all further processing in this phase and move on to
                    # next phase.
                    break

                # We will be here when activePoint is in middle of the edge
                # being traversed and current character being processed is not
                # on the edge (we fall off the tree). In this case, we add a
                # new internal node and a new leaf edge going out of that new
                # node. This is Extension Rule 2, where a new leaf edge and a
                # new internal node get created.
                Glob.splitEnd = IntPtr(nxt.start + Glob.activeLength - 1)

                # New internal node
                split = Node(Glob.root, nxt.start, Glob.splitEnd)
                Glob.activeNode.children[text[Glob.activeEdge]] = split

                # New leaf coming out of new internal node
                split.children[text[pos]] = Node(Glob.root, pos, Glob.leafEnd)
                nxt.start += Glob.activeLength
                split.children[text[nxt.start]] = nxt

                # We got a new internal node here. If there is any internal
                # node created in last extensions of same phase which is still
                # waiting for it's suffix link reset, do it now.
                if Glob.lastNewNode:
                    # suffixLink of lastNewNode points to current newly created
                    # internal node.
                    Glob.lastNewNode.suffixLink = split

                # Make the current newly created internal node waiting for it's
                # suffix link reset (which is pointing to root at present). If
                # we come across any other internal node (existing or newly
                # created) in next extension of same phase, when a new leaf
                # edge gets added (i.e. when Extension Rule 2 applies is any of
                # the next extension of same phase) at that point, suffixLink
                # of this node will point to that internal node.
                Glob.lastNewNode = split

            # One suffix got added in tree, decrement the count of
            # suffixes yet to be added.
            Glob.remainingSuffixCount -= 1
            if Glob.activeNode == Glob.root and Glob.activeLength > 0:
                # APCFER2C1
                Glob.activeLength -= 1
                Glob.activeEdge = pos - Glob.remainingSuffixCount + 1
            elif Glob.activeNode != Glob.root:
                # APCFER2C2
                Glob.activeNode = Glob.activeNode.suffixLink

    def print_range(i, j):
        """
        :type i: int
        :type j: int
        """
        for k in range(j - i):
            print(text[i + k])

    # Print the suffix tree as well along with setting suffix index so tree
    # will be printed in DFS manner.  Each edge along with it's suffix index
    # will be printed.
    def setSuffixIndexByDFS(n, labelHeight):
        """
        :type n: Node
        :type labelHeight: int
        """
        if n is None:
            return

        if n.start != -1:  # A non-root node
            # Print the label on edge from parent to current node.  Uncomment
            # below line to print suffix tree.
            # TODO: print_range(n.start, n.end.get()).
            pass

        leaf = True
        for child in sorted(n.children.values()):
            # Current node is not a leaf as it has outgoing
            # edges from it.
            leaf = False
            setSuffixIndexByDFS(child, labelHeight + edgeLength(child))

        if leaf:
            n.suffixIndex = Glob.size - labelHeight

    # Build the suffix tree and print the edge labels along with
    # suffixIndex. suffixIndex for leaf edges will be >= 0 and
    # for non-leaf edges will be -1.
    def buildSuffixTree():
        Glob.size = len(text)
        Glob.rootEnd = IntPtr(- 1)

        # Root is a special node with start and end indices as -1,
        # as it has no parent from where an edge comes to root.
        Glob.root = Node(Glob.root, -1, Glob.rootEnd)

        Glob.activeNode = Glob.root  # First activeNode will be root
        for i in range(Glob.size):
            extendSuffixTree(i)
        labelHeight = 0
        setSuffixIndexByDFS(Glob.root, labelHeight)

    def getLongestRepeatedSubstring(curNode, curHeight):
        # Traverse the Suffix Tree to retrieve the internal node with a
        # sufficient height, which corresponds to a longest copy-paste.
        if curNode is None:
            return

        if curNode.suffixIndex == -1:  # If it is internal node
            ignore = False

            # Only consider node if none of its children is internal
            if any([curNode.children[i].suffixIndex == -1
                    for i in curNode.children.keys()]):
                ignore = True

            # Only consider node if no two suffixes have longer versions when
            # considering the previous hash.
            previous = [text[child.suffixIndex - 1]
                        if child.suffixIndex != 0
                        else 0
                        for child in sorted(curNode.children.values())]
            if len(previous) != len(set(previous)):
                ignore = True

            if ignore:
                for child in sorted(curNode.children.values()):
                    getLongestRepeatedSubstring(
                        child, curHeight + edgeLength(child))
                return

        # Ignore leaf nodes
        elif curNode.suffixIndex > -1:
            return

        # Only report copy-pastes that correspond to a minimal number of
        # hashes. This is loosely related to the number of lines, given the
        # heuristics used in the 'encode' function to give more or less
        # emphasis on some constructs by computing more or less hashes for
        # each.
        if curHeight > num_hash_limit:

            # Given a leaf node 'n' in the Suffix Tree (which is the case for
            # all children of node 'curNode', otherwise 'curNode' would have
            # been ignored previously), n.suffixIndex is the position in
            # 'codes' where the suffix corresponding to this node starts.

            # Hence, the triplets (hash, node, filename) corresponding to
            # a copy-paste for child 'n' are contained in the range from
            #   codes[n.suffixIndex]
            # to
            #   codes[n.suffixIndex + curHeight - 1]
            # and the corresponding nodes are retrieved from these by getting
            # the 'node' component of the code, and finally the starting line
            # for these nodes are obtained by calling start_line on these.

            # Compute the list of (filename, start_line, end_line) for each
            # copy-paste (of which there are at least 2, possibly more).
            locs = [(codes[n.suffixIndex].filename,
                     start_line(codes[n.suffixIndex].node),
                     start_line(
                         codes[n.suffixIndex + curHeight - 1].node))
                    for i, n in curNode.children.items()]

            # Sort the list to report the message on the first occurrence
            locs.sort()

            # Compute the number of lines in the copy-paste, both to avoid
            # reporting too short ones, and to use that in the message.
            fst_file, fst_start, fst_end = locs[0]
            numlines = fst_end - fst_start + 1

            # Ignore the potential copy-paste in two cases:
            # - if the number of actual code lines copy-pasted is too small;
            # - in case of overlap between the first code snippet and any other
            #   in the set.
            if (numlines < num_line_limit or
                any(start_loc <= fst_end
                    for (f, start_loc, end_loc) in locs[1:] if f == fst_file)):
                return (curHeight, curNode)

            msgs = ["code from line {} to line {}".format(start_loc, end_loc)
                    if f == fst_file
                    else "code from line {} to line {} in file {}".format(
                        start_loc, end_loc, f)
                    for (f, start_loc, end_loc) in locs[1:]]
            msg = " and ".join(msgs)

            # Print useful info about the copy-paste in debug mode
            if debug:
                print("copy-paste of {} hashes".format(curHeight))
                fst_index = curNode.children[
                    curNode.children.keys()[0]].suffixIndex
                fst_node = codes[fst_index].node
                snd_index = curNode.children[
                    curNode.children.keys()[1]].suffixIndex
                snd_node = codes[snd_index].node
                print("start node at index {} is {}".format(
                    fst_index, fst_node))
                print("other start node at index {} is {}".format(
                    snd_index, snd_node))

            print("{}:{}:1: copy-paste of {} lines detected with {}".format(
                fst_file, fst_start, numlines, msg))

    # Get the list of hashes from the 'codes'
    text = [code.h for code in codes]

    # Build the Suffix Tree using Ukkonnen's efficient algorithm
    buildSuffixTree()

    # Retrieve the longest copy-paste. A message might be issued by the call
    # when a copy-paste is found.
    getLongestRepeatedSubstring(Glob.root, 0)

####################################################################
# End of the Python program to implement Ukkonen's Suffix Tree     #
# Construction and then find Longest Repeated Substring            #
####################################################################


def do_file(f):
    """
    Analyze a single file. Issue messages on longer copy-pastes.
    """
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)

    # For the analysis of a single file, return in error if not parsable
    if unit.root is None:
        print('Could not parse {}:'.format(f))
        for diag in unit.diagnostics:
            print('   {}'.format(diag))
            return

    if ignore_ids:
        locnames = None
    else:
        locnames = set()
        collect_local_names(locnames, unit.root)
    codes = encode(f, locnames, unit.root)
    find_copy_pastes(codes, num_hash_limit=size_min, num_line_limit=size_min)


def do_files(files):
    """
    Analyze a list of files. Issue messages on longer copy-pastes, either
    inside the same file, or between different files.
    """
    contexts = [(f, lal.AnalysisContext()) for f in files]
    units = [(f, c.get_from_file(f)) for (f, c) in contexts]

    # For the analysis of multiple files, issue a message for files that are
    # not parsable, and proceed with others.
    for (f, unit) in units:
        if unit.root is None:
            print('Could not parse {}:'.format(f))
            for diag in unit.diagnostics:
                print('   {}'.format(diag))

    units = [(f, unit) for (f, unit) in units if unit.root is not None]
    # Intersperse the codes of each file with a "code" using a different
    # integer as hash (starting from 0 and counting upwards), which serves as a
    # likely unique terminator, to avoid detecting copy-pastes that would start
    # on a file and end up in another.
    codes = []
    for i, (f, unit) in enumerate(units):
        if ignore_ids:
            locnames = None
        else:
            locnames = set()
            collect_local_names(locnames, unit.root)
        codes += encode(f, locnames, unit.root) + [Code(i, unit.root, f)]

    find_copy_pastes(codes, num_hash_limit=size_min, num_line_limit=size_min)


def do_directory(d):
    """
    Analyze a directory. Issue messages on longer copy-pastes on files with
    extension '.adb' inside the directory, either inside the same file, or
    between different files.
    """
    acc = []
    for root, dirs, files in os.walk(d):
        acc += [os.path.join(root, f) for f in files if f.endswith(".adb")]
    do_files(acc)


def main(args):
    global debug, size_min, ignore_ids
    debug = args.debug
    size_min = args.size_min
    ignore_ids = args.ignore_ids

    if len(args.files) == 1:
        f = args.files[0]
        if os.path.isfile(f):
            do_file(f)
        else:
            do_directory(f)
    else:
        do_files(args.files)


if __name__ == '__main__':
    main(parser.parse_args())
