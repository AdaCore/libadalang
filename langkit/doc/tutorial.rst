********
Tutorial
********


Introduction
============

If you are completely new to Langkit, this tutorial is for you! It will run
through the implementation of an analysis library for a simple language and
will go further until actually using the generated library as a Python module
to implement an interpreter for this language. This should provide you a decent
background about how to deal with Langkit at every step of the pipeline.

Little disclaimer, though: this tutorial is intended for people with zero
experience with Langkit but a reasonable knowledge of how compilers work (what
a lexer is, what a parser is, what semantic analysis means, etc.). Being
comfortable with the Python programming language will be useful as well.

We will focus on a very simple language for the purpose of this tutorial:
Kaleidoscope, which is defined and used in a `LLVM tutorial
<http://llvm.org/docs/tutorial/index.html>`_.


Setup
=====

First, please make sure that the ``langkit`` Python package is available in
your Python environment (i.e. that Python scripts can import it). Also, please
install:

  * a GNAT toolchain: the generated library uses the Ada programming language,
    so you need to be able to build Ada source code;

  * `GNATcoll <http://docs.adacore.com/gnatcoll-docs/>`_, an Ada library
    providing various utilities;

  * `Quex <http://sourceforge.net/projects/quex/files/HISTORY/0.64/>`_ (a lexer
    generator), version 0.64.8;

  * Mako, a template system for Python which should already be installed if you
    used ``setup.py/easy_install/pip/...`` to install Langkit.


Getting started
===============

Alright, so having to copy-paste files in order to start something is quite
boring: let's use a script that will do this for us! Move to a working
directory and run:

.. code-block:: text

    $ create-project.py Kaleidoscope

This will create a ``kaleidoscope`` directory, a dummy language specification
(lexer and parser) as well as a ``manage.py`` script that will help you to
generate and build your analysis library. Let's step into it:

.. code-block:: text

    $ cd kaleidoscope

And check that this skeleton already builds:

.. code-block:: text

    $ ./manage.py make

This should generate and then build the analysis library in the ``build`` local
directory. Check in particular:

  * ``build/include`` and ``build/lib``, which contain the Ada sources, C
    header files and static/shared libraries for the generated library;

  * ``build/bin``, which contains a ``parse`` binary, useful to easily run the
    lexer/parser from the command line; note that it is statically linked with
    the generated library to ease debugging and testing (you don't have to add
    ``build/lib`` directory to your ``LD_LIBRARY_PATH``);

  * ``build/python``, which contains the Python binding for the generated
    library.

If everything went fine, you should be able to run the ``parse`` test binary:

.. code-block:: text

    $ build/bin/parse
    Parsing failed:
    Line 1, column 1: Expected "Example", got "Termination"
    <null node>

Great! This binary just tries to parse its command-line argument and displays
the resulting AST. The dummy language specification describes a language that
allows exactly one "example" keyword:

.. code-block:: text

    $ build/bin/parse example
    ExampleNode[1:1-1:8]

Here, we have an ExampleNode which spans from line 1, column 1 to line 1,
column 8.  This language is pretty useless but now we checked that the setup
was working, let's implement Kaleidoscope!


Lexing
======

We are about to start with the most elementary piece of code that will handle
our language: the lexer!  Also known as a scanner, a lexer will take a stream
of text (i.e.  your source files) and split it into *tokens* (or *lexems*),
which are kind of "words" for programming languages. Langkit relies on Quex to
generate an efficient lexer but hides you the gory details and let you just
write a Python description for the lexer. Fire up your favorite code editor and
open ``language/lexer.py``.

This module contains three blocks:

  * an import statement, which pulls all the objects we need to build our lexer
    from Langkit;

  * a ``Token`` class definition, used to define both the set of token kinds
    that the lexer will produce and what to do with them (more on that below);

  * the instantiation of a lexer in ``kaleidoscope_lexer`` and adding two
    lexing rules for it (more on that farther below).

So let's first talk about token kinds. The tokens most lexers yield have a kind
that determines what kind of word they represent: is it an identifier? an
integer literal? a keyword? The parser then relies on this token kind to decide
what to do with it. But we also use the token kind in order to decide whether
we keep the text associated to it and if we do, how to store it.

For instance we generally keep identifiers in symbol tables so that we can
compare them efficiently (no string comparison, just a pointer equality, for
example) and allocate memory for the text only once: identical identifiers will
reference the memory chunk. On the other hand, string literals are almost
always unique and thus are not good candidates for symbol tables.

In Langkit, we declare the list of token kinds subclassing the ``LexerToken``
class.

::

    class Token(LexerToken):
        Example    = NoText()

        # Keywords
        Def        = NoText()
        Extern     = NoText()

        # Other alphanumeric tokens
        Identifier = WithSymbol()
        Number     = WithText()

        # Punctuation
        LPar       = NoText()
        RPar       = NoText()
        Comma      = NoText()
        Colon      = NoText()

        # Operators
        Plus       = NoText()
        Minus      = NoText()
        Mult       = NoText()
        Div        = NoText()

Ok, so here we have four kind of tokens:

  * The ``def`` and ``extern`` keywords, for which keeping the text is useless:
    there is only one possible ``def`` keyword (same for ``external``) so
    copying the text for it gives no useful information. We use ``NoText``
    instances to achieve this.

  * Identifiers, which we'll use for function names and variable names so we
    want to put the corresponding text in a symbol table. We use ``WithSymbol``
    insances to achieve this.

  * Decimal literals (``Number``), for which we will keep the associated text
    so we can later extract the corresponding value later. We use ``WithText``
    instances to achieve this.

  * Punctuation and operators, for which keeping the text is useless, just like
    for keywords.

Do not forget to add ``WithText`` and ``WithSymbol`` to the import statement so
that you can use them in your lexer specification.

Good, so now let's create the lexer itself.  The first thing to do is to
instantiate the ``Lexer`` class and provide it the set of available tokens:

::

    kaleidoscope_lexer = Lexer(Token)

Then, the only thing left to do is to add lexing rules to match text and
actually yield Tokens. This is done using our lexer's ``add_rules`` method:

::

    kaleidoscope_lexer.add_rules(
        (Pattern(r"[ \t\r\n]+"),                        Ignore()),
        (Pattern(r"#.*"),                               Ignore()),

        (Literal("def"),                                Token.Def),
        (Literal("extern"),                             Token.Extern),
        (Pattern(r"[a-zA-Z][a-zA-Z0-9]*"),              Token.Identifier),
        (Pattern(r"([0-9]+)|([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+)"), Token.Number),

        (Literal("("),                                  Token.LPar),
        (Literal(")"),                                  Token.RPar),
        (Literal(","),                                  Token.Comma),
        (Literal(";"),                                  Token.Colon),

        (Literal("+"),                                  Token.Plus),
        (Literal("-"),                                  Token.Minus),
        (Literal("*"),                                  Token.Mult),
        (Literal("/"),                                  Token.Div),
    )

This kind of construct is very analog to what you can find in other lexer
generators such as ``flex``: on the left you have what text to match and on the
right you have what should be done with it:

  * The first ``Pattern`` matches any blank character and discards them, thanks
    to the ``Ignore`` action.

  * The second one discards comments (everything starting with ``#`` until the
    end of the line).

  * The two ``Literal`` matchers hit on the corresponding keywords and
    associate the corresponding token kinds.

  * Two two last ``Pattern`` will respectively match identifiers and numbers, and
    emit the corresponding token kinds.

Only exact input strings trigger ``Literal`` matchers while the input is
matched against a regular expression with ``Pattern`` matchers. Note that the
order of rules is meaningful: here, the input is matched first against keywords
and then only if there is no match, identifers and number patterns are matched.
If ``Literal`` rules did appear at the end, ``def`` would always be emitted
as an identifier.

In both the token kinds definition and the rules specification above, we kept
handling for the ``example`` token in order to keep the parser happy (it still
references it). You will be able to get rid of it once we took care of the
parser.

Alright, let's see how this affects our library. As for token kind definitions,
don't forget to import ``Pattern`` and ``Ignore`` from ``langkit.lexer`` and
then re-build the library.

Before our work, only ``example`` was accepted as an input, everything else was
rejected by the lexer:

.. code-block:: text

    $ build/bin/parse def
    Parsing failed:
    Line 1, column 1: Expected "Example", got "LexingFailure"
    <null node>

Now, you should get this:

.. code-block:: text

    Parsing failed:
    Line 1, column 1: Expected "Example", got "Def"
    <null node>

The parser is still failing but that's not a surprise since we only took care
of the lexer so far. What is interesting is that we see thanks to ``"Def"``
that the lexer correctly turned the ``def`` input text into a ``Def`` token.
Let's check with numbers:

.. code-block:: text

    $ build/bin/parse 0
    Parsing failed:
    Line 1, column 1: Expected "Example", got "Number"
    <null node>

Looking good! Lexing seems to work, so let's get the parser working.


AST and Parsing
===============

The job of parsers is to turn a stream of tokens into an AST (Abstract Syntax
Tree), which is a representation of the source code making analysis easier. Our
next task will be to actually define how our AST will look like so that the
parser will know what to create.

Take your code editor, open ``language/parser.py`` and replace the ``Example``
class definition with the following ones:

::

    class Function(ASTNode):
        proto = Field()
        body  = Field()

    class ExternDecl(ASTNode):
        proto = Field()

    class Prototype(ASTNode):
        name = Field()
        args = Field()

    @abstract
    class Expr(ASTNode):
        pass

    class Number(Expr):
        value = Field()

    class Identifier(Expr):
        name = Field()

    class Operator(EnumType):
        alternatives = ['plus', 'minus', 'mult', 'div']

    class BinaryExpr(Expr):
        lhs = Field()
        op = Field()
        rhs = Field()

    class CallExpr(Expr):
        callee = Field()
        args = Field()

As usual, new code comes with its new dependencies: also complete the
``langkit.compiled_types`` import statement with ``abstract``, ``EnumType`` and
``Field``.

Each class definition is a way to declare how a particular AST node will look.
Think of it as a kind of structure: here the ``Function`` AST node has two
fields: ``proto`` an ``body``. Note that unlike most AST declarations out
there, we did not associate types to the fields: this is expected as we will
see later.

Some AST nodes can have multiple forms: for instance, an expression can be
a number or a binary operation (addition, subtraction, etc.) and in each case
we need to store different information in them: in the former we just need the
number value whereas in binary operations we need both members of the additions
(``lhs`` and ``rhs`` in the ``BinaryExpr`` class definition above) and the kind
of operation (``op`` above). The strategy compiler writers sometimes adopt is
to use inheritance (as in `OOP
<https://en.wikipedia.org/wiki/Object-oriented_programming>`_) in order to
describe such AST nodes: there is an abstract ``Expr`` class while the
``Number`` and ``BinaryExpr`` are concrete classes deriving from it.

This is exactly the approach that Langkit handles: all "root" AST nodes derive
from the ``ASTNode`` class, and you can create abstract classes (using the
``abstract`` class decorator) to create a hierarchy of node types.

Careful readers may also have spotted something else: the ``Operator``
enumeration type. We use an enumeration type in order to store in the most
simple way what kind of operation a ``BinaryExpr`` represents. As you can see,
creating an enumeration type is very easy: just subclass ``EnumType`` and set
the ``alternative`` field to a sequence of strings that will serve as
identifiers for the enumeration values (also called *enumerators*).

Fine, we have our data structures so now we shall use them! In order to create
a parser, Langkit requires you to describe a grammar, hence the ``Grammar``
instantiation already present in ``parser.py``. Basically, the only thing you
have to do with a grammar is to ada *rules* to it: a rule is a kind of
sub-parser, in that it describes how to turn a stream of token into an AST.
Rules can reference each other recursively: an expression can be a binary
operator, but a binary operator is itself composed of expressions! And in order
to let the parser know how to start parsing you have to specify an entry rule:
this is the ``main_rule_name`` field of the grammar (currently set to
``'main_rule'``).

Langkit generates recursive descent parsers using `parser combinators
<https://en.wikipedia.org/wiki/Parser_combinator>`_. Here are a few fictive
examples:

  * ``'def'`` matches exactly one ``def`` token;
  * ``Row('def', Tok(Token.Identifier))`` matches a ``def`` token followed by
    an identifier token.
  * ``Or('def', 'extern')`` matches either a ``def`` keyword, either a
    ``extern`` one (no more, no less).

The basic idea is that you use the callables Langkit provides (``Row``, ``Or``,
etc.) in order to compose in a quite natural way what rules can match. Let's
move forward with a real world example: Kaleidoscope! Each chunk of code below
appears as a keyword argument of the ``add_rules`` method invocation (you can
remove the previous ``main_rule`` one).

::

    main_rule=List(Or(G.extern_decl, G.function, G.expr)),

Remember that ``G`` is another name for ``kaleidoscope_grammar``, so that it's
shorter to write/read here.  ``G.external_decl`` references the parsing rule
called ``external_decl``.  It does not exist yet, but Langkit allows such
forward references anyway so that rules can reference themselves in a recursive
fashion.

So what this rule matches is a list in which elements can be either external
declarations, function definitions or expressions.

::

    extern_decl=Row('extern', G.prototype) ^ ExternDecl,

This one is interersting: the ``Row`` part matches the ``extern`` keyword
followed by what the ``prototype`` rule matches. Then, what the ``^
ExternDecl`` part does is to take what the ``Row`` part matched and create an
``ExternDecl`` AST node to hold the result.

... but how is that possible? We saw above that ``ExternDecl`` has only one
field, whereas the ``Row`` part matched two items. The trick is that by
default, mere tokens are discarded.  Once it's discarded, the only thing left
is what ``prototype`` matched, and so there is exactly one result to put in
``ExternDecl``.

In Langkit, the human-friendly name for ``^`` is the *transform* operator.  On
the left side it takes a sub-parser while on the right side it takes a concrete
ASTNode subclass that must have the same number of fields as the number of
results the sub-parser yields (i.e. one for every sub-parser except ``Row`` and
the number of non-discarded items in ``Row`` sub-parsers).  

::

    function=Row('def', G.prototype, G.expr) ^ Function,

We have here a pattern that is very similar to ``extern_decl``, expect that the
``Row`` part has two non-discarded results: ``prototype`` and ``expr``.  This
is fortunate, as the ``Function`` ASTNode requires two fields.

::

    prototype=Row(G.identifier, '(',
                  List(G.identifier, sep=',', empty_valid=True),
                  ')') ^ Prototype,

The only new bit in this rule is how the ``List`` combinator is used: last
time, it had only one parameter: a sub-parser to specify how to match
individual list elements. Here, we also have a ``sep`` argument to specify that
a comma token must be present between each list item and the ``empty_valid``
argument tells ``List`` that it is valid for the parsed list to be empty (it's
not allowed by default).

So our argument list has commas to separate arguments and we may have functions
that take no argument.

::

    expr=Or(
        Row('(', G.expr, ')')[1],
        Row(G.expr,
            Or(Enum('+', Operator('plus')),
               Enum('-', Operator('minus'))),
            G.prod_expr
        ) ^ BinaryExpr,
        G.prod_expr,
    ),

Let's dive into the richest grammatical element of Kaleidoscope: expressions!
An expression can be either:

  * A sub-expression nested in parenthesis, to give users more control over how
    associativity works. Note that we used here the subscript operation to
    extract the middle result (first one is at index 0, middle one is at index
    1) of the ``Row`` part.

  * Two sub-expressions with an operator in the middle, building a binary
    expression. This shows how we can turn tokens into enumerators:

    ::

        Enum('+', Operator('plus'))

    This matches a ``+`` token (``Plus`` in our lexer definition) and yields
    the ``plus`` enumerator from the ``Operator`` enumeration type.

  * The ``prod_expr`` kind of expression: see below.

::

    prod_expr=Or(
        Row(G.prod_expr,
            Or(Enum('*', Operator('mult')),
               Enum('/', Operator('div'))),
            G.call_or_single
        ) ^ BinaryExpr,
        G.call_or_single,
    ),

This parsing rule is very similar to ``expr``: except for the parents
sub-rule, the difference lies in which operators are allowed there: ``expr``
allowed only sums (plus and minus) whereas this one allows only products
(multiplication and division). ``expr`` references itself everywhere except for
the right-hand-side of binary operations and the "forward" sub-parser: it
references the ``prod_expr`` rule instead. On the other hand, ``prod_expr``
references itself everywhere with the same exceptions.  This layering pattern
is used to deal with associativity in the parser: going into details of parsing
methods is not the purpose of this tutorial buf fortunately there are many
articles that explain `how this works
<https://www.google.fr/search?q=recursive+descent+parser+associativity>`_ (just
remember that: yes, Langkit handles left recursivity!).

::

    call_or_single=Or(
        Row (G.identifier, '(',
             List(G.expr, sep=',', empty_valid=True),
             ')') ^ CallExpr,
        G.identifier,
        G.number,
    ),

Well, this time there is nothing new. Moving on to the two last rules...

::

    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    number=Tok(Token.Number, keep=True) ^ Number,

Until now, the parsing rules we wrote only used string literals to match
tokens. While this works for things like keywords, operators or punctuation, we
cannot match a token kind with no specific text associated this way and
besides, here we need to *keep* the text associated to the tokens. So these
rules use instead the ``Tok`` combinator, which takes a token from your
``language.lexer.Token`` class (don't forget to import it!) and which has a
``keep`` argument that enables us to keep the token so that transform operators
can store them in our AST... which is what both rules do right after the
``Tok`` returns.

Until now, we completely put aside types in the AST: fields were declared
without associated types. However, in order to generate the library, someone
*has* to take care of assigning definite type to them. Langkit uses for that a
`type inference <https://en.wikipedia.org/wiki/Type_inference>`_ algorithm
which deduces types automatically from how AST nodes are used in the grammar.
For instance, doing the following (fictive example):

::

    Enum('sometok', SomeEnumeration('someval')) ^ SomeNode

Then the typer will know that the type of the SomeNode's only field is the
``SomeEnumeration`` type.

Our grammar is complete, for a very simple version of the Kaleidoscope
language! If you have dealt with Yacc-like grammars before, I'm sure you'll
find this quite concise, especially considering that it covers both parsing and
AST building.

Let's check with basic examples if the parser works as expected. First, we have
to launch another build and then run ``parse`` on some code:

.. code-block:: text

    $ ./manage.py make
    [... snipped...]

    $ build/bin/parse 'extern foo(a); def bar(a, b) a * foo(a + 1)'
    ExternDecl[1:1-1:15]
    | proto:
    | | Prototype[1:8-1:14]
    | | | name:
    | | | | Identifier[1:8-1:11]
    | | | | | name: foo
    | | | args:
    | | | | Identifier[1:12-1:13]
    | | | | | name: a
    Function[1:16-1:44]
    | proto:
    | | Prototype[1:20-1:29]
    | | | name:
    | | | | Identifier[1:20-1:23]
    | | | | | name: bar
    | | | args:
    | | | | Identifier[1:24-1:25]
    | | | | | name: a
    | | | | Identifier[1:27-1:28]
    | | | | | name: b
    | body:
    | | BinaryExpr[1:30-1:44]
    | | | lhs:
    | | | | Identifier[1:30-1:31]
    | | | | | name: a
    | | | op: mult
    | | | rhs:
    | | | | CallExpr[1:34-1:44]
    | | | | | callee:
    | | | | | | Identifier[1:34-1:37]
    | | | | | | | name: foo
    | | | | | args:
    | | | | | | BinaryExpr[1:38-1:43]
    | | | | | | | lhs:
    | | | | | | | | Identifier[1:38-1:39]
    | | | | | | | | | name: a
    | | | | | | | op: plus
    | | | | | | | rhs:
    | | | | | | | | Number[1:42-1:43]
    | | | | | | | | | value: 1

Yey! What a pretty AST! Here's also a very useful tip for grammar development:
it's possible to run ``parse`` on rules that are not the main ones. For
instance, imagine we want to test only the ``expr`` parsing rule: you just
have to use the ``-r`` argument to specify that we want the parser to start
with it:

.. code-block:: text

    $ build/bin/parse -r expr '1 + 2'
    BinaryExpr[1:1-1:6]
    | lhs:
    | | Number[1:1-1:2]
    | | | value: 1
    | op: plus
    | rhs:
    | | Number[1:5-1:6]
    | | | value: 2

So we have our analysis library: there's nothing more we can do right now to
enhance it, but on the other hand we can already use it to parse code and get
AST's.


Using the generated library's Python API
========================================

The previous steps of this tutorial led us to generate an analysis library for
the Kaleidoscope language. That's cool, but what would be even cooler would be
to use this library. So what about writing an interpreter for Kaleidoscope
code?

Kaleidoscope interpreter
------------------------

At the moment, the generated library uses the Ada programming language and its
API isn't stable yet. However, it also exposes a C API and a Python one on the
top of it. Let's use the Python API for now as it's more concise, handier and
likely more stable. Besides, using the Python API makes it really easy to
experiment since you have an interactive interpreter. So, considering you
successfully built the library with the Kaleidoscope parser and lexer, make
sure the ``build/lib`` directory is in your ``LD_LIBRARY_PATH`` (on Unix, adapt
for Windows) and that the ``build/python/libkaleidoscopelang.py`` is reachable
from Python (check ``PYTHONPATH``).

Alright, so the first thing to do with the Python API is to import the
``libkaleidoscopelang`` module and instantiate an analysis context from it:

::

    import libkaleidoscopelang as lkl
    ctx = lkl.AnalysisContext()

Then, we can parse code in order to yield ``AnalysisUnit`` objects, which
contain the AST. There are two ways to parse code: parse from a file or parse
from a buffer (i.e. a string value):

::

    # Parse code from the 'foo.kal' file.
    unit_1 = ctx.get_from_file('foo.kal')

    # Parse code from a buffer as if it came from the 'foo.kal' file.
    unit_2 = ctx.get_from_buffer('foo.kal', 'def foo(a, b) a + b')

.. todo::

    When diagnostics bindings in Python will become more convenient (useful
    __repr__ and __str__), talk about them.

The AST is reachable thanks to the ``root`` attribute in analysis units: you
can then browse the AST nodes programmatically:

.. code-block:: python

    # Get the root AST node.
    print unit_2.root
    # <libkaleidoscopelang.ASTList object at 0x7f09dc905bd0>

    unit_2.dump()
    # <list>
    # |item 0:
    # |  <FunctionNode>
    # |  |proto:
    # ...

    print unit_2.root[0]
    # <libkaleidoscopelang.FunctionNode object at 0x7f09dc905c90>

    print list(unit_2.root[0].iter_fields())
    # [('proto', <libkaleidoscopelang.Prototype object at 0x7f09dc905e10>),
    #  ('body', <libkaleidoscopelang.BinaryExpr object at 0x7f09dc905c50>)]

    print list(unit_2.root[0].f_body
    # <libkaleidoscopelang.BinaryExpr object at 0x7f09dc905c50>

Note how names for AST node fields got a ``f_`` prefix: this is used to
distinguish AST node fields from generic AST node attributes and methods, such
as ``iter_fields`` or ``sloc_range``. Similarly, the ``Function`` AST type was
renamed as ``FunctionNode`` so that the name does not clash with the
``function`` keyword in Ada in the generated library.

You are kindly invited to either skim through the generated Python module or
use the ``help(...)`` built-in in order to discover how you can explore trees.

Alright, let's start the interpreter, now! First, let's declare an
``Interpreter`` class and an ``ExecutionError`` exception:

::

    class ExecutionError(Exception):
        def __init__(self, sloc_range, message):
            self.sloc_range = sloc_range
            self.message = message


    class Interpreter(object):
        def __init__(self):
            # Mapping: function name -> FunctionNode instance
            self.functions = {}

        def execute(self, ast):
            pass # TODO

        def evaluate(self, node, env=None):
            pass # TODO

Our interpreter will raise an ``ExecutionError`` each time the Kaleidoscope
program does something wrong. In order to execute a script, one has to
instantiate the ``Interpreter`` class and to invoke its ``execute`` method
passing it the parsed AST. Then, evaluating any expression is easy: just invoke
the ``evaluate`` method passing it an ``Expr`` instance.

Our top-level code looks like this:

::

    def print_error(filename, sloc_range, message):
        line = sloc_range.start.line
        column = sloc_range.start.column
        print >> sys.stderr, 'In {}, line {}:'.format(filename, line)
        with open(filename) as f:
            # Get the corresponding line in the source file and display it
            for _ in range(sloc_range.start.line - 1):
                f.readline()
            print >> sys.stderr, '  {}'.format(f.readline().rstrip())
            print >> sys.stderr, '  {}^'.format(' ' * (column - 1))
        print >> sys.stderr, 'Error: {}'.format(message)


    def execute(filename):
        ctx = lkl.AnalysisContext()
        unit = ctx.get_from_file(filename)
        if unit.diagnostics:
            for diag in unit.diagnostics:
                print_error(filename, diag.sloc_range, diag.messegae)
            sys.exit(1)
        try:
            Interpreter().execute(unit.root)
        except ExecutionError as exc:
            print_error(filename, exc.sloc_range, exc.message)
            sys.exit(1)

Call ``execute`` with a filename and it will:

 1. parse the corresponding script;
 2. print any lexing/parsing error (and exit if there are errors);
 3. interpret it (and print messages from execution errors).

The ``print_error`` function is a fancy helper to nicely show the user where
the error occurred. Now that the framework is ready, let's implement the
important bits in ``Interpreter``:

::

    # Method for the Interpreter class
    def execute(self, ast):
        assert isinstance(ast, lkl.ASTList)
        for node in ast:
            if isinstance(node, lkl.FunctionNode):
                self.functions[node.f_proto.f_name.f_name.text] = node

            elif isinstance(node, lkl.ExternDecl):
                raise ExecutionError(
                    node.sloc_range,
                    'External declarations are not supported'
                )

            elif isinstance(node, lkl.Expr):
                print self.evaluate(node)

            else:
                # There should be no other kind of node at top-level
                assert False

Nothing really surprising here: we browse all top-level grammatical elements
and take different decisions based on their kind: we register functions,
evaluate expressions and complain when coming across anything else (i.e.
external declarations: given our grammar, it should not be possible to get
another kind of node).

Also note how we access text from tokens: ``node.f_proto.f_name.f_name`` is a
``libkaleidoscope.Token`` instance, and its text is available through the
``text`` attribute. Our AST does not contain any, but if you had tokens without
text (remember, it's the lexer declaration that decides whether we keep text or
not for each specific token), the ``text`` attribute would return ``None``
instead.

Now comes the last bit: expression evaluation.

::

    # Method for the Interpreter class
    def evaluate(self, node, env=None):
        if env is None:
            env = {}

        if isinstance(node, lkl.Number):
            return float(node.f_value.text)

        elif isinstance(node, lkl.Identifier):
            try:
                return env[node.f_name.text]
            except KeyError:
                raise ExecutionError(
                    node.sloc_range,
                    'Unknown identifier: {}'.format(node.f_name.text)
                )

This first chunk introduces how we deal with "environments" (i.e. how we
associate values to identifiers). ``evaluate`` takes an optional parameter
which is used to provide an environment to evaluate the expression. If the
expression is allowed to reference the ``a`` variable, which contains ``1.0``,
then ``env`` will be ``{'a': 1.0}``.

Let's continue: first add the following declaration to the ``Interpreter``
class:

::

    # Mapping: enumerators for the Operator type -> callables to perform the
    # operations themselves.
    BINOPS = {'plus':  lambda x, y: x + y,
              'minus': lambda x, y: x - y,
              'mult':  lambda x, y: x * y,
              'div':   lambda x, y: x / y}

Now, we can easily evaluate binary operations. Get back to the ``evaluate``
method definition and complete it with:

.. code-block:: python

        elif isinstance(node, lkl.BinaryExpr):
            lhs = self.evaluate(node.f_lhs, env)
            rhs = self.evaluate(node.f_rhs, env)
            return self.BINOPS[node.f_op](lhs, rhs)

Yep: in the Python API, enumerators appear as strings. It's the better tradeoff
we found so far to write concise code while avoiding name clashes: this works
well even if multiple enumeration types have homonym enumerators.

And finally, the very last bit: function calls!

.. code-block:: python

        elif isinstance(node, lkl.CallExpr):
            name = node.f_callee.f_name.text
            try:
                func = self.functions[name]
            except KeyError:
                raise ExecutionError(
                    node.f_callee.sloc_range,
                    'No such function: "{}"'.format(name)
                )
            formals = func.f_proto.f_args
            actuals = node.f_args

            # Check that the call is consistent with the function prototype
            if len(formals) != len(actuals):
                raise ExecutionError(
                    node.sloc_range,
                    '"{}" expects {} arguments, but got {} ones'.format(
                        node.f_callee.f_name.text,
                        len(formals), len(actuals)
                    )
                )

            # Evaluate arguments and then evaluate the call itself
            new_env = {f.f_name.text: self.evaluate(a, env)
                       for f, a in zip(formals, actuals)}
            result = self.evaluate(func.f_body, new_env)
            return result

        else:
            # There should be no other kind of node in expressions
            assert False

Here we are! Let's try this interpreter on some "real-world" Kaleidoscope code:

.. code-block:: text

    def add(a, b)
      a + b

    def sub(a, b)
      a - b

    1
    add(1, 2)
    add(1, sub(2, 3))

    meh()

Save this to a ``foo.kal`` file, for instance, and run the interpreter:

.. code-block:: text

    $ python kalrun.py foo.kal
    1.0
    3.0
    0.0
    In foo.kal, line 11:
      meh()
      ^
    Error: No such function: "meh"

Congratulations, you wrote an interpreter with Langkit! Enhancing the lexer,
the parser and the interpreter to handle fancy language constructs such as
conditionals, more data types or variables is left as an exercise for the
readers! ;-)

.. todo::

    When the sub-parsers are exposed in the C and Python APIs, write the last
    part to evaluate random expressions (not just standalone scripts).

Kaleidoscope IDE support
------------------------

.. todo::

    When we can use trivia as well as semantic requests from the Python API,
    write some example on, for instance, support for Kaleidoscope in GPS
    (highlighting, blocks, cross-references).
