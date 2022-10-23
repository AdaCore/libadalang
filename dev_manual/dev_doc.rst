************************
Developer documentation
************************

Libadalang is powered by `Langkit <https://github.com/AdaCore/langkit/>`_, so
it consists only of a language description and several helpers that Langkit
compiles into an Ada library with its various language bindings.

So for developers, the gist of Libadalang lies in three files in this
repository:

* ``ada/language/lexer.py`` contains the lexer description for Ada. It will let
  Langkit emit code that will turn text files into tokens.

* ``ada/language/grammar.py`` contains the parser description for Ada. Langkit
  will use it to produce a parser, to turn tokens into abstract syntax trees
  (AST).

* ``ada/language/ast.py`` contains the definition of all AST nodes (one for
  call expressions, one for IF blocks, etc.) and their syntax fields, plus code
  that implements the logic for name resolution (see below).

While those files are Python files, the language has only a marginal link to
Python: it is mostly a DSL implemented on top of Python's syntax, that compiles
down to Ada code. This compilation is the job of Langkit.

The AST node definition part in ``ast.py`` is itself composed of several
layers.  Each AST node is defined as a Python subclass of ``ASTNode`` (a
special class from Langkit). These subclasses contain various members:

* Syntax fields (``... = Field(...)``) which store other AST nodes; these are
  created at parsing time.

* Environment specifications (``env = EnvSpec(...)``) which define how to
  construct lexical environments. This describes what is done when one calls
  ``Populate_Lexical_Env``: the tree is visited and the env specs of all nodes
  are executed.

* Properties (``... = Property(...)`` or ``@langkit_property``). These are like
  C++ methods, or Ada tagged type primitives, in a dedicated programming
  language: the properties DSL.  This DSL provides features to build and solve
  logic equations: create logic variables tied to nodes, add possible values,
  add constraints on variables, etc. This DSL is documented in
  :langkit_doc:`Langkit's documentation <properties_dsl.html>`.

Name resolution is implemented with both environment specifications and the
properties DSL. Libadalang developers first define how lexical environments
are built, and then use the properties DSL to query them, build equations and
then solve equations: their solutions are the results of name resolution.

Note that the properties DSL is mostly functional. This fact gives us some
invariants on which to rely in order to handle memoization of results/data
invalidation, and so on.
