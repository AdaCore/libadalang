****************
Extension points
****************

Langkit tries to provide as much functionality as possible as part of the DSL.
Ideally, you could specify every feature you want in your library via the DSL.

However, for various reasons (time, money, scope, etc.), this goal will remain
an ideal that we want to reach, while being aware of the fact that we probably
won't ever reach it.

To provide a pragmatic way for the user to be able to implement every
functionality he needs *as part of the library itself*, we added the extension
points feature.

High level overview
===================

At the top level directory of your language definition, Langkit will search for
an ``extension`` directory, and will search for extensions inside it. It will
use the directory structure and file names to determine the purpose of each
extension file.

Existing extensions points
==========================

* ``extensions/support``: Every file in this folder needs to be a valid ada spec
  + body pair.

* ``extensions/nodes/$ast_node_name/components``: For every subclass of ASTNode
  that you created in your project, you can add this file to add components to
  the ast node subclass record.
