TODO LIST - Libadalang
======================

Memory management
-----------------

* Need to be able to free/unref nodes recursively
  1. If the node is memoized, use unref
  2. Else use free

* This begs the question: We need to be able to implement this kind of
walkers easily. In c++ : node.visitchildren(free)
* Problem for the generic runtime c++ approach: Lists and Row types are not
ast nodes interface-wise, but can contain astnodes, so need to be walked.
* Let's say we have such a walker: Performance for free/unref ?
  * For free, no perf problem : All treated as pointers
  * For unref, if ref is a shared field in ASTNode, and all nodes are casted
   to ASTNode, unref will have no perf overhead either.

Others
------

* Implement full ada grammar (lol) - ~ 25% DONE BIATCH !!

* Refactor the parsing lib to store and move around a tokenizer offset rather
  than a whole object DONE

* Replace the tokenizer by something more efficient (C generated tokenizer
?) DONE

* Implement packrat style memoization DONE

* Make an opt object with tk_start/tk_end to handle cases where an AST Node
  start with optional tokens.
* Propagate tk_start/tk_end for optional components
* Make a list node for list components to keep track of start/end