TODO LIST - Libadalang
======================

* Handle file resolution and dependencies.
* Create the high level public API - notions of TranslationContext and TranslationUnit.
* Renaming work (folders and files). Do that together.
* Code generation templates : Use a context instead of dirty locals + globals hack.
* Implement memory management of ASTNodes - DONE
* Implement full ada grammar (lol) - DONE !!
* Refactor the parsing lib to store and move around a tokenizer offset rather
  than a whole object DONE
* Replace the tokenizer by something more efficient (C generated tokenizer
?) DONE
* Implement packrat style memoization DONE
* Make an opt object with tk_start/tk_end to handle cases where an AST Node
  start with optional tokens.
* Propagate tk_start/tk_end for optional components
* Make a list node for list components to keep track of start/end
