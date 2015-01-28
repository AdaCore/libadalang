TODO LIST - Libadalang
======================

* Handle file resolution and dependencies.
* Create the high level public API - notions of TranslationContext and TranslationUnit.
* Store the symbol table someplace else. For the moment it is stored in the
  lexer, but in most cases we want to keep the symbol table and free the lexer.
