with Bar;

function Foo return Boolean
   renames Bar --% node.p_referenced_decl()
   ;
