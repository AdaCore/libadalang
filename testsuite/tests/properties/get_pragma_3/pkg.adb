package body Pkg is
   procedure Foo (X : Integer) is null;
   --% node.p_get_pragma("Inline")
   --
   pragma Inline (Foo);
   --% node.p_associated_decls

   procedure Foo is null;
end Pkg;
