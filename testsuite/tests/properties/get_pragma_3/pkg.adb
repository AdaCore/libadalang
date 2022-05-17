package body Pkg is
   procedure Foo (X : Integer) is null;
   --% node.p_get_pragma("Inline")
   --
   pragma Inline (Foo);
   --% node.p_associated_entities

   procedure Foo is null;

   function GF_Function return Integer is (0);
   --% node.p_get_pragma("inline")

   function F_Function is new GF_Function;
   --% node.p_get_pragma("inline")

   pragma Inline (F_Function);
   --% node.p_associated_entities
end Pkg;
