with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   generic
      type T is tagged limited private;
      with procedure Prim (X : in out T) is abstract <>;
   package Gen is
      procedure Foo (X : in out T'Class);
   end Gen;

   package body Gen is
      procedure Foo (X : in out T'Class) is
      begin
         Prim (X);
         --% node.f_call.p_is_dispatching_call()
      end;
   end Gen;

   package Bar is
      type Root is tagged null record;

      procedure Prim (Self : in out Root);

      type Child is new Root with null record;

      overriding procedure Prim (Self : in out Child);
   end Bar;

   package body Bar is
      procedure Prim (Self : in out Root) is
      begin
         Put_Line ("Calling root");
      end Prim;

      procedure Prim (Self : in out Child) is
      begin
         Put_Line ("Calling child");
      end Prim;
   end Bar;

   use Bar;

   package Inst is new Gen (Root);
   --% ce = node.p_designated_generic_decl.p_body_part.find(lal.CallExpr)
   --% ce.p_is_dispatching_call()
   C : Child;
begin
   Inst.Foo (C);
end;

