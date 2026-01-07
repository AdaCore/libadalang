with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   package Pkg is
      type T is tagged record
         I : Integer;
      end record
        with First_Controlling_Parameter;

      function Foo (X : T) return T is ((I => 1));
      function Bar (X : T) return Integer is (X.I);
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;

      function Foo (X : U) return U is ((I => 2));
      function Bar (X : U) return Integer is (-X.I);
   end Der;

   X : Pkg.T'Class := Der.U'((I => 1));
   Y : Integer;
begin
   Y := X.Foo.Bar;
   --  Since T is marked First_Controlling_Parameter, Foo is only dispatching
   --  in the first parameter. This means it does not have a controlling result
   --  even though the primitive type appears in the return type. This implies
   --  that the call X.Foo is dispatching but is not dynamically tagged, and
   --  thus calling Bar on it is not dispatching.
   --% node.f_expr.f_prefix.p_is_dispatching_call()
   --% node.f_expr.p_is_dispatching_call()
   Put_Line (Y'Image);
end Test;
