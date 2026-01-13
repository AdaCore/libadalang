with Ada.Text_IO; use Ada.Text_IO;

procedure Test_2 is
   package Pkg is
      type T is tagged record
         I : Integer;
      end record;

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
   --  Unlike the similar call in test.adb, the result of X.Foo is dynamically
   --  tagged because Foo has a controlling result. Thus, the call to Bar is
   --  dispatching.
   --% node.f_expr.f_prefix.p_is_dispatching_call()
   --% node.f_expr.p_is_dispatching_call()
   Put_Line (Y'Image);
end Test_2;
