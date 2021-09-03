with Ghost_Library_Pkg.Child;

procedure Ghost_Code is

   function Foo return Integer is (12) with Ghost;

   type Rec is record
      A : Integer;
      B : Integer;
   end record with Ghost;

   R : Rec := (12, Foo) with Ghost;
   D : Integer with Ghost;

   package Ghost_Pkg with Ghost is
      A : Integer := 12;
   end Ghost_Pkg;

   package body Ghost_Pkg is
   begin
      A := 15;
      pragma Assert (True);
      pragma Assume (True);
      loop
         pragma Loop_Invariant (True);
      end loop;
   end Ghost_Pkg;
begin
   R := (16, 18);
   D := Foo;
   Ghost_Library_Pkg.Child.A := 12;
end Ghost_Code;
