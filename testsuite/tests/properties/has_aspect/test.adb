procedure Test is
   A : constant Boolean := True;
   B : constant Boolean := False;
   Pi : constant := 3.14;

   procedure Pouet0 is null;
   --% node.p_has_aspect('inline')

   procedure Pouet1 is null
      with Inline;
   --% node.p_has_aspect('inline')

   procedure Pouet2 is null
      with Inline => A and B;
   --% node.p_has_aspect('inline')

   procedure Pouet3 is null
      with Inline => A or B;
   --% node.p_has_aspect('inline')

   procedure Pouet4 is null
      with Inline => True;
   --% node.p_has_aspect('inline')

   procedure Pouet5 is null
      with Inline => False;
   --% node.p_has_aspect('inline')

   type Small is record
      A, B : Character;
   end record;
   --% node.p_has_aspect('size')
   for Small'Size use 16;

   type Angle is delta Pi range -Pi .. Pi;
   --% node.p_has_aspect('small')
   for Angle'Small use 0.001;

   package P is
      type T is tagged private;

      function Foo (X : T) return Boolean is (True);
      function Bar (X : T) return Integer is (42)
         with Pre => Foo (X),
              Pre'Class => Foo (X);
      --% node.p_has_aspect("pre")
      --% node.p_has_aspect("pre'class")

   private
      type T is tagged null record with Invariant => Foo (T);
      --% node.p_has_aspect("invariant")
   end P;

   Op : access function (Arg1 : Integer; Arg2 : Integer) return Integer
     with Import => True,
       Convention => C,
       External_Name => "op";
   --% node.p_has_aspect("convention")
begin
   null;
end Test;
