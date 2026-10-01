procedure Test is

   --  Check that `PragmaNode.p_get_aspect` returns an aspect with the expected
   --  "value" expression for contract pragmas.

   function Identity (I : Integer) return Integer is (I);
   --% node.p_get_aspect("Precondition")
   --% node.p_get_aspect("Postcondition")
   pragma Precondition (I in Integer);
   pragma Postcondition (I = Identity'Result);

   function Cartesian_Product (X, Y : Integer) return Integer is (X * Y);
   --% node.p_get_aspect("Contract_Cases")
   pragma Contract_Cases
     ((X >= 0 and then Y >= 0 => Cartesian_Product'Result >= 0,
       X >= 0 and then Y < 0  => Cartesian_Product'Result < 0,
       X < 0 and then Y >= 0  => Cartesian_Product'Result >= 0,
       others                 => Cartesian_Product'Result < 0));

   package Pkg is
      type T1 is private;
      --% node.p_get_aspect("Invariant")
      pragma Invariant (T1, True);

      type T2 is private;
      --% node.p_get_aspect("Type_Invariant")
      pragma Type_Invariant (T2, True);

      type T3 is tagged private;
      --% node.p_get_aspect("Type_Invariant_Class")
      pragma Type_Invariant_Class (T3, True);

      type T4 is private;
      --% node.p_get_aspect("Predicate")
      pragma Predicate (T4, True);

      type T5 is private;
      --% node.p_get_aspect("Predicate_Failure")
      pragma Predicate_Failure (T5, raise Program_Error);
   private
      type T1 is null record;
      type T2 is null record;
      type T3 is tagged null record;
      type T4 is null record;
      type T5 is null record;
   end Pkg;

   procedure P1;
   --% node.p_get_aspect("Exceptional_Cases")
   pragma Exceptional_Cases
     ((Constraint_Error => True, Program_Error => True));
   procedure P1 is null;

   procedure P2;
   --% node.p_get_aspect("Exit_Cases")
   pragma Exit_Cases ((others => Normal_Return));
   procedure P2 is null;

begin
   null;
end;
