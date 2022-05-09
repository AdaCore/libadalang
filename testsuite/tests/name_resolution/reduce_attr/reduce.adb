procedure Reduce is
   type Grid is array(1 .. 100, 1 .. 80) of Boolean;

   procedure Red_Proc (Acc : in out Boolean; V : Boolean);
   procedure Red_Proc (Acc : in out Integer; V : Integer);
   function Red_Func(Left, Right : Integer) return Integer;

   procedure Red_Proc (Acc : in out Boolean; V : Boolean) is
   begin
      Acc := Acc and V;
   end Red_Proc;

   procedure Red_Proc (Acc : in out Integer; V : Integer) is
   begin
      Acc := Acc + V;
   end Red_Proc;

   function Factorial(N : Natural) return Natural is
     ([for J in 1 .. N => J]'Reduce("*", 1));
   pragma Test_Statement;

   function Red_Func(Left, Right : Boolean) return Boolean is
     (Left and Right);

   function Red_Func(Left, Right : Integer) return Integer is
   begin
      return Left + Right;
   end Red_Func;

   function Red_Func_Body(Left, Right : Integer) return Integer is
   begin
      return Left + Right;
   end Red_Func_Body;

   procedure Gridred(G : Grid; B : out Boolean) is
   begin
      B := G'Reduce("and", True);
      pragma Test_Statement;

      B := G'Reduce(Red_Func, True);
      pragma Test_Statement;

      B := G'Reduce(Red_Proc, B);
      pragma Test_Statement;
   end Gridred;

   Reduced : Integer;
begin
   Reduced := [for J in 1 .. 10 => J]'Reduce(Red_Proc, 1);
   pragma Test_Statement;

   Reduced := [for J in 1 .. 10 => J]'Reduce(Red_Func, Reduced);
   pragma Test_Statement;

   Reduced := [for J in 1 .. 10 => J]'Reduce(Red_Func_Body, Reduced);
   pragma Test_Statement;
end Reduce;
