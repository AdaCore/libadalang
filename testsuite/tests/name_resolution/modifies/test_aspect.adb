procedure Test_Aspect is
   package P is
      G : Integer := 0;
      H : Integer := 0;

      type Inner is record
         F1 : Integer;
      end record;

      type Nested is record
         G1 : Inner;
      end record;

      A : array (1 .. 10) of Integer;

      type Int_Access is access all Integer;

      --  Single output, no guard
      procedure Write_G
         with Global   => (Output => G),
              Modifies => G;
      pragma Test_Block;

      --  Single output with guard; B is a parameter and must be in scope
      procedure Write_G_If_B (B : Boolean)
         with Global   => (In_Out => G),
              Modifies => (G when B);
      pragma Test_Block;

      --  Component access (X is a parameter, no Global needed)
      procedure Write_Nested (X : in out Nested)
        with Modifies => X.G1.F1;
      pragma Test_Block;

      --  Dereference
      procedure Write_Via_Ptr (P : Int_Access)
         with Modifies => P.all;
      pragma Test_Block;

      --  Array element
      procedure Write_A_At_I (I : Integer)
         with Global   => (In_Out => A),
              Modifies => A (I);
      pragma Test_Block;

      --  Multiple clauses; both guards see B
      procedure Write_G_Or_H (B : Boolean)
         with Global   => (In_Out => (G, H)),
              Modifies => (G when B, H when not B);
      pragma Test_Block;

      --  Multiple objects in one clause (RM MODIFIED_OBJECTS)
      procedure Write_Both_If_B (B : Boolean)
         with Global   => (In_Out => (G, H)),
              Modifies => ((G, H) when B);
      pragma Test_Block;

      --  Multiple objects, no guard
      procedure Write_Both
         with Global   => (In_Out => (G, H)),
              Modifies => (G, H);
      pragma Test_Block;

   end P;
begin
   null;
end Test_Aspect;
