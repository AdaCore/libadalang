procedure Test_Pragma is
   package P is
      G : Integer := 0;
      H : Integer := 0;

      type Inner is record
         F1 : Integer;
      end record;

      type Nested is record
         G1 : Inner;
      end record;

      --  Single output with guard
      procedure Write_G_If_B (B : Boolean);
      pragma Modifies ((G when B));
      pragma Test_Statement;

      --  Component access, no guard
      procedure Write_Nested (X : in out Nested);
      pragma Modifies (X.G1.F1);
      pragma Test_Statement;

      --  Multiple objects, no guard
      procedure Write_Both;
      pragma Modifies ((G, H));
      pragma Test_Statement;

      --  Multiple clauses
      procedure Write_G_Or_H (B : Boolean);
      pragma Modifies ((G when B, H when not B));
      pragma Test_Statement;

   end P;
begin
   null;
end Test_Pragma;
