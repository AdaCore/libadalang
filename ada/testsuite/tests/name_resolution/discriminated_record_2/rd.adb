with Ada.Text_IO; use Ada.Text_IO;

procedure RD is
   type R1 (D1 : Integer) is tagged record
      A, B, C : Float;
   end record;

   type N is (A, B, C);

   type R2 (D2 : N) is new R1 (12) with record
      D, E, F : Integer;
   end record;

   Inst : R2 := (A, 14.0, 15.0, 28.0, 33, 44, 88);
   pragma Test_Statement;
begin
   Put_Line (Inst.D2'Image);
end RD;
