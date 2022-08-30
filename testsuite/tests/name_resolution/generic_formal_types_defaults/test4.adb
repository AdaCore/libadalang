with Text_IO; use Text_IO;

procedure Test4 is
   type Day is (Monday, Tuesday, Friday, Sunday);
   generic
      type T is (<>) or use Day;
      D : T;
   procedure Check (Day : T);
   procedure Check (Day : T) is
   begin
      Put_Line (T'First'Img);
      Put_Line (Day'Img);
   end;
   procedure Inst1 is new Check (D => Monday);
   pragma Test_Statement;
   procedure Inst2 is new Check (Positive, 8);
   pragma Test_Statement;
begin
   Put_Line ("Check default discrete type");
   Inst1 (Sunday);
   Inst2 (Day'Pos (Sunday));
end;
