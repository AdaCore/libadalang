separate (P1.P2.P3)
procedure P4 is
     S : String (1 .. 2);
     C : Character;
begin
     S := P2.S;
     pragma Test_Statement;

     C := P1.C;
     pragma Test_Statement;

     C := P2.H;
     pragma Test_Statement;

     S := P3.S;
     pragma Test_Statement;

     C := P3.C;
     pragma Test_Statement;

end P4;
