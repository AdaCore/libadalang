separate (P1.P2)
procedure P3 is
     S : String (1 .. 2);
     C : Character;
     procedure P4 is separate;
begin
     S := P2.S;
     pragma Test_Statement;

     C := P1.C;
     pragma Test_Statement;

     C := P2.H;
     pragma Test_Statement;
end P3;
