separate (P1)
procedure P2 (H : Character) is
     S : String (1 .. 2);
     C : Character;
     procedure P3 is separate;
begin
     C := P1.C;
     pragma Test_Statement;

     S := P1.C & "-";
     pragma Test_Statement;
end P2;
