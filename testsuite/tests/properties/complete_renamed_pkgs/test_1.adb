with Renamed;

procedure Test_1 is
   package My_Gen is new Renamed.Gen;
begin
   Renamed. --% list(node.p_complete)
   null;
   My_Gen.  --% list(node.p_complete)
end Test_1;
