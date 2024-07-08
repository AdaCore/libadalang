with Renamed;

with Pkg.Child;

procedure Test_2 is
begin
   Renamed. --% list(node.p_complete)
   null;
   Renamed.Child. --% list(node.p_complete)
end Test_2;
