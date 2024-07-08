with Renamed;
with Renamed.Child;

procedure Test_3 is
   use Renamed;
begin
   --% list(node.p_complete)
   null;
   Child. --% list(node.p_complete)
   null;
   Pkg. --% list(node.p_complete)
end Test_3;

