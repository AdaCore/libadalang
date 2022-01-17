procedure Iter is
   type Vec is array (1..10) of access Integer;
   X : Vec := (others => new Integer'(1));
begin
   for Ref : access Integer of X loop
      Ref.all := Ref.all + 1;
   end loop;
   pragma Test_Block;
end Iter;
