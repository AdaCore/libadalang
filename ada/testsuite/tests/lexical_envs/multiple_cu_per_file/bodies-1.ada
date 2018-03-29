package body Parent.Child.Grandchild is
   function Fact (I : Integer) return Integer is
   begin
      if I < 2 then
         return 1;
      else
         return I * Fact (I - 1);
      end if;
   end Fact;
end Parent.Child.Grandchild;
