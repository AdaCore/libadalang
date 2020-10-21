procedure Test is
   generic
      T : Integer;
   package D is
   end D;

   package D_Inst is new D (T => 12);
begin
   null;
end Test;
