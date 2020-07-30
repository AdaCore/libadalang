procedure Test is
   function Foo return Boolean is (True);

   procedure Process is null;
begin
   begin
      --  Foo should be resolved even though Bar fails in the
      --  first alternative.
      if Foo then
         Process;
      elsif Bar then
         Process;
      else
         Process;
      end if;

      --  Bar will fail but this should not impact the resolution
      --  of Foo in the first alternative.
      if Bar then
         Process;
      elsif Foo then
         Process;
      else
         Process;
      end if;
   end;
   pragma Test_Block;
end Test;
