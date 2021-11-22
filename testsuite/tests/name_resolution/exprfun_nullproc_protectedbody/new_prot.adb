procedure New_Prot is
   protected type Prot is
      entry Change (x : Integer);
      procedure Dummy;
      function Double return Integer;
   private
      Counter : Integer := 20;
   end;

   protected body Prot is
      entry Change (x : Integer) when Counter < 100 is
      begin
         Counter := Counter + x;
      end;
      procedure Dummy (x : Integer) is null;
      pragma Test_Block;
      function Double return Integer is (counter * 2);
      pragma Test_Statement;
   end Prot;

   Obj : Prot;

begin
   Obj.Dummy;

   if Obj.Double /= 40 then
      raise Program_Error;
   end if;
end;
