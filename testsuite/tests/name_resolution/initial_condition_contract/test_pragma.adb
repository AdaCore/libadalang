package body Test_Pragma is
   package body Inner is
      function F (A : Address_Type) return Boolean is
      begin
         return True;
      end F;
   end Inner;
end Test_Pragma;
