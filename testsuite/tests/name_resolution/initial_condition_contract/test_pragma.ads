package Test_Pragma is
   pragma Initial_Condition ((for all A in Address_Type => F (A)));
   pragma Test_Statement;
   package Inner is
      type Address_Type is new Integer;
      function F (A : Address_Type) return Boolean;
   end Inner;

   use Inner;
   --  Makes Inner visible in the aspect spec above
end Test_Pragma;
