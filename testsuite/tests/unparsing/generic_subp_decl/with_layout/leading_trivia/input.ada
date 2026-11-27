package Generics is
   generic
      type T is private;
   procedure Adjacent (Value : T);

   generic
      type T is private;  -- Formal type.
   -- Procedure documentation.
   procedure After_Comment (Value : T);

   generic
      type T is private;

   -- Function documentation after a blank line.
   function After_Blank_Comment return T;

   package Nested is
      generic
         type T is private;
      -- Function documentation before a blank line.

      function After_Comment_Blank return T;

      generic
         type T is private;

      procedure Procedure_With_A_Long_Name_And_Several_Parameters
        (First_Parameter : in out T;
         Second_Parameter : T;
         Third_Parameter : T);

      generic
         type T is private;

      function Function_With_A_Long_Name_And_Several_Parameters
        (First_Parameter : T;
         Second_Parameter : T;
         Third_Parameter : T) return T;

      generic

      procedure Empty_Formal_Part;
   end Nested;
end Generics;
