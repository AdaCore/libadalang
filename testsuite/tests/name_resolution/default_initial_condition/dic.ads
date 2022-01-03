package Dic is
   type Set1 is private
      with Default_Initial_Condition => Is_Empty (Set1);
   pragma Test_Block;

   function Is_Empty (S : Set1) return Boolean;

   type Set2 is private;
   pragma Default_Initial_Condition (Is_Empty (Set2));
   pragma Test_Statement;

   function Is_Empty (S : Set2) return Boolean;
end Dic;
