package Base_Types is

   generic
   function Safe_Pred (Index : integer) return integer with
      Post =>
      (if Index > integer'First then Safe_Pred'Result = Index - 1
       else Safe_Pred'Result = integer'First);
   pragma Test_Block;

end Base_Types;
