package body B is
   function Make_Rec_1 (X : Integer) return A.Rec_Type is
   begin
      return (X, X);
   end Make_Rec_1;
   pragma Find_All_References (Any);
   pragma Find_All_References (Any, "X");
end B;
