procedure Aspec is
   type Items_Array is array (Positive range <>) of Integer;

   type R (D : Integer with XXX) is
   record
      Items : Items_Array (1 .. D);
   end record;
   --% node.find(lal.DiscriminantSpec).p_has_aspect('XXX')
   --% node.find(lal.DiscriminantSpec).p_get_aspect('XXX')

   function Func1 return String is
   begin
        return Result : String (1..0) with XXX;
   end Func1;
   --% node.find(lal.ExtendedReturnStmtObjectDecl).p_has_aspect('XXX')
   --% node.find(lal.ExtendedReturnStmtObjectDecl).p_get_aspect('XXX')

   protected P is
   private
      entry E1 (Boolean) (N : Natural);
   end P;

   protected body P is
      entry E1 (for B in Boolean with XXX)
      (N : Natural) when True is
      begin
        null;
      end E1;
   end P;
   --% node.find(lal.EntryIndexSpec).p_has_aspect('XXX')
   --% node.find(lal.EntryIndexSpec).p_get_aspect('XXX')

begin
   null;
end Aspec;
