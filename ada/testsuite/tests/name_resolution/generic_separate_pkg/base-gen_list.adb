separate (Base)
package body Gen_List is
   procedure Append (Self : in out List; Item : T) is
   begin
      Self.Item := Item;
      pragma Test_Statement;
   end Append;
end Gen_List;
