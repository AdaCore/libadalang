procedure Test_Pro is
   type Item is new Integer;

     protected type Protected_Buffer is
        entry Insert (An_Item : in  Item);
        entry Remove (An_Item : out Item);
     private
        Buffer : Item;
        Empty  : Boolean := True;
     end Protected_Buffer;

     protected body Protected_Buffer is
        entry Insert (An_Item : in  Item)
           when Empty is
        begin
           Buffer := An_Item;
           Empty := False;
        end Insert;
        entry Remove (An_Item : out Item)
           when not Empty is
        begin
           An_Item := Buffer;
           Empty := True;
        end Remove;
     end Protected_Buffer;

     Inst : Protected_Buffer;
begin
   Inst.Insert (12);
end Test_Pro;
pragma Test_Block;
