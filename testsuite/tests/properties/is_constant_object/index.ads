package Index is

   type T is limited private;
   type Entry_Id_Mod is mod 2;

private

   protected type T is
     entry E (Entry_Id_Mod) with
       Pre => E'Index < 2;
     --% node.find(lal.AttributeRef).p_is_constant
   end T;

end Index;
