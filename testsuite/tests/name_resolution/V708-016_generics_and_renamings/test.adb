procedure Test is
   generic
      type Item is private;
   package Generic_Collections is
      generic
      package Abstract_Collections is
         generic
         package Unbounded_Collections is
            type Collection is null record;

            procedure Append (Self : Collection; X : Item) is null;
         end Unbounded_Collections;
      end Abstract_Collections;
   end Generic_Collections;

   generic
      type T is private;
   package T_Collections is
      package T_Collections is new Generic_Collections (T);
      package T_Abstract_Collections is
         new T_Collections.Abstract_Collections;
      package T_Unbounded_Collections is
         new T_Abstract_Collections.Unbounded_Collections;
      subtype Coll is T_Unbounded_Collections.Collection;

      procedure Append (Self : Coll; X : T) renames
         T_Unbounded_Collections.Append;
   end T_Collections;

   package Int_Collections is new T_Collections (Integer);

   X : Int_Collections.Coll;
begin
   Int_Collections.Append (X, 2);
   --% renaming=node.f_call.f_name.p_referenced_decl()
   --% renaming.f_renames.f_renamed_object.p_referenced_decl()
end Test;
