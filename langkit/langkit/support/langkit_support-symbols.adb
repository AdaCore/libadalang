with Ada.Unchecked_Deallocation;

package body Langkit_Support.Symbols is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Sets.Set, Symbol_Table);

   ------------
   -- Create --
   ------------

   function Create return Symbol_Table is
   begin
      return new Sets.Set;
   end Create;

   ----------
   -- Find --
   ----------

   function Find (ST : Symbol_Table; T : Text_Type) return Symbol_Type
   is
      use Sets;

      T_Acc    : Symbol_Type := T'Unrestricted_Access;
      Result   : Cursor := ST.Find (T_Acc);
      Inserted : Boolean;
   begin
      --  If we already have such a symbol, return the access we already
      --  internalized.

      if not Has_Element (Result) then

         --  Otherwise internalize it first

         T_Acc := new Text_Type'(T);
         ST.Insert (T_Acc, Result, Inserted);
         pragma Assert (Inserted);
      end if;

      return Element (Result);
   end Find;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (ST : in out Symbol_Table) is
      use Sets;
      C       : Cursor := ST.First;
   begin
      while Has_Element (C) loop
         declare
            To_Free : Symbol_Type := Element (C);
         begin
            Next (C);
            Free (To_Free);
         end;
      end loop;
      Deallocate (ST);
   end Destroy;

end Langkit_Support.Symbols;
