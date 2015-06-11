with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package body Langkit_Support.Extensions is

   package Extension_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Extension_Id,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   Next_Extension_Id : Extension_ID := 1;
   Extensions        : Extension_Maps.Map;

   function Register_Extension (Name : String) return Extension_ID is
      use Extension_Maps;

      Key : constant Unbounded_String := To_Unbounded_String (Name);
      Cur : constant Extension_Maps.Cursor := Extensions.Find (Key);
   begin
      if Cur = No_Element then
         declare
            Result : constant Extension_ID := Next_Extension_Id;
         begin
            Next_Extension_Id := Next_Extension_Id + 1;
            Extensions.Insert (Key, Result);
            return Result;
         end;
      else
         return Element (Cur);
      end if;
   end Register_Extension;

end Langkit_Support.Extensions;
