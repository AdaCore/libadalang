pragma Restrictions(No_Elaboration_Code);

package body Foo is
   procedure Set_X (V : Myint) is
   begin
      X := V; -- # body
   end;

   procedure Do_Stuff is separate;
end;
