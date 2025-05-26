with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out My_Event_Handler;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is
   begin
      if not Found then
         Put_Line ("Unit_Requested_Callback: not found: " & To_UTF8 (Name));
      end if;
   end Unit_Requested_Callback;

   --------------------------
   -- Create_Event_Handler --
   --------------------------

   function Create_Event_Handler return Event_Handler_Reference is
      Result : constant My_Event_Handler := (null record);
   begin
      return Create_Event_Handler_Reference (Result);
   end Create_Event_Handler;

end Support;
