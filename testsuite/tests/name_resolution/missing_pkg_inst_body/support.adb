with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body Support is

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out My_EH;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is
   begin
      if not Found and then Is_Not_Found_Error then
         Put_Line ("Missing " & Simple_Name (Image (Name)));
      end if;
   end Unit_Requested_Callback;

end Support;
