with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

package Support is

   type My_EH is new Event_Handler_Interface with null record;

   overriding procedure Release (Self : in out My_EH) is null;
   overriding procedure Unit_Requested_Callback
     (Self               : in out My_EH;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

end Support;
