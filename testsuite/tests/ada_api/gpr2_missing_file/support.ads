with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

package Support is
   function Create_Event_Handler return Event_Handler_Reference;
private
   type My_Event_Handler is new Event_Handler_Interface with null record;

   overriding procedure Unit_Requested_Callback
     (Self               : in out My_Event_Handler;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Release (Self : in out My_Event_Handler) is null;
end Support;
