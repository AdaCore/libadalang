with Libadalang.Analysis; use Libadalang.Analysis;

package Support is

   function Create_EH return Event_Handler_Reference;
   --  Return an event handler that prints a message only the first time the
   --  Unit_Parsed_Callback callback is called.

end Support;
