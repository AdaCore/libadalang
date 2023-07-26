with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   type Dummy_EH is new Event_Handler_Interface with record
      Triggered : Boolean := False;
   end record;

   overriding procedure Unit_Parsed_Callback
     (Self     : in out Dummy_EH;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean);

   overriding procedure Release (Self : in out Dummy_EH) is null;

   --------------------------
   -- Unit_Parsed_Callback --
   --------------------------

   overriding procedure Unit_Parsed_Callback
     (Self     : in out Dummy_EH;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean)
   is
   begin
      if not Self.Triggered then
         Self.Triggered := True;
         Put_Line ("Unit_Parsed_Callback invoked");
      end if;
   end Unit_Parsed_Callback;

   ---------------
   -- Create_EH --
   ---------------

   function Create_EH return Event_Handler_Reference is
   begin
      return Create_Event_Handler_Reference (Dummy_EH'(others => <>));
   end Create_EH;

end Support;
