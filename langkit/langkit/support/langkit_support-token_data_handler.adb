package body Langkit_Support.Token_Data_Handler is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (TDH     : out Token_Data_Handler;
                         Symbols : Symbol_Table_Access) is
   begin
      TDH := (Tokens          => <>,
              Symbols         => Symbols,
              String_Literals => <>);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset (TDH : in out Token_Data_Handler) is
   begin
      --  Explicit iteration for perf
      for J in 0 .. Last_Index (TDH.String_Literals) loop
         declare
            SL : String_Access := Get (TDH.String_Literals, J);
         begin
            Free (SL);
         end;
      end loop;

      Clear (TDH.String_Literals);
      Clear (TDH.Tokens);
   end Reset;

   ----------------
   -- Add_String --
   ----------------

   function Add_String (TDH : in out Token_Data_Handler;
                        S   : String) return String_Access is
      S_Access : constant String_Access := new String'(S);
   begin
      Append (TDH.String_Literals, S_Access);
      return S_Access;
   end Add_String;

   ----------
   -- Free --
   ----------

   procedure Free (TDH : in out Token_Data_Handler) is
   begin
      Reset (TDH);
      Destroy (TDH.Tokens);
      Destroy (TDH.String_Literals);
      TDH.Symbols := null;
   end Free;

end Langkit_Support.Token_Data_Handler;
