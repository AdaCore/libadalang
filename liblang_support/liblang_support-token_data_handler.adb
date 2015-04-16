package body Liblang_Support.Token_Data_Handler is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (TDH     : out Token_Data_Handler;
                         Symbols : Symbol_Table_Access) is
   begin
      TDH.Tokens.Clear;
      TDH.Symbols := Symbols;
      TDH.String_Literals.Clear;
   end Initialize;

   ----------------
   -- Add_String --
   ----------------

   function Add_String (TDH : in out Token_Data_Handler;
                        S   : String) return String_Access is
      S_Access : constant String_Access := new String'(S);
   begin
      TDH.String_Literals.Append (S_Access);
      return S_Access;
   end Add_String;

   ----------
   -- Free --
   ----------

   procedure Free (TDH : in out Token_Data_Handler) is
   begin
      TDH.Tokens.Clear;
      TDH.Symbols := null;
      for Str_Lit of TDH.String_Literals loop
         Free (Str_Lit);
      end loop;
      TDH.String_Literals.Clear;
   end Free;

end Liblang_Support.Token_Data_Handler;
