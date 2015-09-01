package body Langkit_Support.Token_Data_Handler is

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Natural) return Token_Vectors.Elements_Array;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (TDH     : out Token_Data_Handler;
      Symbols : Symbol_Table_Access) is
   begin
      TDH := (Tokens            => <>,
              Symbols           => Symbols,
              String_Literals   => <>,
              Tokens_To_Trivias => <>,
              Trivias           => <>);
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
      Destroy (TDH.Trivias);
      Destroy (TDH.Tokens_To_Trivias);
      TDH.Symbols := null;
   end Free;

   --------------------------
   -- Internal_Get_Trivias --
   --------------------------

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Natural) return Token_Vectors.Elements_Arrays.Array_Type
   is
      First_Trivia_Index : constant Integer :=
        (if Length (TDH.Tokens_To_Trivias) = 0
         then -1
         else Get (TDH.Tokens_To_Trivias, Index));
      J : Natural;

      function Extract (T : Trivia_Node) return Token
      is (T.T);

      function Map_Extract is new Trivia_Vectors.Elements_Arrays.Map_Gen
        (Token, Token_Vectors.Elements_Arrays.Array_Type, Extract);

   begin
      if First_Trivia_Index /= -1 then
         J := First_Trivia_Index;

         while Get (TDH.Trivias, J).Has_Next loop
            J := J + 1;
         end loop;

         return Map_Extract (Slice (TDH.Trivias, First_Trivia_Index, J));
      end if;

      return Token_Vectors.Elements_Arrays.Empty_Array;
   end Internal_Get_Trivias;

   -----------------
   -- Get_Trivias --
   -----------------

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Natural) return Token_Vectors.Elements_Arrays.Array_Type
   is
   begin
      return Internal_Get_Trivias (TDH, Index + 1);
   end Get_Trivias;

   -------------------------
   -- Get_Leading_Trivias --
   -------------------------

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Vectors.Elements_Array is
   begin
      return Internal_Get_Trivias (TDH, 0);
   end Get_Leading_Trivias;

end Langkit_Support.Token_Data_Handler;
