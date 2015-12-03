with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
with Langkit_Support.Tokens;  use Langkit_Support.Tokens;
with Langkit_Support.Vectors;

package Langkit_Support.Token_Data_Handler is

   pragma Suppress (Container_Checks);
   pragma Suppress (Tampering_Check);

   --------------
   --  Trivias --
   --------------

   --  Trivias are tokens that are not to be taken into account during parsing,
   --  and are marked as so in the lexer definition. Conceptually, we want
   --  to keep a (potentially empty) list of trivias for each token, which
   --  is every trivia that is between the current token and the next token.

   type Trivia_Node is record
      T        : Token;
      Has_Next : Boolean;
   end record;
   --  This defines a node in a trivia linked list

   package Token_Vectors is new Langkit_Support.Vectors
     (Element_Type => Token);
   package Text_Vectors is new Langkit_Support.Vectors
     (Element_Type => Text_Access);
   package Trivia_Vectors is new Langkit_Support.Vectors
     (Element_Type => Trivia_Node);
   package Integer_Vectors is new Langkit_Support.Vectors
     (Element_Type => Integer);

   use Token_Vectors, Text_Vectors, Trivia_Vectors, Integer_Vectors;

   type Token_Data_Handler is record
      Tokens            : Token_Vectors.Vector;
      Trivias           : Trivia_Vectors.Vector;
      --  Keep the trivias as a contiguous list of trivia nodes. This is
      --  basically a list of linked lists, where you can get the next
      --  element by querying the next index.

      Tokens_To_Trivias : Integer_Vectors.Vector;
      --  This is the correspondence map between regular tokens and trivias

      Symbols           : Symbol_Table;
      String_Literals   : Text_Vectors.Vector;
   end record;

   type Token_Data_Handler_Access is access all Token_Data_Handler;

   procedure Initialize (TDH     : out Token_Data_Handler;
                         Symbols : Symbol_Table);

   procedure Reset (TDH : in out Token_Data_Handler);
   --  Remove all tokens and string literals from TDH. As the ownership for
   --  symbols is shared, symbols are preserved.

   function Add_String
     (TDH : in out Token_Data_Handler;
      S   : Text_Type) return Text_Access;

   function Get_Token
     (TDH   : Token_Data_Handler;
      Index : Natural) return Token
   is
     (Token_Vectors.Get (TDH.Tokens, Index));

   procedure Free (TDH : in out Token_Data_Handler);

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Natural) return Token_Vectors.Elements_Array;

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Vectors.Elements_Array;

end Langkit_Support.Token_Data_Handler;
