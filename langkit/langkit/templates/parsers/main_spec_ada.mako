## vim: filetype=makoada

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Token_Data_Handler; use Langkit_Support.Token_Data_Handler;

package ${_self.ada_api_settings.lib_name}.Parsers is

   type Fail_Info is record
      Pos               : Integer := -1;
      Expected_Token_Id : Unsigned_16;
      Found_Token_Id    : Unsigned_16;
   end record;

   type Parser_Type is record
      Current_Pos : Integer := 0;
      Last_Fail   : Fail_Info;
      Diagnostics : Diagnostics_Vectors.Vector;
      TDH         : Token_Data_Handler_Access;
   end record;

   function Create_From_File
     (Filename    : String;
      TDH         : Token_Data_Handler_Access;
      With_Trivia : Boolean := False) return Parser_type;

   function Create_From_Buffer
     (Buffer      : String;
      TDH         : Token_Data_Handler_Access;
      With_Trivia : Boolean := False) return Parser_type;

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) return AST_Node
     with Inline_Always => True;
   --  Do the actual parsing using the main parsing rule.  If Check_Complete,
   --  consider the case when the parser could not consume all the input tokens
   --  as an error.

   ## Generate user wrappers for all parsing rules
   % for rule_name, parser in sorted(_self.rules_to_fn_names.items()):
      function Parse_${parser._name}
        (Parser         : in out Parser_Type;
         Check_Complete : Boolean := True)
         return ${decl_type(parser.get_type())};
      --  Do the actual parsing using the ${rule_name} parsing rule.  If
      --  Check_Complete, consider the case when the parser could not consume
      --  all the input tokens as an error.
   % endfor

   procedure Clean_All_Memos;
   --  TODO??? We want to allow multiple parsers to run at the same time so
   --  memos should be stored in Parser_Type. In the end, this should be turned
   --  into a Parser_Type finalizer.

end ${_self.ada_api_settings.lib_name}.Parsers;
