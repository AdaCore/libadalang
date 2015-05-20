## vim: filetype=makoada

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with Liblang_Support.Diagnostics; use Liblang_Support.Diagnostics;
with Liblang_Support.Token_Data_Handler; use Liblang_Support.Token_Data_Handler;

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

   function Create_From_File (Filename : String;
                              TDH      : Token_Data_Handler_Access)
                              return Parser_type;

   function Create_From_Buffer (Buffer : String_Access;
                                TDH    : Token_Data_Handler_Access)
                                return Parser_type;

   function Parse (Parser : in out Parser_Type) return AST_Node;

   procedure Clean_All_Memos;
   --  TODO??? We want to allow multiple parsers to run at the same time so
   --  memos should be stored in Parser_Type. In the end, this should be turned
   --  into a Parser_Type finalizer.

   % for parser in _self.generated_parsers:
   ${parser.spec}
   % endfor

end ${_self.ada_api_settings.lib_name}.Parsers;
