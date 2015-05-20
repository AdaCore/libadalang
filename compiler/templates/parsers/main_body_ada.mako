## vim: filetype=makoada

with Liblang_Support.Diagnostics; use Liblang_Support.Diagnostics;
with Liblang_Support.Packrat;

with Libadalang.Lexer; use Libadalang.Lexer;

package body ${_self.ada_api_settings.lib_name}.Parsers is

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists).

   % for enum_type in _self.enum_declarations:
      package ${enum_type.type.name()}_Memos is new Liblang_Support.Packrat
        (${enum_type.type.name()});
      use ${enum_type.type.name()}_Memos;
   % endfor

   package AST_Node_Memos is new Liblang_Support.Packrat (AST_Node, Dec_Ref);
   use AST_Node_Memos;

   package List_AST_Node_Memos is new Liblang_Support.Packrat
     (List_AST_Node, Dec_Ref);
   use List_AST_Node_Memos;

   % for cls in _self.astnode_types:
      package ${cls.name()}_Memos is new Liblang_Support.Packrat
        (${cls.name()}, Dec_Ref);
      use ${cls.name()}_Memos;

      % if cls in _self.list_types:
         package List_${cls.name()}_Memos is new Liblang_Support.Packrat
           (List_${cls.name()}, Dec_Ref);
         use List_${cls.name()}_Memos;
      % endif
   % endfor

   ----------------------
   -- Create_From_File --
   ----------------------

   function Create_From_File (Filename : String;
                              TDH      : Token_Data_Handler_Access)
                              return Parser_type
   is
   begin
      Lex_From_Filename (Filename, "", TDH.all);
      return (TDH => TDH, others => <>);
   end Create_From_File;

   ------------------------
   -- Create_From_Buffer --
   ------------------------

   function Create_From_Buffer (Buffer : String;
                                TDH    : Token_Data_Handler_Access)
                                return Parser_type
   is
   begin
      Lex_From_Buffer (Buffer, TDH.all);
      return (TDH => TDH, others => <>);
   end Create_From_Buffer;

   -----------
   -- Parse --
   -----------

   function Parse (Parser : in out Parser_Type) return AST_Node
   is
      Result : AST_Node := AST_Node
        (${_self.rules_to_fn_names[_self.main_rule_name].gen_fn_name} (Parser, 0));
   begin
      if Result = null then
         declare
            Last_Token : Token renames
               Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
            D : constant Diagnostic :=
              (Sloc_Range => Last_Token.Sloc_Range,
               Message    => To_Unbounded_String
                 ("Expected """
                  & Token_Text (Parser.Last_Fail.Expected_Token_Id)
                  & """, got """
                  & Token_Text (Parser.Last_Fail.Found_Token_Id)
                  & """"));
         begin
            Parser.Diagnostics.Append (D);
         end;
      else
         Inc_Ref (Result);
      end if;
      Clean_All_Memos;
      return Result;
   end Parse;

   % for parser in _self.generated_parsers:
   ${parser.body}
   % endfor

   ---------------------
   -- Clean_All_Memos --
   ---------------------

   procedure Clean_All_Memos is
   begin
      % for fn in _self.fns:
         Clear (${fn}_Memo);
      % endfor
   end Clean_All_Memos;

end ${_self.ada_api_settings.lib_name}.Parsers;
