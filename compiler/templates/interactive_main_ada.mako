## vim: filetype=makoada

with Ada.Calendar;              use Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
with Ada.Text_IO;               use Ada.Text_IO;

with Interfaces; use Interfaces;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;

with Liblang_Support.AST; use Liblang_Support.AST;
with Liblang_Support.Diagnostics; use Liblang_Support.Diagnostics;
with Liblang_Support.Indent; use Liblang_Support.Indent;
with Liblang_Support.Token_Data_Handler; use Liblang_Support.Token_Data_Handler;
with Liblang_Support.Tokens; use Liblang_Support.Tokens;
with ${_self.ada_api_settings.lib_name}; use ${_self.ada_api_settings.lib_name};
with ${_self.ada_api_settings.lib_name}.Parsers;
use ${_self.ada_api_settings.lib_name}.Parsers;

procedure Parse is

   function "+" (S : String) return Unbounded_String renames
      To_Unbounded_String;

   package String_Vectors is new Ada.Containers.Vectors
     (Natural, Unbounded_String);

   Config    : Command_Line_Configuration;
   Silent    : aliased Boolean;
   Measure_Time : aliased Boolean;
   Indent    : aliased Boolean;
   Rule_Name : aliased GNAT.Strings.String_Access :=
      new String'("compilation_unit");
   File_Name : aliased GNAT.Strings.String_Access;
   File_List : aliased GNAT.Strings.String_Access;

   Input_Str : Unbounded_String;
   Lookups : String_Vectors.Vector;

   -----------------
   -- Split_Lines --
   -----------------

   procedure Split_Lines
     (Str   : Unbounded_String;
      Lines : in out Liblang_Support.Indent.String_Vectors.Vector)
   is
      Old_Cursor, Cursor : Natural := 1;
   begin
      Lines.Clear;
      loop
         Cursor := Index (Str, (1 => ASCII.LF), Old_Cursor);
         exit when Cursor = 0;
         Lines.Append (Unbounded_Slice (Str, Old_Cursor, Cursor - 1));
         Old_Cursor := Cursor + 1;
      end loop;
   end Split_Lines;

   ---------------
   -- Do_Indent --
   ---------------

   procedure Do_Indent (Root  : AST_Node;
                        Lines : String_Vector_Access)
   is
      Engine : Indent_Engine := Create (Root, Lines);
   begin
      Process (Engine);

      Put_Line ("============= UNINDENTED CODE ==============");
      New_Line;

      for Line of Lines.all loop
         Trim (Line, Left);
         Put_Line (To_String (Line));
      end loop;
      New_Line;

      Put_Line ("============= INDENTED CODE ==============");
      New_Line;

      for Cur in Lines.Iterate loop
         declare
            Line_Index : constant Natural :=
               Liblang_Support.Indent.String_Vectors.To_Index (Cur);
            Line       : constant Unbounded_String :=
               Liblang_Support.Indent.String_Vectors.Element (Cur);
            Level : constant Natural :=
               Natural (Liblang_Support.Indent.Indent (Engine, Line_Index));
         begin
            Put ((1 .. Level => ' '));
            Put_Line (To_String (Line));
         end;
      end loop;
      New_Line;

      Put_Line ("==========================================");
   end Do_Indent;

   ------------------
   -- Process_Node --
   ------------------

   procedure Process_Node (Res : in out AST_Node) is
   begin
      if Res = null then
         Put_Line ("<null node>");
         return;
      end if;

      Res.Validate;
      if not Silent then
         Res.Print;
      end if;

      for Lookup_Str of Lookups loop
         New_Line;

         declare
            Sep : constant Natural := Index (Lookup_Str, ":");

            Line : Unsigned_32 := Unsigned_32'Value
              (Slice (Lookup_Str, 1, Sep - 1));
            Column : Unsigned_16 := Unsigned_16'Value
              (Slice (Lookup_Str, Sep + 1, Length (Lookup_Str)));

            Sloc : constant Source_Location := (Line, Column);
            Lookup_Res : AST_Node :=
               Lookup (AST_Node (Res), (Line, Column));
         begin
            Put_Line ("Lookup " & Image (Sloc) & ":");
            Lookup_Res.Print;
         end;
      end loop;

      if Indent then
         declare
            Lines : Liblang_Support.Indent.String_Vectors.Vector;
         begin
            Split_Lines (Input_Str, Lines);
            Do_Indent (AST_Node (Res), Lines'Unrestricted_Access);
         end;
      end if;

      Dec_Ref (Res);
   end Process_Node;

   -----------------
   -- Parse_Input --
   -----------------

   procedure Parse_Input is
      Input_Str_Ptr    : Big_String_Access;
      Input_Str_Length : Natural;

      Context : Analysis_Context := Create;
      TDH     : Token_Data_Handler;
      Parser  : Parser_Type;
   begin
      Initialize (TDH, Context.Symbols);

      Get_String (Input_Str, Input_Str_Ptr, Input_Str_Length);
      Parser := Create_From_Buffer
        (Input_Str_Ptr (1 .. Input_Str_Length),
         TDH'Unrestricted_Access);

      % for i, (rule_name, parser) in enumerate(_self.rules_to_fn_names.items()):
         ${"if" if i == 0 else "elsif"} Rule_Name.all = ${string_repr(rule_name)} then
            declare
               Res : ${parser.get_type().name()} :=
                  Parse_${parser._name} (Parser);
            begin
               if not Parser.Diagnostics.Is_Empty then
                  Put_Line ("Parsing failed:");
                  for D of Parser.Diagnostics loop
                     Put_Line (To_Pretty_String (D));
                  end loop;
               end if;

               ## Error recovery may make the parser return something even on
               ## error: print anyway.
               % if is_ast_node(parser.get_type()):
                  Process_Node (AST_Node (Res));
               % else:
                  Put_Line (${parser.get_type().name()}'Image (Res));
                  if not Lookups.Is_Empty then
                     Put_Line ("Cannot lookup non-AST nodes!");
                  end if;
               % endif
            end;

      % endfor

         else
            raise Program_Error with "Unknown rule: " & Rule_Name.all;
         end if;

      Free (TDH);
      Destroy (Context);
   end Parse_Input;

   procedure Process_File (File_Name : String; Ctx : in out Analysis_Context) is
      Unit : Analysis_Unit;
      Time_Before  : constant Time := Clock;
      Time_After   : Time;
   begin
      Unit := Get_From_File (Ctx, File_Name, True);
      Time_After := Clock;

      if not Unit.Diagnostics.Is_Empty then
         Put_Line ("Errors while parsing " & File_Name);
         for D of Unit.Diagnostics loop
            Put_Line (To_Pretty_String (D));
         end loop;

      elsif not Silent then
         Unit.AST_Root.Print;
      end if;

      if Measure_Time then
         Put_Line
           ("Time elapsed: " & Duration'Image (Time_After - Time_Before));
      end if;

   end Process_File;

begin
   Set_Usage
     (Config,
      Usage    => "[switches] [input] [lookups]");
   Define_Switch
     (Config, Silent'Access, "-s", "--silent",
      Help   => "Do not print the representation of the resulting tree");
   Define_Switch
     (Config, Measure_Time'Access, "-t", "--time",
      Help   => "Time the execution of parsing");
   Define_Switch
     (Config, Indent'Access, "-i", "--indent",
      Help   => "Print an indented representation of the input code");
   Define_Switch
     (Config, Rule_Name'Access, "-r:", "--rule-name:",
      Help   => "Rule name to parse");
   Define_Switch
     (Config, File_Name'Access, "-f:", "--file-name:",
      Help   => "Parse file");
   Define_Switch
     (Config, File_List'Access, "-F:", "--file-list:",
      Help   => ("Parse files listed in the provided filename with the regular"
                 & " analysis circuitry (useful for timing measurements)"));
   begin
      Getopt (Config);
   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         return;
   end;

   if File_List.all'Length /= 0 then
      declare
         F : File_Type;
         Ctx : Analysis_Context := Create;
      begin
         Open (F, In_File, File_List.all);
         while not End_Of_File (F) loop
            Process_File (Get_Line (F), Ctx);
         end loop;
         Close (F);
         Destroy (Ctx);
      end;
   elsif File_Name.all'Length /= 0 then
      declare
         Ctx : Analysis_Context := Create;
      begin
         Process_File (File_Name.all, Ctx);
         Destroy (Ctx);
      end;
   else
      Input_Str := +Get_Argument;
      loop
         declare
            Arg : String := Get_Argument;
         begin
            exit when Arg'Length = 0;
            Lookups.Append (+Arg);
         end;
      end loop;

      declare
         Time_Before : constant Time := Clock;
         Time_After  : Time;
      begin
         Parse_Input;
         Time_After := Clock;
         if Measure_Time then
            Put_Line
              ("Time elapsed: " & Duration'Image (Time_After - Time_Before));
         end if;
      end;

   end if;

   GNAT.Strings.Free (Rule_Name);
   GNAT.Strings.Free (File_List);
   GNAT.Strings.Free (File_Name);
   Free (Config);
end Parse;
