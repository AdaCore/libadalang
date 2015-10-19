## vim: filetype=makoada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Extensions;
with Langkit_Support.PP_Utils; use Langkit_Support.PP_Utils;
with Langkit_Support.Text;     use Langkit_Support.Text;

with ${get_context().ada_api_settings.lib_name}.Parsers;
use ${get_context().ada_api_settings.lib_name}.Parsers;

package body ${_self.ada_api_settings.lib_name} is

   procedure Destroy (Unit : Analysis_Unit);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Analysis_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Analysis_Unit);

   procedure Update_Charset (Unit : Analysis_Unit; Charset : String);
   --  If Charset is an empty string, do nothing. Otherwise, update
   --  Unit.Charset field to Charset.

   procedure Do_Parsing
     (Unit       : Analysis_Unit;
      Get_Parser : access function (Unit : Analysis_Unit) return Parser_Type);
   --  Helper for Get_Unit and the public Reparse procedures: parse an analysis
   --  unit using Get_Parser and replace Unit's AST_Root and the diagnostics
   --  with the parsers's output.

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Get_Parser        : access function (Unit : Analysis_Unit)
                                           return Parser_Type;
      With_Trivia       : Boolean)
      return Analysis_Unit;
   --  Helper for Get_From_File and Get_From_Buffer: do all the common work
   --  using Get_Parser to either parse from a file or from a buffer. Return
   --  the resulting analysis unit.
   --
   --  Get_Parser is allowed to raise a Name_Error exception if there reading a
   --  file does not work: resources will be correctly released in this case.

   --------------------
   -- Update_Charset --
   --------------------

   procedure Update_Charset (Unit : Analysis_Unit; Charset : String)
   is
   begin
      if Charset'Length /= 0 then
         Unit.Charset := To_Unbounded_String (Charset);
      end if;
   end Update_Charset;

   ------------
   -- Create --
   ------------

   function Create (Charset : String) return Analysis_Context is
   begin
      return new Analysis_Context_Type'
        (Units_Map => <>,
         Symbols   => Create,
         Charset   => To_Unbounded_String (Charset));
   end Create;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Get_Parser        : access function (Unit : Analysis_Unit)
                                           return Parser_Type;
      With_Trivia : Boolean)
      return Analysis_Unit
   is
      use Units_Maps;

      Fname   : constant Unbounded_String := To_Unbounded_String (Filename);
      Cur     : constant Cursor := Context.Units_Map.Find (Fname);
      Created : constant Boolean := Cur = No_Element;
      Unit    : Analysis_Unit;

      Actual_Charset : Unbounded_String;

   begin
      --  Determine which encoding to use.  The parameter comes first, then the
      --  unit-specific default, then the context-specific one.

      if Charset'Length /= 0 then
         Actual_Charset := To_Unbounded_String (Charset);
      elsif not Created then
         Actual_Charset := Element (Cur).Charset;
      else
         Actual_Charset := Context.Charset;
      end if;

      --  Create the Analysis_Unit if needed

      if Created then
         Unit := new Analysis_Unit_Type'
           (Context      => Context,
            Ref_Count    => 1,
            AST_Root     => null,
            File_Name    => Fname,
            Charset     => <>,
            TDH          => <>,
            Diagnostics  => <>,
            With_Trivia  => With_Trivia,
            AST_Mem_Pool => No_Pool);
         Initialize (Unit.TDH, Context.Symbols);
      else
         Unit := Element (Cur);
      end if;
      Unit.Charset := Actual_Charset;

      --  (Re)parse it if needed

      if Created
         or else Reparse
         or else (With_Trivia and then not Unit.With_Trivia)
      then
         begin
            Do_Parsing (Unit, Get_Parser);
         exception
            when Name_Error =>
               if Created then
                  Dec_Ref (Unit);
               end if;
               raise;
         end;
      end if;

      if Created then
         Context.Units_Map.Insert (Fname, Unit);
      end if;

      return Unit;
   end Get_Unit;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit       : Analysis_Unit;
      Get_Parser : access function (Unit : Analysis_Unit) return Parser_Type)
   is
      Parser : Parser_Type := Get_Parser (Unit);
   begin
      --  If we have an AST_Mem_Pool already, we are reparsing. We want to
      --  destroy it to free all the allocated memory.
      if Unit.AST_Mem_Pool /= No_Pool then
         Free (Unit.AST_Mem_Pool);
      end if;

      Unit.AST_Mem_Pool := Create;
      Parser.Mem_Pool := Unit.AST_Mem_Pool;

      Unit.AST_Root := Parse (Parser);
      Unit.Diagnostics := Parser.Diagnostics;
      Clean_All_Memos;
   end Do_Parsing;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False)
      return Analysis_Unit
   is
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_File (Filename, To_String (Unit.Charset),
                            Unit.TDH'Access, With_Trivia));
   begin
      return Get_Unit
        (Context, Filename, Charset, Reparse, Get_Parser'Access, With_Trivia);
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Buffer      : String;
      With_Trivia : Boolean := False)
      return Analysis_Unit
   is
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_Buffer (Buffer, To_String (Unit.Charset),
                              Unit.TDH'Access, With_Trivia));
   begin
      return Get_Unit (Context, Filename, Charset, True, Get_Parser'Access,
                       With_Trivia);
   end Get_From_Buffer;

   ------------
   -- Remove --
   ------------

   procedure Remove (Context   : Analysis_Context;
                     File_Name : String)
   is
      use Units_Maps;

      Cur : Cursor := Context.Units_Map.Find (To_Unbounded_String (File_Name));
   begin
      if Cur = No_Element then
         raise Constraint_Error with "No such analysis unit";
      end if;

      --  We remove the corresponding analysis unit from this context but
      --  users could keep references on it, so make sure it can live
      --  independently.

      declare
         Unit : constant Analysis_Unit := Element (Cur);
      begin
         Unit.Context := null;
         Dec_Ref (Unit);
      end;

      Context.Units_Map.Delete (Cur);
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Analysis_Context) is
   begin
      for Unit of Context.Units_Map loop
         Unit.Context := null;
         Dec_Ref (Unit);
      end loop;
      Destroy (Context.Symbols);
      Free (Context);
   end Destroy;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Unit : Analysis_Unit) is
   begin
      Unit.Ref_Count := Unit.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Unit : Analysis_Unit) is
   begin
      Unit.Ref_Count := Unit.Ref_Count - 1;
      if Unit.Ref_Count = 0 then
         Destroy (Unit);
      end if;
   end Dec_Ref;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "")
   is
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_File (To_String (Unit.File_Name),
                            To_String (Unit.Charset),
                            Unit.TDH'Access));
   begin
      Update_Charset (Unit, Charset);
      Do_parsing (Unit, Get_Parser'Access);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String)
   is
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_Buffer (Buffer, To_String (Unit.Charset),
                              Unit.TDH'Access));
   begin
      Update_Charset (Unit, Charset);
      Do_parsing (Unit, Get_Parser'Access);
      Unit.Charset := To_Unbounded_String (Charset);
   end Reparse;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : Analysis_Unit) is
      Unit_Var : Analysis_Unit := Unit;
   begin
      if Unit.AST_Root /= null then
         Destroy (Unit.AST_Root);
      end if;
      Free (Unit.TDH);
      Free (Unit.AST_Mem_Pool);
      Free (Unit_Var);
   end Destroy;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit) is
   begin
      if Unit.AST_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Unit.AST_Root.Print;
      end if;
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit) is
   begin
      for Tok of Get_Leading_Trivias (Unit.TDH) loop
         Put_Line (Image (Tok.Text.all));
      end loop;

      PP_Trivia (Unit.AST_Root);

      for Tok of Get_Trivias (Unit.TDH, Unit.AST_Root.Token_End) loop
         Put_Line (Image (Tok.Text.all));
      end loop;
   end PP_Trivia;

   % for chunk in _self.primitives_bodies:
   ${chunk}
   % endfor

   ----------------
   -- Initialize --
   -----------------

   procedure Initialize is

      --  Invoke GNAT-specific initialization procedures manually (these
      --  trigger package elaboration). This is needed because on some
      --  platforms, such as AIX, there is no facility at link time to
      --  introduce automatically called procedures.

      --  Trigger the elaboration of all our dependencies that are standalone
      --  libraries: libadalang and liblangkit_support. We also depend on
      --  GNATcoll and on the GNAT runtime, but these are not standalone
      --  libraries.

      --  Note that on platforms that do have automatic initialization, calling
      --  initialization more that once is supported and does not cause
      --  trouble. You can hence write multi-platform code by explicitly
      --  calling Initialize, and omit it if you'll run your code on modern
      --  platforms only.

      procedure Langkit_Support_Init
        with Import        => True,
             Convention    => C,
             External_Name => "langkit_supportinit";
      procedure Libadalang_Init
        with Import        => True,
             Convention    => C,
             External_Name => "${capi.shared_object_basename}init";
   begin
      Langkit_Support_Init;
      Libadalang_Init;
   end Initialize;

end ${_self.ada_api_settings.lib_name};
