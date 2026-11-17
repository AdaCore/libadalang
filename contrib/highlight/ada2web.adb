with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Strings;            use GNATCOLL.Strings;
with GNATCOLL.Traces;
with GPR2.Build.Source;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with Libadalang.Analysis;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Colors;
with Highlighter;
with HTML;

--  Take an Ada source file as command-line argument and output a syntax
--  highlighted version of the source code on the standard output.
--
--  By default, or if one argument is --html, output it as an HTML document
--  (CSS included). If --term256 appears instead, output it as a sequence of
--  ANSI escape codes.

procedure Ada2Web is

   package LAL renames Libadalang.Analysis;

   function Writeable (File : Ada.Text_IO.File_Type) return Boolean is
     (Ada.Text_IO."/=" (Ada.Text_IO.Mode (File), Ada.Text_IO.In_File));

   procedure Create_If_Needed (Directory : String);
   --  If Directory does not exist, create it

   procedure Print_Usage (Error : String := "");
   --  Display command-line usage on the standard output. If Error is a
   --  non-empty string, also display it as an error message.

   function Parse_Arguments return Boolean;
   --  Process command-line arguments, loading the given project and processing
   --  the given project name list. Return whether we are ready to create HTML
   --  pages: False means that there is an error. In this case, Parse_Arguments
   --  prints the error message.

   procedure Process_File
     (Project     : GPR2.Project.View.Object;
      Source_File : String;
      Output_File : in out Ada.Text_IO.File_Type)
     with Pre => Writeable (Output_File);
   --  Emit highlighted and xref'd source code for the given Source_File, which
   --  comes from Project. Write the syntax highlighted source code for Unit
   --  according to Output_Format. The HTML code is written to Output_File.

   procedure Emit_HTML_Header
     (File          : in out Ada.Text_IO.File_Type;
      Title, Prefix : String)
     with Pre => Writeable (File);
   --  Emit header HTML code with the given Title string. Prefix is used as a
   --  prefix for all file paths to be made relative to the root path.

   procedure Emit_HTML_Footer (File : in out Ada.Text_IO.File_Type)
     with Pre => Writeable (File);

   package String_Vectors is new Ada.Containers.Vectors (Positive, XString);

   package Project_Sets is new Ada.Containers.Ordered_Sets
     (GPR2.Project.View.Object,
      "=" => GPR2.Project.View."=",
      "<" => GPR2.Project.View."<");

   function To_Set
     (View : GPR2.Project.View.Object) return GPR2.Project.View.Set.Object;
   --  Return a set that contains the givew view only

   Prj_Tree : GPR2.Project.Tree.Object;
   --  Project tree for all sources to analyze

   Projects : Project_Sets.Set;
   --  Subset of projects in Prj_Tree for which we emit highlighted source code

   Ctx : LAL.Analysis_Context;
   UFP : LAL.Unit_Provider_Reference;

   CSS_Filename : constant String := "style.css";

   ---------------------
   -- Parse_Arguments --
   ---------------------

   function Parse_Arguments return Boolean is
      Opts         : GPR2.Options.Object;
      Project_List : String_Vectors.Vector;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Arg : constant XString :=
              To_XString (Ada.Command_Line.Argument (I));
         begin
            if Arg.Length > 2 and then Arg.Starts_With ("--") then
               declare
                  Opt : constant XString := Arg.Slice (3, Arg.Length);
               begin
                  if Opt = "help" then
                     Print_Usage;
                     return False;
                  else
                     Print_Usage ("Invalid option: " & Arg.To_String);
                     return False;
                  end if;
               end;

            elsif Arg.Length > 1 and then Arg.Starts_With ("-") then
               declare
                  Opt : constant XString :=
                    Arg.Slice (2, Arg.Length);
               begin
                  if Opt.Starts_With ("P") then
                     Opts.Add_Switch
                       (GPR2.Options.P, Opt.Slice (2, Opt.Length).To_String);
                  elsif Opt.Starts_With ("X") then
                     Opts.Add_Switch
                       (GPR2.Options.X, Opt.Slice (2, Opt.Length).To_String);
                  else
                     Print_Usage ("Invalid option: " & Arg.To_String);
                     return False;
                  end if;
               end;

            else
               Project_List.Append (Arg);
            end if;
         end;
      end loop;

      if Project_List.Is_Empty then
         Print_Usage ("Missing list of project files to process");
         return False;
      end if;

      --  Now load the root project

      if not Prj_Tree.Load
        (Opts,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units,
         Absent_Dir_Error     => GPR2.No_Error,
         Check_Drivers        => False)
      then
         return False;
      end if;

      --  And resolve the project names to actual view

      for P of Project_List loop
         declare
            use type GPR2.Path_Name.Object;

            Normalized_Name : constant String := P.To_Lower.To_String;
            File_Name       : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_File (GPR2.Filename_Type (P.To_String));

            Prj : GPR2.Project.View.Object;
         begin
            for View of Prj_Tree loop
               if To_Lower (String (View.Name)) = Normalized_Name
                  or else View.Path_Name = File_Name
               then
                  --  If here are more than one project matching the requested
                  --  name, abort with an error.

                  if Prj.Is_Defined then
                     Print_Usage ("Invalid project file name: " & P.To_String);
                     return False;
                  end if;

                  Prj := View;
               end if;
            end loop;

            --  If we found no project matching the requested name, abort with
            --  an error.

            if not Prj.Is_Defined then
               Print_Usage ("Invalid project file name: " & P.To_String);
               return False;
            end if;
            Projects.Include (Prj);
         end;
      end loop;

      return True;
   end Parse_Arguments;

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage (Error : String := "") is
      Command : constant String := Ada.Command_Line.Command_Name;
   begin
      if Error'Length > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         Ada.Text_IO.Put_Line (Command & ": " & Error);
         Ada.Text_IO.New_Line;
      end if;
      Ada.Text_IO.Put_Line
        ("Usage: " & Command
         & " -P[project-file]"
         & " -X[scenario-variable]=[value]"
         & " [project-names]");
   end Print_Usage;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Project     : GPR2.Project.View.Object;
      Source_File : String;
      Output_File : in out Ada.Text_IO.File_Type)
   is
      procedure Put (S : String);
      --  Write the given string to Output_File

      function URL (U : LAL.Analysis_Unit) return String;
      --  If U belongs to the set of projects under consideration, return the
      --  relative URL to the highlighted source code for U. Otherwise, return
      --  an empty string.

      Unit       : constant LAL.Analysis_Unit :=
        LAL.Get_From_File (Ctx, Source_File);
      Highlights : Highlighter.Highlights_Holder
        (Highlighter.Token_Index (LAL.Token_Count (Unit)),
         Highlighter.Token_Index (LAL.Trivia_Count (Unit)));

      ---------
      -- Put --
      ---------

      procedure Put (S : String) is
      begin
         Ada.Text_IO.Put (Output_File, S);
      end Put;

      ---------
      -- URL --
      ---------

      function URL (U : LAL.Analysis_Unit) return String is
         Filename  : constant String := LAL.Get_Filename (U);
         Basename  : constant String := Ada.Directories.Simple_Name (Filename);
         Ambiguous : Boolean;
         Source    : constant GPR2.Build.Source.Object :=
           Project.Visible_Source (GPR2.Simple_Name (Basename), Ambiguous);
         pragma Unreferenced (Ambiguous);
      begin
         if not Source.Is_Defined
            or else not Projects.Contains (Source.Owning_View)
         then
            return "";
         end if;

         declare
            Prj_Dir : constant String := Ada.Directories.Compose
              ("..", String (Source.Owning_View.Name));
         begin
            return Ada.Directories.Compose (Prj_Dir, Basename & ".html");
         end;
      end URL;

      procedure Put_Tokens_HTML is new HTML.Put_Tokens (Put, URL);

   begin
      --  If there are any error, just print them on the standard error
      --  stream and abort. Otherwise, do our job.
      if LAL.Has_Diagnostics (Unit) then
         for D of LAL.Diagnostics (Unit) loop
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               LAL.Format_GNU_Diagnostic (Unit, D));
         end loop;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      --  Otherwise, create highlighting annotations and emit the HTML document

      Highlighter.Highlight (Unit, Highlights);

      Emit_HTML_Header
        (Output_File,
         String (Project.Name) & " - " & HTML.Escape (Source_File), "../");

      Put ("<div><a href=""../index.html"">Go back to the index</a></div>");
      Put_Tokens_HTML (Unit, Highlights, "utf-8", With_Xrefs => True);
      Put ("<div><a href=""../index.html"">Go back to the index</a></div>");

      Emit_HTML_Footer (Output_File);
   end Process_File;

   ----------------------
   -- Create_If_Needed --
   ----------------------

   procedure Create_If_Needed (Directory : String) is
   begin
      if not Ada.Directories.Exists (Directory) then
         Ada.Directories.Create_Directory (Directory);
      end if;
   end Create_If_Needed;

   ----------------------
   -- Emit_HTML_Header --
   ----------------------

   procedure Emit_HTML_Header
     (File          : in out Ada.Text_IO.File_Type;
      Title, Prefix : String)
   is
      use Ada.Text_IO;
      Escaped : constant String := HTML.Escape (Title);
   begin
      Put_Line (File, "<html><head>");
      Put_Line (File, "<meta http-equiv=""Content-Type"""
                & " content=""charset=utf-8"" />");
      Put_Line (File, "<title>" & Escaped & "</title>");
      Put_Line (File, "<link rel=""StyleSheet"" type=""text/css"" href="""
                & Prefix & CSS_Filename & """/>");
      Put_Line (File, "</head><body>");
      Put_Line (File, "<h1>" & Escaped & "</h1>");
   end Emit_HTML_Header;

   ----------------------
   -- Emit_HTML_Footer --
   ----------------------

   procedure Emit_HTML_Footer (File : in out Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line (File, "</body></html>");
   end Emit_HTML_Footer;

   ------------
   -- To_Set --
   ------------

   function To_Set
     (View : GPR2.Project.View.Object) return GPR2.Project.View.Set.Object is
   begin
      return Result : GPR2.Project.View.Set.Object do
         Result.Insert (View);
      end return;
   end To_Set;

   Output_Dir : XString;
   Index      : Ada.Text_IO.File_Type;

begin
   GNATCOLL.Traces.Parse_Config_File;

   if not Parse_Arguments then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   --  Create the analysis context for Libadalang
   UFP := Libadalang.Project_Provider.Create_Project_Unit_Provider
     (Prj_Tree, Prj_Tree.Root_Project);
   Ctx := LAL.Create_Context (Unit_Provider => UFP);

   --  Create the output directories, if needed
   declare
      Obj_Dir : constant String :=
        String (Prj_Tree.Root_Project.Object_Directory.Value);
   begin
      Output_Dir := To_XString (Ada.Directories.Compose (Obj_Dir, "ada2web"));
      Create_If_Needed (Obj_Dir);
      Create_If_Needed (Output_Dir.To_String);
   end;

   --  Create the file that will contain common CSS rules
   declare
      use Ada.Text_IO;

      procedure Put (S : String);
      F : File_Type;

      ---------
      -- Put --
      ---------

      procedure Put (S : String) is
      begin
         Put (F, S);
      end Put;

      procedure Put_CSS_Rules is new HTML.Put_CSS_Rules (Put);

   begin
      Create (F, Out_File,
              Ada.Directories.Compose (Output_Dir.To_String, CSS_Filename));

      Put_Line (F, "a {");
      Put_Line (F, "text-decoration: none;");
      Put_Line (F, "color: #"
                & HTML.Color_To_HTML (Colors.Default_Style.Text_Color)
                & ";");
      Put_Line (F, "}");
      Put_Line (F, "a:hover { text-decoration: underline; }");

      Put_Line (F, "span.line { display: block; }");
      Put_Line (F, "span.line:target { background-color: #"
                & HTML.Color_To_HTML (Colors.Default_Style.Selected_Bg_Color)
                & "; }");

      Put_Line (F, "body {");
      Put_Line (F, "color: #"
                & HTML.Color_To_HTML (Colors.Default_Style.Text_Color)
                & ";");
      Put_Line (F, "background-color: #"
                & HTML.Color_To_HTML (Colors.Default_Style.Bg_Color)
                & ";");
      Put_Line (F, "}");

      Put_CSS_Rules (Colors.Default_Style.Style);

      Close (F);
   end;

   --  Start the index file...
   Ada.Text_IO.Create
     (Index, Ada.Text_IO.Out_File,
      Ada.Directories.Compose (Output_Dir.To_String, "index.html"));
   Emit_HTML_Header (Index, String (Prj_Tree.Root_Project.Name), "");

   --  Go through each source file in each analyzed project to generate one
   --  HTML document of highlighted source code per source file.
   for P of Projects loop
      declare
         Sub_Dir   : constant String :=
           Ada.Directories.Compose (Output_Dir.To_String, String (P.Name));
         Src_Files : constant Filename_Vectors.Vector :=
           Source_Files (Prj_Tree, Root_Project, To_Set (P));
      begin
         Create_If_Needed (Sub_Dir);

         Ada.Text_IO.Put_Line
           (Index, "<h2>" & HTML.Escape (String (P.Name)) & "</h2>");
         Ada.Text_IO.Put_Line (Index, "<ul>");

         for F of Src_Files loop
            declare
               Src_Filename    : constant String :=
                 Ada.Directories.Simple_Name (To_String (F));
               HTML_Filename   : constant String := Src_Filename & ".html";
               Output_Filename : constant String :=
                 Ada.Directories.Compose (Sub_Dir, HTML_Filename);
               Output_File     : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Put_Line (Index, "<li><a href=""" & Output_Filename
                                     & """>" & Src_Filename & "</a>");

               Ada.Text_IO.Create
                 (Output_File, Ada.Text_IO.Out_File, Output_Filename);
               Process_File (P, To_String (F), Output_File);
               Ada.Text_IO.Close (Output_File);
            end;
         end loop;
         Ada.Text_IO.Put_Line (Index, "</ul>");
      end;
   end loop;

   Emit_HTML_Footer (Index);
   Ada.Text_IO.Close (Index);
end Ada2Web;
