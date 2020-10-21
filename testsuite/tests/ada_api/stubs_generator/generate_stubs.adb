with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Rewriting; use Libadalang.Rewriting;

procedure Generate_Stubs is
   Input_File : constant String := Argument (1);

   Charset   : constant String := "ISO-8859-1";
   Ctx       : Analysis_Context := Create_Context (Charset);

   Spec_Unit : constant Analysis_Unit := Get_From_File (Ctx, Input_File);
   --  Unit to contain the package specification for which we will generate
   --  stubs subprogram bodies.

   Spec_List : Ada_Node_List;
   --  List to contain the declaration for which we will generate stubs

   RH : Rewriting_Handle := Start_Rewriting (Ctx);
   --  Rewriting handle, owning all rewriting data

   function Extract_Spec_List return Ada_Node_List;
   --  If Spec_Unit contains a package specification, return the list of nodes
   --  thata constitutes its public part. Oterwise, return No_Ada_Node_List.

   -----------------------
   -- Extract_Spec_List --
   -----------------------

   function Extract_Spec_List return Ada_Node_List is
      N : Ada_Node := Root (Spec_Unit);
   begin
      if N.Kind /= Ada_Compilation_Unit then
         return No_Ada_Node_List;
      end if;

      N := N.As_Compilation_Unit.F_Body;
      if N.Kind /= Ada_Library_Item then
         return No_Ada_Node_List;
      end if;

      N := N.As_Library_Item.F_Item.As_Ada_Node;
      if N.Kind /= Ada_Package_Decl then
         return No_Ada_Node_List;
      end if;

      return N.As_Package_Decl.F_Public_Part.F_Decls;
   end Extract_Spec_List;

begin

   --  Make sure we could parse the input source file and get the list of
   --  declarations for which we will generate stubs.

   if Has_Diagnostics (Spec_Unit) then
      for D of Diagnostics (Spec_Unit) loop
         Put_Line (Format_GNU_Diagnostic (Spec_Unit, D));
      end loop;
      return;
   end if;

   Spec_List := Extract_Spec_List;
   if Spec_List = No_Ada_Node_List then
      Put_Line (Input_File & ": package spec expected");
      return;
   end if;

   --  Walk through this list, generating stubs for the subprogram declarations
   --  we wind on the way.

   for Decl of Spec_List.Children loop
      if Decl.Kind = Ada_Subp_Decl then
         declare
            SD   : constant Subp_Decl := Decl.As_Subp_Decl;
            SS   : constant Subp_Spec := SD.F_Subp_Spec;
            Name : constant Defining_Name := SS.F_Subp_Name;

            Template : constant Text_Type :=
              (if SS.F_Subp_Kind.Kind = Ada_Subp_Kind_Function
               then "{} {} is begin return (raise Program_Error); end {};"
               else "{} {} is begin null; end {};");
            Body_Stub : constant Node_Rewriting_Handle := Create_From_Template
              (Handle    => RH,
               Template  => Template,
               Arguments =>
                 (Handle (SD.F_Overriding), Handle (SS), Handle (Name)),
               Rule      => Subp_Body_Rule);
            --  Create the tree of nodes that will constitute the body stub.
            --  Instead of creating all nodes manually, one by one, we just
            --  give a string template, several nodes to fill in holes, and let
            --  the rewriting machinery do its magic.
         begin
            Put_Line ("===");
            Put_Line (Encode (Unparse (Body_Stub), Charset));
            New_Line;
         end;
      end if;
   end loop;

   --  Free resources: data for rewriting session, and then the whole analysis
   --  context.

   Abort_Rewriting (RH);

   Put_Line ("generate_stubs.adb: Done.");
end Generate_Stubs;
