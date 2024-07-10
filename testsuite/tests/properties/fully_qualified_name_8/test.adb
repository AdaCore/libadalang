procedure Test is
   generic
   package Gen is
      procedure Visible;
   end Gen;

   package body Gen is
      procedure Inner is
      begin
         null;
      end Inner;

      procedure Visible is
      begin
         Inner;
      end Visible;

   end Gen;

   package Inst is new Gen;
   --% gen_body = node.p_designated_generic_decl.p_body_part
   --% gen_body.p_fully_qualified_name
   --% inner = gen_body.findall(lal.SubpBody)[0]
   --% visible = gen_body.findall(lal.SubpBody)[1]
   --% inner.p_fully_qualified_name
   --% visible.p_fully_qualified_name
begin
   Inst.Visible;
end Test;

