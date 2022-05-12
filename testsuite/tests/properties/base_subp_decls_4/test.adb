procedure Test is
   package Pkg is
      type I is limited interface;

      procedure IP (X : in out I) is abstract;
      procedure IQ (X : in out I) is abstract;

      protected type P is new I with
         overriding entry IP;
         --% node.p_base_subp_declarations()
         --% node.p_root_subp_declarations()

         overriding procedure IQ;
         --% node.p_base_subp_declarations()
         --% node.p_root_subp_declarations()
      end P;

      task type T is new I with
         overriding entry IP;
         --% node.p_base_subp_declarations()
         --% node.p_root_subp_declarations()

         overriding entry IQ;
         --% node.p_base_subp_declarations()
         --% node.p_root_subp_declarations()

         entry IR;
         --% node.p_base_subp_declarations()
         --% node.p_root_subp_declarations()
      end T;
   end Pkg;

   package body Pkg is
      protected body P is
         entry IP when True is
         begin
            null;
         end IP;

         procedure IQ is
         begin
            null;
         end IQ;
      end P;

      task body T is
      begin
         null;
      end T;
   end Pkg;
begin
   null;
end Test;
