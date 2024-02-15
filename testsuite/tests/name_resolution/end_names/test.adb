procedure Test is
begin
   Local_Block:
   declare
      procedure P;

      procedure P is
      begin
         null;
      end P;
      --% node.f_end_name.p_referenced_decl()
   begin
      null;
   end Local_Block;
   --% node.f_end_name.p_referenced_decl()

   Loop_Name_1:
   loop
      Loop_Name_2:
      loop
         null;
      end loop Loop_Name_2;
      --% node.f_end_name.p_referenced_decl()
   end loop Loop_Name_1;
   --% node.f_end_name.p_referenced_decl()
end Test;
