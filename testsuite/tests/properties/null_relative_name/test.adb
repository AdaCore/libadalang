procedure Test is
begin
   null;
exception
   when A =>
      --% node.p_relative_name
      --% node.p_relative_name_text
      null;

   when B : C =>
      --% node.p_relative_name
      --% node.p_relative_name_text
      null;
end Test;
