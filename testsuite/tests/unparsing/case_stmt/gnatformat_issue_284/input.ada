procedure Main is
begin
   case Choice is
      when Short => First; Second;
      when Medium_Choice => Action;
      when others => null;
   end case;

   case Choice is
      when Short => First; Second;
      when Medium_Choice => Action;
      when A_Much_Longer_Choice =>
         case Nested_Choice is
            when A => Nested_Action;
            when A_Longer_Nested_Choice => null;
            when others => null;
         end case;
      when others => null;
   end case;
end Main;
