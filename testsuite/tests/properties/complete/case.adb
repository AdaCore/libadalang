procedure Pcase is
   type Week is (M, Tu, W, Th, F, Sa, Su);

   procedure P1 (D : Week) is
   begin
      case D is
         when
         --% list(node.f_choices.p_complete)
      end case;
   end P1;

   procedure P2 (D : Week) is
   begin
      case D is
         when T
         --% list(node.p_complete)
      end case;
   end P2;

   procedure P3 (D : Week) is
   begin
      case D is
         when M |
      end case;
      --% list(node.f_alternatives[0].f_choices.p_complete)
   end P3;

   procedure P4 (D : Week) is
   begin
      case D is
         when M ..
         --% list(node.f_choices.p_complete)
      end case;
   end P4;

begin
   null;
end Pcase;
