package Missing_Component is

   type T1 is record
      X1 : Integer;
      --  Placeholder
      X3 : Integer;
   end record;

   type T2 (B : Boolean) is record
      case B is
         when others => null;
      end case;
   end record;

   type T3 (B : Boolean) is record
      case B is
         when others => I : Integer;
      end case;
   end record;

end Missing_Component;
