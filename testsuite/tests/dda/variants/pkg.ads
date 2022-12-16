--  Check the handling of record variant parts in basic cases

package Pkg is

   type Enum_Type is (A, B, C, D);

   type Rec_1 (Disc : Enum_Type) is record
      X1 : Boolean;

      case Disc is
         when A =>
            X2 : Integer;
            X3 : Character;

         when B | C =>
            X4 : Character;

            case Disc is
               when A =>
                  X5 : Integer;
               when B =>
                  X6 : Character;
               when C | D =>
                  X7 : Boolean;
            end case;

         when D =>
            X8 : Integer;
      end case;
   end record;
   pragma Test (A);
   pragma Test (B);
   pragma Test (C);
   pragma Test (D);

   type Rec_2 (N1, N2 : Natural) is record
      S1 : String (1 .. N1);
      S2 : String (1 .. N2);
   end record;
   pragma Test (0, 0);
   pragma Test (10, 20);
   pragma Test (-10, -20);

   type Rec_3 (N : Natural) is record
      case N is
         when 0 .. 9 | 90 .. 99 =>
            X1 : Integer;
         when 900 | 902 .. 999 =>
            X2 : Integer;
         when others =>
            null;
      end case;
   end record;
   pragma Test (0);
   pragma Test (9);
   pragma Test (10);
   pragma Test (89);
   pragma Test (90);
   pragma Test (91);
   pragma Test (99);
   pragma Test (100);
   pragma Test (899);
   pragma Test (900);
   pragma Test (901);
   pragma Test (902);
   pragma Test (903);
   pragma Test (998);
   pragma Test (999);
   pragma Test (1000);

end Pkg;
