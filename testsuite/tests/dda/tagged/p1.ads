package P1 is

   type R1_Parent is tagged null record;

   type R2_Parent is tagged record
      X1 : Integer;
      X2 : Integer;
   end record;

   type R3_Parent (N : Natural) is tagged record
      X1 : String (1 .. N);
   end record;

   type R4_Parent (B : Boolean) is tagged record
      case B is
         when False =>
            null;
         when True =>
            X1 : Integer;
      end case;
   end record;

   type R5_Parent is tagged private;

   type R6_Parent is private;

   subtype R8_Parent is R3_Parent (2);

private

   type R5_Parent is tagged record
      X1 : Integer;
   end record;

   type R6_Parent is tagged record
      X1, X2 : Boolean;
   end record;

end P1;
