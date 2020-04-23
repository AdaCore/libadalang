procedure Test is
   type R (A : Integer; B : Integer) is tagged limited record
      X : Integer;
      case A is
         when 1 .. 10 =>
            Y_1 : Integer;
            case B is
               when 2 .. 8 =>
                  Z_1 : Integer;
               when others => null;
            end case;
         when 11 .. 20 =>
            Y_2 : Integer;
            case B is
               when 2 .. 8 =>
                  Z_2 : Integer;
               when others => null;
            end case;
         when others =>
            null;
      end case;
   end record;

   type S is new R with record
      V : Integer;
   end record;

   type T (A : Integer; B : Integer) is new R (A, B) with record
      W : Integer;
      case A is
         when 1 .. 10 =>
            M : Integer;
         when others =>
            null;
      end case;
   end record;
   --  This case is not handled accurately: we output some shapes that are
   --  infeasible in practice. Indeed, since the condition for M to exist
   --  is the same as the condition for Z_1 to exist, returned shapes should
   --  either include both or none of them. However, our approximation here
   --  will also include cases where M appears but not Z_1 and conversely.
begin
   null;
end Test;
