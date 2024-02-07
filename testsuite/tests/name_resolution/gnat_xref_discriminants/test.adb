procedure Test is
   package P is
      type R (D : Integer) is private;
      type S (D, E : Integer) is private;
   private
      type R (D : Integer) is record F : Float; end record;
      type S (D, E : Integer) is record F : Float; end record;
   end P;
begin
   null;
end Test;
