package body Pkg is

   protected body Regular is
      procedure Set (I : Integer) is
      begin
         State := I;
      end Set;

      function Get return Integer is
      begin
         return State;
      end Get;
   end;

   protected body P_Rec is separate;

end Pkg;
