with Ada.Text_IO; use Ada.Text_IO;

package body D is

   --------
   -- F2 --
   --------

   function F2 return T1 is
   begin
      Put_Line ("F2 T1");
      return T1'(null record);
   end F2;

   ---------
   -- "+" --
   ---------

   function "+" (X, Y : in T1) return Int is
   begin
      return 1;
   end "+";

   package Der is
      type T2 is new T1 with null record;

      function F2 return T2;
   end Der;

   package body Der is
      function F2 return T2 is
      begin
         Put_Line ("F2 T2");
         return T2'(null record);
      end F2;
   end Der;

   --------
   -- P2 --
   --------

   procedure P2 (XC : in T1'Class; Y : in Int) is
      Res : Int;
   begin

      --  operator calls in a for loop

      for I in 1 .. 2 loop
         --  operator calls in an assignment
         Res := X + F2;  -- should be flagged twice
         --% node.f_expr.p_is_dispatching_call()
         --% node.f_expr.f_right.p_is_call
         --% node.f_expr.f_right.p_is_dispatching_call()
         Res := XC + F2; -- should NOT be flagged
         --% node.f_expr.p_is_dispatching_call()
         --% node.f_expr.f_right.p_is_call
         --% node.f_expr.f_right.p_is_dispatching_call()
      end loop;
   end P2;

   procedure Main is
   begin
      P2 (Der.F2, 3);
   end Main;
end D;
