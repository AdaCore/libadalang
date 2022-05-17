procedure Test is
   package Pkg is
      X, Y : constant Integer;
      --% node.f_ids[0].p_is_ghost_code
      --% node.f_ids[1].p_is_ghost_code
   private
      X : constant Integer := 1;
      Y : constant Integer := 2;
   end Pkg;

   package Pkg_2 is
      X : constant Integer with Ghost;
      --% node.p_is_ghost_code
      Y : constant Integer;
      --% node.p_is_ghost_code
   private
      X, Y : constant Integer := 1;
      --% node.f_ids[0].p_is_ghost_code
      --% node.f_ids[1].p_is_ghost_code
   end Pkg_2;

   package Pkg_3 is
      X, Y : Integer;
      A, B : Integer with Ghost;
      procedure Foo;
   end Pkg_3;

   package body Pkg_3 is
      procedure Foo is
      begin
         X := X + 1;
         --% node.p_is_ghost_code
         B := B + 1;
         --% node.p_is_ghost_code
      exception
         when Constraint_Error =>
            Y := Y + 1;
            --% node.p_is_ghost_code
      end Foo;
   end Pkg_3;

   generic
      type T is private;
   package Pkg_4 is
   end Pkg_4;
   --% node.p_is_ghost_code
   --% node.parent.p_is_ghost_code

   generic
      type T is private;
   package Pkg_5 with Ghost is
   end Pkg_5;
   --% node.p_is_ghost_code
   --% node.parent.p_is_ghost_code
begin
   null;
end Test;
