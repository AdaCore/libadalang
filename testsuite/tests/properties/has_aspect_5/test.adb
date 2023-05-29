procedure Test is

   package P with
      Obsolescent
   is
   end P;
   --% node.p_has_aspect ("Obsolescent")

   package body P is
   end P;
   --% node.p_has_aspect ("Obsolescent")

   package Q is
      pragma Obsolescent;

      procedure P;
      --% node.p_has_aspect ("Obsolescent")
   end Q;
   --% node.p_has_aspect ("Obsolescent")

   package body Q is
      procedure P is null;
      --% node.p_has_aspect ("Obsolescent")
   end Q;
   --% node.p_has_aspect ("Obsolescent")

   package R is
      pragma Obsolescent (R);
   end R;
   --% node.p_has_aspect ("Obsolescent")

   package body R is
   end R;
   --% node.p_has_aspect ("Obsolescent")

   package S is
   end S;
   --% node.p_has_aspect ("Obsolescent")

   package T is
   end T;
   --% node.p_has_aspect ("Obsolescent")
   pragma Obsolescent (T);

   package U is
      procedure P;
      --% node.p_has_aspect ("Obsolescent")
      pragma Obsolescent;
   end U;
   --% node.p_has_aspect ("Obsolescent")

   package body U is
      procedure P is null;
      --% node.p_has_aspect ("Obsolescent")
   end U;
   --% node.p_has_aspect ("Obsolescent")

   package qq is
      procedure q2;
      --% node.p_has_aspect ("Obsolescent")
      pragma Obsolescent ("use q2new instead");

      procedure q3;
      --% node.p_has_aspect ("Obsolescent")

      type R is new integer;
      --% node.p_has_aspect ("Obsolescent")
      pragma Obsolescent
        (Entity  => R,
         Message => "use RR in Ada 2005",
         Version => Ada_05);

      type M is record
         F1 : Integer;
         --% node.p_has_aspect ("Obsolescent")
         F2 : Integer;
         --% node.p_has_aspect ("Obsolescent")
         pragma Dummy;
         pragma Obsolescent;
         F3 : Integer;
      end record;
      --% node.p_has_aspect ("Obsolescent")

      type E is (a, bc, 'd', quack);
      --% node.p_has_aspect ("Obsolescent")
      --% node.f_type_def.f_enum_literals[0].p_has_aspect ("Obsolescent")
      --% node.f_type_def.f_enum_literals[1].p_has_aspect ("Obsolescent")
      --% node.f_type_def.f_enum_literals[2].p_has_aspect ("Obsolescent")
      --% node.f_type_def.f_enum_literals[3].p_has_aspect ("Obsolescent")
      pragma Obsolescent (Entity => bc);
      pragma Obsolescent (Entity => 'd');

      function "+"
        (a, b : character) return character;
      --% node.p_has_aspect ("Obsolescent")
      pragma Obsolescent (Entity => "+");
   end;
   --% node.p_has_aspect ("Obsolescent")

   package body qq is
      procedure q2 is null;
      --% node.p_has_aspect ("Obsolescent")
      procedure q3 is null;
      --% node.p_has_aspect ("Obsolescent")

      function  "+"
        (a, b : character) return character is
      begin
        return 'a';
      end;
      --% node.p_has_aspect ("Obsolescent")
   end;
   --% node.p_has_aspect ("Obsolescent")

begin
   null;
end Test;
