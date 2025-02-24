procedure Predicate95 is
   --  These tests are for Expr.is_predicate_static (called by the public
   --  property SubtypeIndication.is_static_subtype). The property should return
   --  whether an expression used in a Predicate pragma is predicate-static or
   --  not. Predicate pragma is an Ada95/GNAT extension that can be used in
   --  place of the Ada2012 Static_Predicate/Dynamic_Predicate aspects (RM
   --  3.2.5).

   --  NOTE: we don't need to test Static_Predicate/Dynamic_Predicate
   --  expressions because libadalang won't run any check for them but simply
   --  rely on the fact the code is valid, and therefore, used expressions are
   --  known to be static or not regarding the aspect used.

   function Id (I : Integer) return Integer
   is (I);

   type E is (A, B, C, D);


   -- All subtypes below are static because Predicate check expr is
   -- predicate-static.
   subtype S1 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype S2 is E range A .. C;
   --% node.f_subtype.p_is_static_subtype()
   subtype S3 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype S4 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype S5 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype S6 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype S7 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype S8 is S7;
   --% node.f_subtype.p_is_static_subtype()


   -- All subtypes below are not static because Predicate check expr is
   -- not predicate-static.
   subtype D1 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype D2 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype D3 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype D4 is Integer;
   --% node.f_subtype.p_is_static_subtype()
   subtype D5 is D4;
   --% node.f_subtype.p_is_static_subtype()

   -- Not static even if S9's Predicate is static because predicates are
   -- inherited (inherits from D4 predicate through D5) and are anded together.
   subtype S9 is D5;
   --% node.f_subtype.p_is_static_subtype()


   --  Static predicates

   pragma Predicate (Entity => S1, Check => S1 in 1 .. 10);
   pragma Predicate (Entity => S2, Check => S2 in A .. B);
   pragma
     Predicate
       (Entity => S3,
        Check =>
          (case S3 is
             when 1 .. 3 => true,
             when others => false));
   pragma Predicate (Entity => S4, Check => S4 > 8);
   pragma
     Predicate
       (Entity => S5,
        Check => ((S5 > 8) and (8 < S5)) or ((S5 in 1 .. 2) and (not true)));
   pragma Predicate (Entity => S6, Check => 3 + 3 > 3);
   pragma
     Predicate (Entity => S7, Check => S7 in 1 .. 10 or else S7 in 30 .. 40);
   pragma Predicate (Entity => S9, Check => true);

   --  Dynamic predicates

   pragma Predicate (Entity => D1, Check => D1 in 1 .. Id (10));
   pragma
     Predicate
       (Entity => D2,
        Check =>
          (case D2 is
             when 1 .. 3 => (if Id (0) > 0 then true else false),
             when others => false));
   pragma Predicate (Entity => D3, Check => D3 > Id (8));
   pragma Predicate (Entity => D4, Check => Id (8) > 2);

begin
   null;
end;
