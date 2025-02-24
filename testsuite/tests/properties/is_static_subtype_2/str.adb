--  Various checks for string types

procedure Str (A, B : Integer) is

   subtype S1 is String (1 .. 10);
   --% node.f_subtype.p_is_static_subtype()
   subtype S2 is String (A .. B);
   --% node.f_subtype.p_is_static_subtype()
   type S3 is new String (1 .. 10);
   --% node.f_type_def.f_subtype_indication.p_is_static_subtype()
   type S4 is new String (A .. B);
   --% node.f_type_def.f_subtype_indication.p_is_static_subtype()

   type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', '%');
   --% node.p_is_static_decl()
   type Roman is array (Positive range <>) of Roman_Digit;
   --% node.p_is_static_decl()
   type Roman2 is array (Positive range 1 .. 10) of Roman_Digit;
   --% node.p_is_static_decl()

   subtype R1 is Roman (1 .. 4);
   --% node.f_subtype.p_is_static_subtype()

   subtype R2 is Roman;
   --% node.f_subtype.p_is_static_subtype()

   type R3 is new Roman (1 .. 4);
   --% node.f_type_def.f_subtype_indication.p_is_static_subtype()

   type R4 is new Roman (A .. 4);
   --% node.f_type_def.f_subtype_indication.p_is_static_subtype()

begin
   null;
end Str;
