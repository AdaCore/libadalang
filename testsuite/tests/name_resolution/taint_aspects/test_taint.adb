procedure Test_Taint is
   type Rec is record
      F : Integer;
   end record;

   Str : String (1 .. 10);

   function Source return String
      with Taint_Source => (Source'Result, "my_source");
   pragma Test_Block;

   procedure Source (R : out Rec)
      with Taint_Source => (R.F, "my_source");
   pragma Test_Block;

   procedure Source
      with Taint_Source => (Str, "my_source");
   pragma Test_Block;


   function Sanitizer (S : String) return String
      with Taint_Sanitizer => (Sanitizer'Result, "my_sink");
   pragma Test_Block;

   procedure Sanitizer (R : in out Rec)
      with Taint_Sanitizer => (R.F, "my_sink");
   pragma Test_Block;


   procedure Sink (R : Rec)
      with Taint_Sink => (R.F, "my_sink");
   pragma Test_Block;

   procedure Sink
      with Taint_Sink => (Str, "my_sink");
   pragma Test_Block;

   --  Code is shared for all aspects, only test the alternatives with
   --  Taint_Source.

   procedure Source (Param : out Rec)
      with Taint_Source => (Kind => "my_source", Target => Param);
   pragma Test_Block;

   Source_Kind : constant String := "my_source";

   procedure Source2 (Param : out Rec)
      with Taint_Source => (Kind => Source_Kind, Target => Param);
   pragma Test_Block;

   procedure Source (Param1, Param2 : out Rec)
      with Taint_Source => ((Param1, Param2), "my_source");
   pragma Test_Block;
begin
   null;
end Test_Taint;
