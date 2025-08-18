procedure Test_Taint is
   type Rec is record
      F : Integer;
   end record;

   Str : String (1 .. 10);

   function Source return String
      with Taint_Source => (Source'Result => FILESYSTEM);
   pragma Test_Block;

   procedure Source (R : out Rec)
      with Taint_Source => (R.F => FILESYSTEM);
   pragma Test_Block;

   procedure Source
      with Taint_Source => (Str => FILESYSTEM);
   pragma Test_Block;


   function Sanitizer (S : String) return String
      with Taint_Sanitizer => (Sanitizer'Result => OS_COMMAND_INJECTION);
   pragma Test_Block;

   procedure Sanitizer (R : in out Rec)
      with Taint_Sanitizer => (R.F => OS_COMMAND_INJECTION);
   pragma Test_Block;


   procedure Sink (R : Rec)
      with Taint_Sink => (R.F => OS_COMMAND_INJECTION);
   pragma Test_Block;

   procedure Sink
      with Taint_Sink => (Str => OS_COMMAND_INJECTION);
   pragma Test_Block;

   --  Code is shared for all aspects, only test the alternatives with
   --  Taint_Source.

   procedure Source (Param1, Param2: out Rec)
      with Taint_Source => ((Param1, Param2) => FILESYSTEM);
   pragma Test_Block;

   procedure Source2 (Param1, Param2 : out Rec)
      with Taint_Source => (Param1 => FILESYSTEM, Param2 => NETWORK);
   pragma Test_Block;

   procedure Source3 (Param1, Param2 : out Rec)
      with Taint_Source => (
         Param1 => FILESYSTEM,
         Param2 => (FILESYSTEM, NETWORK)
      );
   pragma Test_Block;

   --  Now test the pragma version

   procedure Source_With_Pragma (Param : out Integer);
   pragma Taint_Source ((Param => FILESYSTEM));
   pragma Test_Statement;

   procedure Sink_With_Pragma (Param : Integer);
   pragma Taint_Sink ((Param => OS_COMMAND_INJECTION));
   pragma Test_Statement;

   procedure Sanitizer_With_Pragma (Param : in out Integer);
   pragma Taint_Sanitizer ((Param => FILESYSTEM));
   pragma Test_Statement;

   procedure Source_With_Pragma (Param1, Param2: Integer; B : Boolean);
   pragma Taint_Source (((Param1, Param2) => FILESYSTEM, B => NETWORK));
   pragma Test_Statement;
begin
   null;
end Test_Taint;
