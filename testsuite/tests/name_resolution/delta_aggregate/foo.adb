procedure Foo is

   type Kind is (Bool, Int);

   type T (K : Kind := Bool) is record
      case K is
         when Bool => B : Boolean;
         when Int  => I : Integer;
      end case;
   end record;

   type Date is record
      Day   : Integer range 1 .. 31;
      Month : Integer;
      Year  : Integer range 0 .. 4000;
   end record;

   type Vector is array(Integer  range <>) of Float;

   function Copy (V : T) return T is
     (case V.K is
      when Bool => (V with delta B => V.B),
      when Int  => (V with delta I => V.I));
   pragma Test_Statement;

   V1 : T := (K => Bool, B => False);
   V2 : T;

   procedure Twelfth (D : in out Date)
   with Post => D = (D'Old with delta Day => 12);
   pragma Test_Block;

   procedure The_Answer (V : in out Vector;
                         A, B : in Integer)
   with Post => V = (V'Old with delta A .. B => 42.0, V'First => 0.0);
   pragma Test_Block;

   type Histo_Date is new Date with
      record
         Histo : String;
      end record;

   procedure Twelfth_Histo (D : in out Histo_Date)
   with Post => D = (D'Old with delta Day => 12);
   pragma Test_Block;

   procedure Twelfth_Histo_Ric (D : in out Histo_Date)
   with Post => D = (D'Old with delta Day => 12, Histo => "RIC");
   pragma Test_Block;

begin
   V2 := Copy (V1);
end Foo;
