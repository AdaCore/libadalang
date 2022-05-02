procedure Foo is

   type Kind is (Bool, Int);

   type T (K : Kind := Bool) is record
      case K is
         when Bool => B : Boolean;
         when Int  => I : Integer;
      end case;
   end record;

   function Copy (V : T) return T is
     (case V.K is
      when Bool => (V with delta B => V.B),
      when Int  => (V with delta I => V.I));
   --% node.findall(lambda n: n.text == "B")[1]
   --% node.findall(lambda n: n.text == "B")[1].p_referenced_decl()

   V1 : T := (K => Bool, B => False);
   V2 : T;
begin
   V2 := Copy (V1);
end Foo;
