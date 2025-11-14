package Ifaces is
   type Iface is interface;
   procedure Print (Self : Iface) is abstract;

   type T is new Iface with record
      B : Boolean;
   end record;
   overriding procedure Print (Self : T) is null;
end Ifaces;
