with Ada; use Ada;
with Ada.Containers.Doubly_Linked_Lists;

private
with Containers;

package Pkg is
   package Socket_Lists is new Containers.Doubly_Linked_Lists (Integer);
   pragma Test_Statement; -- OK
end Pkg;
