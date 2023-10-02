with Ada; use Ada;
with Ada.Containers.Doubly_Linked_Lists;

private
with Containers;

package Pkg_2 is
   package Socket_Lists is new Containers.Doubly_Linked_Lists (Integer);
   pragma Test_Statement; -- OK
private
   Tmp : Integer;
end Pkg_2;
