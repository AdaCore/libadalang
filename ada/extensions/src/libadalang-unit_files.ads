private with GNAT.Semaphores;

package Libadalang.Unit_Files is
private

   GPR_Lock : GNAT.Semaphores.Binary_Semaphore
     (Initially_Available => True, Ceiling => GNAT.Semaphores.Default_Ceiling);
   --  Lock to serialize all calls to GNATCOLL.Projects, which is not
   --  thread-safe. Hopefully we will be able to get rid of this lock when
   --  moving to Libgpr2.

end Libadalang.Unit_Files;
