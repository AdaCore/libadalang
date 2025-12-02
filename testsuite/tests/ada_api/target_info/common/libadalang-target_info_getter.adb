with Libadalang.Target_Info_Native; use Libadalang.Target_Info_Native;

package body Libadalang.Target_Info_Getter is

   ---------
   -- Get --
   ---------

   function Get (N : String) return Target_Information is
      Found : Boolean;
   begin
      return Result : Target_Information do
         Get_Builtin_Target_Info (N, Result, Found);
         if not Found then
            raise Program_Error;
         end if;
      end return;
   end Get;

end Libadalang.Target_Info_Getter;
