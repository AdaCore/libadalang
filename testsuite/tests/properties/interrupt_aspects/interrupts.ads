with Ada.Interrupts;
package Interrupts is

   function Get_Interrupt return Ada.Interrupts.Interrupt_ID with Import;

   protected type Pr_T is
      procedure Handler1_Hdl with Interrupt_Handler;
      --% node.p_has_aspect("interrupt_handler")
      procedure Handler1 with Interrupt_Handler;                --  FLAG
      --% node.p_has_aspect("interrupt_handler")

      procedure Handler2;                                       --  FLAG
      --% node.p_has_aspect("interrupt_handler")
      pragma Interrupt_Handler (Handler2);

      procedure Handler2_Hdl;
      --% node.p_has_aspect("interrupt_handler")
      pragma Interrupt_Handler (Handler2_Hdl);

      procedure Handler4;
      --% node.p_has_aspect("interrupt_handler")
      --% node.p_has_aspect("attach_handler")

      procedure Handler5_Hdl with Attach_Handler => Get_Interrupt;
      --% node.p_has_aspect("attach_handler")
      procedure Handler5 with Attach_Handler => Get_Interrupt;  --  FLAG
      --% node.p_has_aspect("attach_handler")

      procedure Handler6;                                       --  FLAG
      --% node.p_has_aspect("attach_handler")
      pragma Attach_Handler (Handler6, Get_Interrupt);

      procedure Handler6_Hdl;
      --% node.p_has_aspect("attach_handler")
      pragma Attach_Handler (Handler6_Hdl, Get_Interrupt);

   private
      B : Boolean;
   end Pr_T;
end Interrupts;
