procedure Initialize
  (Self     : in out Default_Message_Handler'Class;
   Handler  :
     not null access LSP.Server_Message_Visitors.Server_Message_Visitor'Classss;
   Priority : LSP.Server_Jobs.Job_Priority := LSP.Server_Jobs.Fence);
