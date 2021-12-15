with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Choice is
   Ex : exception;
begin
   null;
exception
   when E : Ex =>
      Put_Line(
          Ada.Exceptions.Exception_Message(
              E
              --% node.p_referenced_decl().p_is_constant_object
          )
       );
      raise;
end Choice;
