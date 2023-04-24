procedure Discr is
   type Person_Group is (Student, Faculty);
   type Person (Group : Person_Group := Student) is record
      Name : String (1 .. 3);
      case Group is
         when Student =>
            Gpa : Float range 0.0 .. 4.0;
         when Faculty => 
            Pubs : Integer;
      end case;
   end record;
   --% node.find(lal.DiscriminantSpec).p_is_constant_object

   S1 : Person;
   S2 : Person (Group => Student);
   S3 : constant Person := (Student, "zzz", 0.0);
begin
   S1 := (Group => Student, Name => "aaa", Gpa => 0.0);
   --% decl = node.find(lal.AggregateAssoc).f_designators[0].p_referenced_decl()
   --% decl.p_is_constant_object
   S1 := (Group => Faculty, Name => "bbb", Pubs => 10);
   --% decl = node.find(lal.AggregateAssoc).f_designators[0].p_referenced_decl()
   --% decl.p_is_constant_object
   S2.Name := S3.Name;
   --% node.f_expr.p_is_constant
end Discr;
