procedure Foo
  with Modifies =>
    (-- Before the first clause
     G -- After the objects
       when -- Before the guard
         Some_Condition, -- After the guard

     -- Before an unguarded clause
     H,
     K when Some_Very_Long_Condition_Name and then Another_Very_Long_Condition_Name and then Yet_Another_Long_Condition_Name,
     -- Before the last clause
     (I, J) when Another_Condition -- After the last guard
    );
