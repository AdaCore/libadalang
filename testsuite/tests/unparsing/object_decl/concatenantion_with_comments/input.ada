Attribute_Value : constant Wide_Wide_String :=
     "(" & HTML.Unquoted_Attribute_Value & ")|" &  --  2 group
     "'([^']*)'" &  --  3 group is Single_Quoted_Attribute_Value
     """([^""]*)""";  --  4 group is Double_Quoted_Attribute_Value
