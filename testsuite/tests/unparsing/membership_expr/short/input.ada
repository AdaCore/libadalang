function Is_Expected_Parent_Kind (Kind_1, Kind_2: Ada_Node_Kind_Type) return Boolean
is (Kind_1 in Ada_Package_Body and Kind_2 in Ada_Subp_Body);
