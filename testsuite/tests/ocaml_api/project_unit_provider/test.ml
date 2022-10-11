open Libadalang

let format_exc_message msg =
  (* For exceptions with no explicit message (e.g. Invalid_Project exceptions
   * from gnatcoll-projects.adb), hide the line number, which is out of our
   * control. *)
  Str.replace_first (Str.regexp "\\([a-z_-]+\\.adb\\):[0-9]+") "\\1:XXX" msg

let () =
  let load_project
        ?(project = "")
        ?(scenario_vars = [])
        ?(target = "")
        ?(runtime = "")
        project_file
  =
    try ignore (UnitProvider.for_project
                  ~project:project
                  ~scenario_vars:scenario_vars
                  ~target:target
                  ~runtime:runtime
                  project_file : UnitProvider.t) ;
        ignore (GPRProject.load
                  ~scenario_vars
                  ~target
                  ~runtime
                  project_file |>
                GPRProject.create_unit_provider : UnitProvider.t)
    (* These exceptions come from GNATCOLL and contain no message but a
     *  reference to a sloc in gnatcoll-project.adb, so not worth testing. *)
    with InvalidProject s ->
      Format.printf "@[<v>got InvalidProject %s@ @ @]" (format_exc_message s)
  in
  (* Try to load a project file that is not valid *)
  load_project "invalid.gpr" ;
  (* Try to load a project file that does not exist *)
  load_project "does_not_exist.gpr" ;
  (* Try to load a project that does not exist in the loaded tree *)
  load_project
    ~project:"does_not_exist"
    ~scenario_vars:[("SRC_DIR", "src1")]
    "p.gpr"

(* Test that the unit provider correclty works and Libadalang is able to
 * get the reference of a node that is declared in another unit *)

let test_src src_dir =
  let unit_provider =
    UnitProvider.for_project ~scenario_vars:[("SRC_DIR", src_dir)] "p.gpr"
  in
  let ctx = AnalysisContext.create ~unit_provider () in
  let filename = "p2.ads" in
  let u =
    AnalysisContext.get_from_file ctx (Filename.concat src_dir filename)
  in
  let root =
    match AnalysisUnit.root u with
    | Some n ->
        n
    | None ->
        Format.printf "@[<v>Cannot get root node for file %s@ @]" filename ;
        exit 1
  in
  let subtype_indication = AdaNode.find SubtypeIndication root in
  let matching_nodes =
    SubtypeIndication.f_name subtype_indication |> Expr.p_matching_nodes
  in
  let pp_node fmt node =
    Format.pp_print_string fmt (AdaNode.image node)
  in
  Format.printf "@[<v>For SRC_DIR=%s@ @[<v 2>%a resolves to:@ %a@]@ @]" src_dir
    pp_node subtype_indication
    (Format.pp_print_list pp_node)
    matching_nodes

let () = test_src "src1" ; test_src "src2"
