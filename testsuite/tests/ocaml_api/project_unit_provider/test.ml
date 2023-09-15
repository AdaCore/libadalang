open Libadalang

let value_exn = function
  | Some x ->
      x
  | None ->
      raise (Invalid_argument "Some expected, got None")

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
    try ignore (GPRProject.load
                  ~scenario_vars:scenario_vars
                  ~target:target
                  ~runtime:runtime
                  project_file |> GPRProject.create_unit_provider ~project : UnitProvider.t) ;
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
    GPRProject.(load ~scenario_vars:[("SRC_DIR", src_dir)] "p.gpr"
                |> create_unit_provider)
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

let analysis_context () =
  let open GPRProject in
  let gpr = load ~scenario_vars:[("SRC_DIR", "src3")] "p.gpr" in
  create_analysis_context gpr

let test_gpr_project_context () =
  let ctx = analysis_context () in
  (* At this point gpr is out of scope, call GC.full_major to hopefully trigger
     a valgrind issue in case gpr has been gced (we want it to stay alive
     because the context uses it *)
  Gc.full_major () ;
  let u = AnalysisContext.get_from_file ctx "src3/a.ads" in
  let root =
    match AnalysisUnit.root u with
    | Some n ->
        n
    | None ->
        Format.printf "@[<v>Cannot get root node for file a.ads@ @]" ;
        exit 1
  in
  let name =
    AdaNode.find ObjectDecl root
    |> ObjectDecl.f_default_expr
    |> value_exn
    |> AdaNode.as_a Name
    |> value_exn
  in
  let ref = Name.p_referenced_decl name |> value_exn in
  Format.printf "%s referenced_decl is %s@."
    (AdaNode.image name)
    (AdaNode.image ref)


let () = test_src "src1" ; test_src "src2"; test_gpr_project_context ()
