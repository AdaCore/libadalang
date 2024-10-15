import libadalang as lal


for filename in ["no_such_file.txt", "invalid.txt", "small_ints.txt"]:
    print(f"== {filename} ==")
    print("")

    try:
        info = lal.TargetInformation(filename)
    except lal.InvalidInput as exc:
        print(f"Got an exception ({type(exc).__name__}):")
        print(f"  {exc}")
        print("")
        continue

    ctx = lal.AnalysisContext()
    u = ctx.get_from_file("pkg.ads")
    decl = u.root.f_body.f_item.f_public_part.f_decls[0]
    expr = decl.f_expr

    print("Before set_target_information")
    print(f"{decl} = {expr.p_eval_as_int}")
    print("")

    ctx.set_target_information(info)

    print("After set_target_information")
    print(f"{decl} = {expr.p_eval_as_int}")
    print("")


print("Done")
