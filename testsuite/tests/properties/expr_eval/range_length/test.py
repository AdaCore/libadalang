import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("main.adb")


def ar_name(ar):
    return str(ar) + " (" + ar.f_prefix.text + "):"


for ar in u.root.findall(lal.AttributeRef):
    if ar.f_attribute.p_name_is("range_length"):
        print("Evaluation of", ar_name(ar))
        try:
            print("=>", ar.p_eval_as_int)
        except lal.PropertyError:
            if not ar.f_prefix.p_is_static_expr():
                print("=> can't evaluate non static range")
            else:
                raise
        print("")


print("Done")
