import libadalang as lal


ctx = lal.AnalysisContext()

units = [
    ctx.get_from_file(f)
    for f in ('pkg.ads', 'pkg-child.ads', 'pkg-child-grand_child.ads')
]

for u in units:
    for dn in u.root.findall(lal.DefiningName):
        print("in defining name {}".format(dn))

        for n in dn.findall(lal.Name):
            if n.p_is_defining:
                print("{0:30} is     defining".format(n.text))
            else:
                print("{0:30} is not defining".format(n.text))

        print("")
