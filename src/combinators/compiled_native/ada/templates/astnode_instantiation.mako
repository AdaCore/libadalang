new ${cls.name()}_Record${"'({0})".format(", ".join(
            "{0} => {1}".format(f.name, arg)
            for f, arg in zip(cls.get_fields(), args))) if args else ""}