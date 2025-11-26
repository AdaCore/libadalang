"""
Test suite for UnitProvider.from_callback()

This tests the callback-based unit provider which allows Python code to
resolve unit names to filenames dynamically.
"""
import libadalang as lal


SPEC = lal.AnalysisUnitKind.unit_specification
BODY = lal.AnalysisUnitKind.unit_body


def test_basic_callback():
    """Test basic callback resolution with simple mapping."""
    print("== Test: Basic callback resolution ==")
    print("")

    # Create a simple mapping
    file_map = {
        ("pkg", "spec"): "pkg.ads",
        ("pkg", "body"): "pkg.adb",
        ("foo", "spec"): "foo.ads",
    }

    def resolver(name, kind):
        result = file_map.get((name, kind))
        if result:
            print(f"  Callback: {name} ({kind}) -> {result}")
        else:
            print(f"  Callback: {name} ({kind}) -> None")
        return result

    provider = lal.UnitProvider.from_callback(resolver)
    ctx = lal.AnalysisContext(unit_provider=provider)

    # Test successful resolution
    unit = ctx.get_from_provider("pkg", SPEC)
    if unit.root:
        print(f"  Success: pkg (spec) -> {unit.root.kind_name}")
    else:
        print(f"  Error: pkg (spec) failed to load")
        for d in unit.diagnostics:
            print(f"    {d}")

    unit = ctx.get_from_provider("pkg", BODY)
    if unit.root:
        print(f"  Success: pkg (body) -> {unit.root.kind_name}")
    else:
        print(f"  Error: pkg (body) failed to load")

    unit = ctx.get_from_provider("foo", SPEC)
    if unit.root:
        print(f"  Success: foo (spec) -> {unit.root.kind_name}")
    else:
        print(f"  Error: foo (spec) failed to load")

    print("")


def test_callback_returns_none():
    """Test callback returning None for unit not found."""
    print("== Test: Callback returns None (unit not found) ==")
    print("")

    def resolver(name, kind):
        # Only resolve "pkg"
        if name == "pkg":
            return "pkg.ads" if kind == "spec" else "pkg.adb"
        print(f"  Callback: {name} ({kind}) -> None (not found)")
        return None

    provider = lal.UnitProvider.from_callback(resolver)
    ctx = lal.AnalysisContext(unit_provider=provider)

    # Try to get a unit that doesn't exist
    unit = ctx.get_from_provider("nonexistent", SPEC)
    # When callback returns None, libadalang creates an empty unit
    print(f"  Unit has root: {unit.root is not None}")
    print(f"  Unit root kind: {unit.root.kind_name if unit.root else 'None'}")

    # The unit should exist but be empty
    print(f"  Unit exists: {unit is not None}")
    print(f"  Unit has no diagnostics: {len(unit.diagnostics) == 0}")

    print("")


def test_callback_exception():
    """Test callback raising an exception (should return None)."""
    print("== Test: Callback raises exception ==")
    print("")

    call_count = [0]

    def resolver(name, kind):
        call_count[0] += 1
        if name == "error":
            print(f"  Callback: {name} ({kind}) -> raising exception")
            raise ValueError("Simulated error in callback")
        return "pkg.ads"

    provider = lal.UnitProvider.from_callback(resolver)
    ctx = lal.AnalysisContext(unit_provider=provider)

    # This should not crash, callback exception should be caught
    unit = ctx.get_from_provider("error", SPEC)
    print(f"  Callback was called: {call_count[0] > 0}")
    print(f"  Unit exists after exception: {unit is not None}")

    # Try a successful one to ensure provider still works
    unit = ctx.get_from_provider("pkg", SPEC)
    print(f"  Subsequent call succeeded: {unit.root is not None}")

    print("")


def test_charset_parameter():
    """Test that charset parameter is passed through correctly."""
    print("== Test: Charset parameter ==")
    print("")

    def resolver(name, kind):
        return "pkg.ads"

    # Test with explicit charset
    provider = lal.UnitProvider.from_callback(resolver, charset="utf-8")
    ctx = lal.AnalysisContext(unit_provider=provider)

    unit = ctx.get_from_provider("pkg", SPEC)
    if unit.root:
        print(f"  Success with utf-8 charset: {unit.root.kind_name}")

    # Test with default charset (iso-8859-1)
    provider2 = lal.UnitProvider.from_callback(resolver)
    ctx2 = lal.AnalysisContext(unit_provider=provider2)

    unit2 = ctx2.get_from_provider("pkg", SPEC)
    if unit2.root:
        print(f"  Success with default charset: {unit2.root.kind_name}")

    print("")


def test_multiple_calls():
    """Test that callback can be called multiple times correctly."""
    print("== Test: Multiple callback invocations ==")
    print("")

    call_log = []

    def resolver(name, kind):
        call_log.append((name, kind))
        if name == "pkg":
            return "pkg.ads" if kind == "spec" else "pkg.adb"
        elif name == "foo":
            return "foo.ads" if kind == "spec" else None
        return None

    provider = lal.UnitProvider.from_callback(resolver)
    ctx = lal.AnalysisContext(unit_provider=provider)

    # Make several calls
    ctx.get_from_provider("pkg", SPEC)
    ctx.get_from_provider("pkg", BODY)
    ctx.get_from_provider("foo", SPEC)
    ctx.get_from_provider("foo", BODY)
    ctx.get_from_provider("unknown", SPEC)

    print(f"  Total callback invocations: {len(call_log)}")
    for name, kind in call_log:
        print(f"    - {name} ({kind})")

    print("")


def test_callback_memory():
    """Test callback with many invocations (memory stress test)."""
    print("== Test: Memory stress (many invocations) ==")
    print("")

    invocation_count = [0]

    def resolver(name, kind):
        invocation_count[0] += 1
        # Only resolve pkg
        if name == "pkg":
            return "pkg.ads" if kind == "spec" else "pkg.adb"
        return None

    provider = lal.UnitProvider.from_callback(resolver)
    ctx = lal.AnalysisContext(unit_provider=provider)

    # Call many times with different unit names
    for i in range(100):
        ctx.get_from_provider(f"unit{i}", SPEC)
        if i % 10 == 0:
            # Also successfully resolve pkg occasionally
            ctx.get_from_provider("pkg", SPEC)

    print(f"  Total invocations: {invocation_count[0]}")
    print(f"  No crashes or memory issues observed")

    print("")


def test_unicode_unit_name():
    """Test callback with non-ASCII characters in unit name."""
    print("== Test: Unicode unit name ==")
    print("")

    received_names = []

    def resolver(name, kind):
        received_names.append(name)
        print(f"  Callback received: {repr(name)}")
        return None

    provider = lal.UnitProvider.from_callback(resolver)
    ctx = lal.AnalysisContext(unit_provider=provider)

    # This likely won't happen in real Ada code, but tests the UTF-8 path
    ctx.get_from_provider("tëst_üñít", SPEC)

    # Verify the UTF-8 encoding worked correctly
    if received_names and received_names[0] == "tëst_üñít":
        print(f"  UTF-8 handling verified correctly")
    else:
        print(f"  Warning: UTF-8 may not have round-tripped correctly")

    print("")


if __name__ == "__main__":
    test_basic_callback()
    test_callback_returns_none()
    test_callback_exception()
    test_charset_parameter()
    test_multiple_calls()
    test_callback_memory()
    test_unicode_unit_name()

    print("All tests completed successfully.")
