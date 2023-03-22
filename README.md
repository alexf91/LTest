# Fixture-based test framework for Lean 4

`LTest` uses macros to define testcases and fixtures for testing IO functions in Lean 4.
It is heavily inspired by [`pytest`](https://docs.pytest.org/) and [`rstest`](https://docs.rs/rstest).


## Fixtures

Fixtures are structures with a default state, a setup function and a teardown function.
They can depend on other fixtures. The value of dependencies is available in the setup function.

If `AnotherFixture` has type `Fixture Foo Bar`, then `n` with type `Bar` is available in the
setup function.
Both `setup` and `teardown` are executed in the `StateM Foo` monad to keep track of their state.

```Lean
fixture NatFixture Nat Nat requires (n : AnotherFixture) where
  default := 0
  setup := do return 0
  teardown := do return
```

**NOTE: Dependent fixtures are currently not supported.**


## Testcases

Testcases are essentially functions with a return value `IO Unit`, but they can depend
on fixtures. The value of a setup function is available in a testcases and their
teardown-function is executed regardless of the test result.

```Lean
testcase testSomething requires (a : FixtureA) (b : FixtureB) := do
  -- Do someting with a and b
  return
```
