import unittest

import euwren

suite "euwren":
  setup:
    var
      wren = newWren()
      vmOut = ""
    wren.onWrite do (str: string):
      vmOut.add(str)

  test "running code":
    wren.run("""
      System.print("Hello, world!")
    """)
    check vmOut == "Hello, world!\n"
  test "primitive variables":
    wren.run("""
      var x = 1
      var str = "Hello"
    """)
    let
      x = wren["main", "x", int]
      str = wren["main", "str", string]
    check x == 1
    check str == "Hello"
  test "WrenRef variables and method calls":
    wren.run("""
      class Test {
        static run(param) {
          System.print("result = " + param.toString)
          if (param == 42) {
            return "Success"
          } else {
            return "Fail"
          }
        }
      }
    """)
    let
      testClass = wren["main", "Test"]
      methodRun1 = wren{"run(_)"}
      callResult = wren.call[:string](methodRun1, testClass, 42)
    check vmOut == "result = 42\n"
    check callResult == "Success"
  test "WrenError on fiber abort":
    expect WrenError:
      wren.run("""
        Fiber.abort("oops!")
      """)
    expect WrenError:
      wren.run("""
        System.print("Adding a num to a string " + 10)
      """)

  test "foreign() - basic procedures":
    var state = "didn't pass"
    proc goal() =
      state = "passed!"
    wren.foreign("test"):
      Test:
        goal
    wren.ready()
    wren.run("""
      import "test" for Test
      Test.goal()
    """)
    check state == "passed!"
  test "foreign() - procedure aliasing":
    proc getGreeting(target: string): string =
      result = "Hello, " & target & "!"
    wren.foreign("test"):
      Greeting:
        getGreeting -> "get"
        getGreeting -> `[]`
    wren.ready()
    wren.run("""
      import "test" for Greeting
      System.print(Greeting.get("world"))
      System.print(Greeting["Nim"])
    """)
    check vmOut == "Hello, world!\nHello, Nim!\n"

  test "foreign() - objects":
    type
      TestObject = object
        publicField*: string
        privateField: string
    proc newTestObject(): TestObject =
      result = TestObject(publicField: "access granted",
                          privateField: "access denied")
    wren.foreign("test"):
      TestObject:
        [new] newTestObject
    wren.ready()
    wren.run("""
      import "test" for TestObject
      var x = TestObject.new()
      System.print(x.publicField)
      x.publicField = "hacked"
      System.print(x.publicField)
    """)
    check vmOut == "access granted\nhacked\n"
    expect WrenError:
      wren.run("""
        import "test" for TestObject
        var x = TestObject.new()
        System.print(x.privateField)
      """)

