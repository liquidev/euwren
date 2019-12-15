import unittest

import euwren

suite "base API":
  setup:
    var
      wren = newWren()
      vmOut = ""
    wren.onWrite do (str: string):
      vmOut.add(str)

  #--
  # basics
  #--

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

suite "foreign()":
  setup:
    var
      wren = newWren()
      vmOut = ""
    wren.onWrite do (str: string):
      vmOut.add(str)

  #--
  # procedures
  #--

  test "basic procedures":
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
  test "procedure aliasing":
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
  test "procedure overloading":
    proc add(a, b: int): int = a + b
    proc add(a, b, c: int): int = a + b + c
    wren.foreign("test"):
      Adder:
        add(int, int)
        add(int, int, int)
    wren.ready()
    wren.run("""
      import "test" for Adder
      System.print(Adder.add(3, 7))
      System.print(Adder.add(1, 2, 3))
    """)
    check vmOut == "10\n6\n"
  test "type checking":
    type
      A = ref object of RootObj
        a*: string
      B = ref object of A
        b*: int
      C = ref object
    proc newA(): A = A()
    proc newB(): B = B()
    proc newC(): C = C()
    proc printA(a: A) = vmOut.add("got A\n")
    proc printB(b: B) = vmOut.add("got B\n")
    proc printC(c: C) = vmOut.add("got C\n")
    wren.foreign("test"):
      A:
        [new] newA
      B:
        [new] newB
      C:
        [new] newC
      Test:
        printA
        printB
        printC
    wren.ready()
    wren.run("""
      import "test" for A, B, C, Test
      var a = A.new()
      var b = B.new()
      var c = C.new()
      a.a = "hello"
      b.b = 42
      b.a = "world"
      Test.printA(a)
      Test.printB(b)
      Test.printA(b)
      Test.printC(c)
    """)
    check vmOut == "got A\ngot B\ngot A\ngot C\n"
    expect WrenError:
      wren.run("""
        var a = A.new()
        Test.printC(a)
      """)
  test "procedure exceptions":
    var success = false
    proc exceptionTest() =
      raise newException(Exception, "test error")
    proc ok() = success = true
    wren.foreign("test"):
      Test:
        exceptionTest
        ok
    wren.ready()
    expect WrenError:
      wren.run("""
        import "test" for Test
        Test.exceptionTest()
      """)
    wren.run("""
      var error = Fiber.new { Test.exceptionTest() }.try()
      if (error.startsWith("test error [Exception]")) {
        Test.ok()
      } else {
        System.print("fail: wanted `test error [Exception]`")
        System.print("got: " + error)
      }
    """)
    if not success: echo vmOut
    check success
  test "default param handling":
    proc defaultParams(x = 2): int = x + 1
    wren.foreign("test"):
      Test:
        defaultParams
    wren.ready()
    wren.run("""
      import "test" for Test
      System.print(Test.defaultParams(2))
    """)
  test "array params":
    proc testArray0(x: array[4, int], doCheck: bool) =
      if doCheck:
        check x == [1, 2, 3, 4]
    proc testArray1(x: array[1..4, int]) =
      for i in 1..4:
        check x[i] == i
    wren.foreign("test"):
      Array:
        testArray0 -> test0
        testArray1 -> test1
    wren.ready()
    wren.run("""
      import "test" for Array
      Array.test0([1, 2, 3, 4], true)
      Array.test1([1, 2, 3, 4])
    """)
    expect WrenError:
      wren.run("""
        Array.test0([1, 2], false)
      """)
  test "seq params":
    proc testSeq(x: seq[int]) =
      check x == @[1, 2, 3]
    wren.foreign("test"):
      Seq:
        testSeq -> test
    wren.ready()
    wren.run("""
      import "test" for Seq
      Seq.test([1, 2, 3])
    """)

  #--
  # objects
  #--

  test "objects":
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
  test "object aliasing":
    type
      ObjectWithVerboseName = object
    proc newObjectWithVerboseName(): ObjectWithVerboseName =
      result = ObjectWithVerboseName()
    wren.foreign("test"):
      ObjectWithVerboseName -> Verbose:
        [new] newObjectWithVerboseName
    wren.ready()
    wren.run("""
      import "test" for Verbose
      var x = Verbose.new()
    """)
  test "object getters":
    type
      Greeter = object
        target: string
    proc newGreeter(target: string): Greeter = Greeter(target: target)
    proc greeting(greeter: Greeter): string = "Hello, " & greeter.target & "!"
    wren.foreign("test"):
      Greeter:
        [new] newGreeter
        [get] greeting
    wren.ready()
    wren.run("""
      import "test" for Greeter
      var x = Greeter.new("world")
      System.print(x.greeting)
    """)
    check vmOut == "Hello, world!\n"
  test "{.dataClass.}":
    type
      Vec2 = object
        x*, y*: float
    proc vec2(x, y: float): Vec2 = Vec2(x: x, y: y)
    proc `$`(a: Vec2): string = "[" & $a.x & ", " & $a.y & "]"
    wren.foreign("test"):
      Vec2:
        {.dataClass.}
        [get] `$` -> toString
      Vec:
        vec2 -> new
    wren.ready()
    wren.run("""
      import "test" for Vec
      var a = Vec.new(10, 20)
      a.x = 30
      System.print(a)
    """)
    check vmOut == "[30.0, 20.0]\n"
  test "`var` receiver":
    type
      Counter = object
        count: int
    proc newCounter(): Counter = Counter(count: 0)
    proc inc(counter: var Counter) = inc(counter.count)
    proc count(counter: Counter): int = counter.count
    wren.foreign("count"):
      Counter:
        [new] newCounter
        inc(var Counter)
        [get] count
    wren.ready()
    wren.run("""
      import "count" for Counter
      var x = Counter.new()
      System.print(x.count)
      x.inc()
      System.print(x.count)
    """)
    check vmOut == "0\n1\n"
  test "ref objects":
    type
      RefObj = ref object
        x: int
    proc newRefObj(x: int): RefObj = RefObj(x: x)
    proc something(): RefObj = RefObj(x: 2)
    wren.foreign("test"):
      RefObj:
        [new] newRefObj
        something
    wren.ready()
    wren.run("""
      import "test" for RefObj
      var x = RefObj.new(2)
    """)

  #--
  # enums
  #--

  test "basic enums":
    type
      TestEnum = enum
        A, B, C
    wren.foreign("test"):
      TestEnum
    wren.ready()
    wren.run("""
      import "test" for TestEnum
      System.print(TestEnum.A)
      System.print(TestEnum.B)
      System.print(TestEnum.C)
    """)
    check vmOut == "0\n1\n2\n"
  test "enum aliasing":
    type
      TestEnum = enum
        A, B, C
    wren.foreign("test"):
      TestEnum -> Test
    wren.ready()
    wren.run("""
      import "test" for Test
      System.print(Test.A)
      System.print(Test.B)
      System.print(Test.C)
    """)
    check vmOut == "0\n1\n2\n"
  test "enum prefix stripping":
    type
      TestEnum = enum
        testA, testB, testC
    wren.foreign("test"):
      TestEnum - test
    wren.ready()
    wren.run("""
      import "test" for TestEnum
      System.print(TestEnum.A)
      System.print(TestEnum.B)
      System.print(TestEnum.C)
    """)
    check vmOut == "0\n1\n2\n"
  test "enum prefix stripping with aliasing":
    type
      TestEnum = enum
        testA, testB, testC
    wren.foreign("test"):
      TestEnum - test -> Test
    wren.ready()
    wren.run("""
      import "test" for Test
      System.print(Test.A)
      System.print(Test.B)
      System.print(Test.C)
    """)
    check vmOut == "0\n1\n2\n"

