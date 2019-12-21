import segfaults
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
  test "onLoadModule":
    wren.onLoadModule do (path: string) -> string:
      if path == "hello":
        result = """
        class Hello {
          static say() {
            System.print("Hello!")
          }
        }
        """
    wren.run("""
      import "hello" for Hello
      Hello.say()
    """)
    check vmOut == "Hello!\n"
  test "onResolveModule":
    wren.onResolveModule do (importer, name: string) -> string:
      if importer == "main":
        result = "test" & name
      else:
        result = name
    wren.module("testthing", """
      System.print("running from testthing")
    """)
    wren.run("""
      import "thing"
    """)
    check vmOut == "running from testthing\n"

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
      [Test]:
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
      [Greeting]:
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
      [Adder]:
        add(int, int)
        add(int, int, _)
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
        *newA -> new
      B:
        *newB -> new
      C:
        *newC -> new
      [Test]:
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
      [Test]:
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
    var a = 2
    proc literalParams(x = 2): int = x + 1
    proc identParams(x = a): int = x + 2
    wren.foreign("test"):
      [Test]:
        literalParams
        identParams
    wren.ready()
    wren.run("""
      import "test" for Test
      System.print(Test.literalParams(2))
      System.print(Test.identParams(3))
    """)
  test "array params":
    test "array params - basic":
      proc testArray0(x: array[4, int], doCheck: bool) =
        if doCheck:
          check x == [1, 2, 3, 4]
      proc testArray1(x: array[1..4, int]) =
        for i in 1..4:
          check x[i] == i
      wren.foreign("test1"):
        [Array]:
          testArray0 -> test0
          testArray1 -> test1
      wren.ready()
      wren.run("""
        import "test1" for Array
        Array.test0([1, 2, 3, 4], true)
        Array.test1([1, 2, 3, 4])
      """)
      expect WrenError:
        wren.run("""
          Array.test0([1, 2], false)
        """)
      expect WrenError:
        wren.run("""
          Array.test0([1, "hello", 3, 4], false)
        """)
    test "array params - matrix":
      proc testMatrix(x: array[3, array[3, float]]) =
        check x == [
          [0.0, 1.0, 2.0],
          [1.0, 2.0, 3.0],
          [2.0, 3.0, 4.0]
        ]
      wren.foreign("test2"):
        [Matrix]:
          testMatrix -> test
      wren.ready()
      wren.run("""
        import "test2" for Matrix
        Matrix.test([
          [0, 1, 2],
          [1, 2, 3],
          [2, 3, 4]
        ])
      """)
    test "array params - objects":
      type
        Test = object
          x: int
      proc newTest(x: int): Test = Test(x: x)
      proc testObjects(x: array[4, Test]) =
        check x == [
          Test(x: 1), Test(x: 2), Test(x: 3), Test(x: 4)
        ]
      wren.foreign("test3"):
        Test:
          *newTest -> new
        [Objects]:
          testObjects -> test
      wren.ready()
      wren.run("""
        import "test3" for Test, Objects
        Objects.test([
          Test.new(1),
          Test.new(2),
          Test.new(3),
          Test.new(4)
        ])
      """)
  test "array return type":
    test "array return type - simple":
      proc getArray(): array[4, int] =
        result = [1, 2, 3, 4]
      wren.foreign("test1"):
        [Array]:
          getArray -> get
      wren.ready()
      wren.run("""
        import "test1" for Array
        var x = Array.get()
        for (a in x) {
          System.print(a)
        }
      """)
      check vmOut == "1\n2\n3\n4\n"
    test "array return type - matrix":
      proc getMatrix(): array[3, array[3, float]] =
        result = [
          [1.0, 2.0, 3.0],
          [2.0, 3.0, 4.0],
          [3.0, 4.0, 5.0]
        ]
      wren.foreign("test2"):
        [Matrix]:
          getMatrix -> get
      wren.ready()
      wren.run("""
        import "test2" for Matrix
        var x = Matrix.get()
        var result = ""
        for (a in x) {
          for (b in a) {
            result = result + b.toString
          }
        }
        System.print(result)
      """)
      check vmOut == "123234345\n"
  test "seq params":
    test "seq params - basic":
      proc testSeq(x: seq[int], doCheck: bool) =
        if doCheck:
          check x == @[1, 2, 3]
      wren.foreign("test1"):
        [Seq]:
          testSeq -> test
      wren.ready()
      wren.run("""
        import "test1" for Seq
        Seq.test([1, 2, 3], true)
      """)
      expect WrenError:
        wren.run("""
          Seq.test([1, "hello", 3, true], false)
        """)
    test "seq params - nested":
      proc testNestedSeq(x: seq[seq[int]]) =
        check x == @[
          @[1, 2, 3, 4, 5],
          @[2, 3],
          @[5, 4, 3]
        ]
      wren.foreign("test2"):
        [NestedSeq]:
          testNestedSeq -> test
      wren.ready()
      wren.run("""
        import "test2" for NestedSeq
        NestedSeq.test([
          [1, 2, 3, 4, 5],
          [2, 3],
          [5, 4, 3]
        ])
      """)
    test "inline procs":
      wren.foreign("test"):
        [Inline]:
          normal do (x: int) -> float:
            result = float(x) * 1.5
          ?getter do -> float: 3.14159
      wren.ready()
      wren.run("""
        import "test" for Inline
        System.print(Inline.normal(2))
        System.print(Inline.getter)
      """)
      check vmOut == "3\n3.14159\n"

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
        *newTestObject -> new
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
        *newObjectWithVerboseName -> new
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
        *newGreeter -> new
        ?greeting
    wren.ready()
    wren.run("""
      import "test" for Greeter
      var x = Greeter.new("world")
      System.print(x.greeting)
    """)
    check vmOut == "Hello, world!\n"
  test "`var` receiver":
    type
      Counter = object
        count: int
    proc newCounter(): Counter = Counter(count: 0)
    proc inc(counter: var Counter) = inc(counter.count)
    proc count(counter: Counter): int = counter.count
    wren.foreign("count"):
      Counter:
        *newCounter -> new
        inc(var Counter)
        ?count
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
        *newRefObj -> new
        something
    wren.ready()
    wren.run("""
      import "test" for RefObj
      var x = RefObj.new(2)
    """)
  test "tuples":
    type
      Rect = tuple[x, y, width, height: float]
    proc checkRect(r: Rect) =
      check r == (x: 1.0, y: 2.0, width: 3.0, height: 4.0)
    wren.foreign("test"):
      Rect: discard
      [Test]:
        checkRect
    wren.ready()
    wren.run("""
      import "test" for Rect, Test
      var r = Rect.new(1, 2, 3, 4)
      Test.checkRect(r)
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

