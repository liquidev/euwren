import euwren

type
  A = ref object of RootObj
    x: int
  B = ref object of A
    y: int
  C = ref object
    z: int

var wren = newWren()

proc newA(x: int): A = A(x: x)
proc newB(x, y: int): B = B(x: x, y: y)
proc newC(z: int): C = C(z: z)

proc printA(a: A) =
  echo "success!"
  echo a.x

proc printC(c: C) =
  echo "fail!"
  echo c.z

wren.foreign("inherit"):
  A:
    [new] newA
  B:
    [new] newB
  C:
    [new] newC
  Test:
    printA
    printC
  """
    foreign class A {
      construct new(x) {}
    }
    foreign class B {
      construct new(x, y) {}
    }
    foreign class C {
      construct new(z) {}
    }
    class Test {
      foreign static printA(a)
      foreign static printC(c)
    }
  """
wren.ready()

wren.run("""
import "inherit" for A, B, C, Test

System.print("testing")

var a = A.new(1)
var b = B.new(2, 3)

Test.printA(b)

var error = Fiber.new {
  var a = A.new(1)
  Test.printC(a)
}.try()
if (error) {
  System.print("success!")
  System.print("error message: " + error)
}
""")
