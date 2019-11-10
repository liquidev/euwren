import macros

import euwren

proc add(a, b: int): int = a + b
proc pi: float = 3.14159265

type
  Greeter = object
    target: string

proc init(greeter: var Greeter, target: string) =
  greeter.target = target

proc initGreeter(target: string): Greeter =
  result = Greeter()
  result.init(target)

proc greeting(greeter: Greeter): string =
  result = "Hello, " & greeter.target

var wren = newWren()
wren.foreign("math"):
  Math:
    add(int, int)
    [get] pi
  module """
    class Math {
      foreign static add(x, y)
      foreign static pi
    }
  """

expandMacros:
  wren.foreign("greet"):
    Greeter:
      [new] initGreeter
      [get] greeting
    module """
      foreign class Greeter {
        construct new(target) {}
        foreign greeting
      }
    """

wren.run("""
import "greet" for Greeter

var greeter = Greeter.new("world")
System.print(greeter.greeting)
""")
