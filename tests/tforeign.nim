import macros
import strutils

import euwren

proc add(a, b: int): int = a + b
proc pi: float = 3.14159265

type
  Greeter = object
    target: string

proc init(greeter: var Greeter, target: string) =
  greeter.target = target

proc initGreeter(target: string): Greeter =
  echo target.escape
  result = Greeter()
  result.init(target)

proc getGreeting(greeter: Greeter): string =
  result = "Hello, " & greeter.target

var wren = newWren()
wren.foreign("math"):
  Math:
    add(int, int)
    [get] pi

expandMacros:
  wren.foreign("greet"):
    Greeter:
      [new] initGreeter
      [get] getGreeting -> "greeting"
      """
        greet() {
          System.print(greeting)
        }
      """

wren.run("""
import "greet" for Greeter

var greeter = Greeter.new("world")
System.print(greeter.greeting)
greeter.greet()
""")
