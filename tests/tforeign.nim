import macros
import strutils

import euwren

proc add(a, b: int): int = a + b
proc pi: float = 3.14159265

type
  Greeter = object
    target: string
  Vec2f = object
    x*, y*: float

proc init(greeter: var Greeter, target: string) =
  greeter.target = target

proc initGreeter(target: string): Greeter =
  echo target.escape
  result = Greeter()
  result.init(target)

proc vec2f(x, y: float): Vec2f = Vec2f(x: x, y: y)
proc `$`(vec: Vec2f): string = "[" & $vec.x & ", " & $vec.y & "]"
proc `+`(a, b: Vec2f): Vec2f = Vec2f(x: a.x + b.x, y: a.y + b.y)

proc getGreeting(greeter: Greeter): string =
  result = "Hello, " & greeter.target

var wren = newWren()

expandMacros:
  wren.foreign("math"):
    Math:
      add(int, int)
      [get] pi
    Vec2f:
      {.dataClass.}
      [get] `$` -> toString
      `+`(Vec2f, Vec2f)
    Vec:
      vec2f -> new
  wren.foreign("greet"):
    Greeter:
      [new] initGreeter
      [get] getGreeting -> "greeting"
      """
        greet() {
          System.print(greeting)
        }
      """
  wren.ready()

wren.module("testMath", """
import "math" for Math, Vec

var a = Vec.new(10, 20)
var b = Vec.new(30, 40)

System.print(a)
""")

wren.module("testGreet", """
import "greet" for Greeter

var greeter = Greeter.new("world")
System.print(greeter.greeting)
greeter.greet()
""")
