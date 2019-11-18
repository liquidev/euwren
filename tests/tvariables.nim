import euwren

var wren = newWren()

wren.module("main", """
var x = 42

class Program {
  static run() {
    System.print("hello")
  }
}
""")

let
  classProgram = wren["main", "Program"]
echo wren.raw.getSlotTypeString(22)
let
  runCall = wren["run()"]

# wren.call(runCall, classProgram)
