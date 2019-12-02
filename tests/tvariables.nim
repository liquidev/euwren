import euwren
import euwren/private/wren as wren_c

var
  wren = newWren()

wren.run("""
var x = 42

class Program {
  static run() {
    System.print("hello")
  }
}
""")

let
  programClass = wren["main", "Program"] 
  runCallHandle = wren{"run()"} 

wren.call(runCallHandle, programClass)

