import euwren
import euwren/private/wren as wren_c

# var
#   wren = newWren()
#   vm = wren.raw
var config: WrenConfiguration
wrenInitConfiguration(addr config)
config.writeFn = proc (vm: ptr WrenVM, text: cstring) {.cdecl.} =
  stdout.write(text)
var vm = wrenNewVM(addr config)
wrenEnsureSlots(vm, 24)

echo wrenInterpret(vm, "main", """
var x = 42

class Program {
  static run() {
    System.print("hello")
  }
}
""")

wrenEnsureSlots(vm, 1)

wrenGetVariable(vm, "main", "Program", 0)
let
  programClass = wrenGetSlotHandle(vm, 0)
  runCallHandle = wrenMakeCallHandle(vm, "run()")

wrenEnsureSlots(vm, 1)
wrenSetSlotHandle(vm, 0, programClass)
let callResult = wrenCall(vm, runCallHandle)

echo callResult

