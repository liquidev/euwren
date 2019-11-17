import euwren

var wren = newWren()

wren.run("""
var x = 42
""")

let wrenX = wren["main", "x", float]
assert wrenX == 42
echo wrenX
