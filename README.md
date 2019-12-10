# euwren

> The [Eurasian wren](https://en.wikipedia.org/wiki/Wren) has been long
> considered "the king of birds" in Europe.

euwren (pronounced _oyren_, like _euro_ in German) is a high-level
[Wren](https://github.com/wren-lang/wren) wrapper for Nim. Wren is a small,
fast, embedded scripting language.

The main point of euwren is to create a very user-friendly, high-level wrapper:
"The king of Wren wrappers". It leverages Nim's powerful macro system to make
the API as simple as listing all the things you need in Wren. While it may not
be the fastest of wrappers, it's not the primary goal. It's the end user
experience that really counts.

euwren is still WIP, so not all features are implemented. Those that are
unavailable are marked with `(NYI)`.

## Features

- Syntactically simple
- Supports proc, object, and enum binding
- Does type checks for procedures
- Supports operator overloading
- Automatically generates Wren glue code with declarations (experimental)

## Installing

### Adding to your .nimble file
```nim
requires "euwren"
```

### Installing directly from the command line
```bash
$ nimble install euwren
```

## Usage

Because Nim and Wren have different programming paradigms, some work must be
done by the programmer. Fortunately, what needs to be done is very simple, so
don't worry.

### Running code

First, a VM instance must be created.
```nim
import euwren

var wren = newWren()
```
After that, running code is as simple as:
```nim
# run() runs code in the 'main' module
# it's an alias to wren.module("main", <code>)
wren.run("""
  System.print("Hello from Wren!")
""")
```

### Retrieving variables

To get a primitive variable from Wren, use the subscript operator with three
arguments.
```nim
wren.run("""
  var myInt = 2
""")

               # module  name     type
let myInt = wren["main", "myInt", int]
assert myInt == 2
```

Any number type conversions between Nim and Wren are performed automatically.

To retrieve a Wren object, eg. a class, use the subscript operator with two
arguments.
```nim
wren.run("""
  class Program {
    static run() {
      System.print("Hello from inside the class!")
    }
  }
""")

let classProgram = wren["main", "Program"]
```

### Calling methods

Calling methods on Wren objects is done by first obtaining a call handle, and
then calling the method. Note that the Wren VM **is not reentrant**, meaning
you cannot call Wren in a foreign method.

To obtain a call handle, use the curly brace operator. Then, to call the
method, use `call()`.
```nim
# the convention for naming the variable:
# method<name><number of arguments>
# this convention is the preferred naming conventions for variables and fields
# that store Wren call handles, but you're free to use any convention you want
let methodRun0 = wren{"run()"}
# the second parameter is the call handle to the method, the third is the
# receiver of the method, and the rest is the parameters to pass to
# the method.
# when the method is static, the receiver is the class of the method
wren.call(methodRun0, classProgram)
```

### Binding procs

Wren is strictly class-based, but Nim is not—that means that any procs passed to
Wren must be nested inside a class. Fortunately, that's pretty simple.

```nim
proc hello() =
  echo "Hello from Nim!"

wren.foreign("nim"):
  # create a namespace 'Nim' that will hold our proc
  Nim:
    # bind the proc 'hello'
    hello
# ready() must be called to ready the VM for code execution after any
# foreign() calls. this arms the VM to do code execution with foreign type
# checking. no calls to foreign() should be done after you call this!
wren.ready()
```
```js
import "nim" for Nim
Nim.hello() // Output: Hello from Nim!
```
Here's a more advanced example:
```nim
proc add(a, b: int): int = a + b
proc add(a, b, c: int): int = a.add(b).add(c)
proc subtract(a, b: int): int = a - b

# foreign() accepts the name of the module we want to bind
wren.foreign("math"):
  # we create a namespace 'Math' for our procs
  Math:
    # procs can be overloaded by arity, but not by parameter type
    # (this is not enforced, so be careful!)
    add(int, int)
    add(int, int, int)
    # procs can be aliased on the Wren side
    subtract -> sub
wren.ready()
```
```js
import "math" for Math
System.print(Math.add(2, 2)) // Output: 4
```

Nim procedures can accept `WrenRef` as arguments. This allows Wren objects to
be passed to Nim:
```nim
# this example also demonstrates a way of passing callbacks from Wren to Nim,
# but this works for any Wren type (eg. classes)
var onTickFn: WrenRef

proc onTick(callback: WrenRef) =
  onTickFn = callback

wren.foreign("engine"):
  Engine:
    onTick
wren.ready()

wren.run(code)

let methodCall0 = wren{"call()"}
wren.call(methodCall0, onTickFn)
```
```js
import "engine" for Engine
Engine.onTick {
  System.println("Hello from callback!")
}
```

### Binding objects

Binding objects is very similar to procs. All *public* object fields are
exported to Wren (NYI).

If a proc returns an object, the class for that object must be declared *before*
the proc is declared.

```nim
type
  Foo = object
    name*: string
    count: int

proc initFoo(name: string): Foo =
  result = Foo()
  result.name = name
  result.count = 1

proc more(foo: var Foo) =
  inc(foo.count)

proc count(foo: Foo) = foo.count

wren.foreign("foo"):
  # objects can be aliased, just like procs
  Foo -> Bar:
    # an object must have exactly one constructor or initializer, and it must
    # be the first thing that's bound
    # adding one changes the namespace to a foreign object
    # a constructor creates an object from scratch, an initializer initializes
    # an object in place
    [new] initFoo
    more
    [get] count
wren.ready()
```
```js
import "foo" for Bar

var foo = Bar.new("Thing")
foo.more()
System.print(foo.count)
```

If an object stores some foreign data, but does not have a constructor, the
`{.dataClass.}` pragma must be used.

```nim
type
  Vec2 = object
    x*, y*: float

proc `$`(v: Vec2): string = "(" & $v.x & ", " & $v.y & ")"
proc `+`(a, b: Vec2): Vec2 = Vec2(x: a.x + b.x, y: a.y + b.y)

proc vec2(x, y: float): Vec2 = Vec2(x: x, y: y)

wren.foreign("vec"):
  Vec2:
    {.dataClass.}
    `$`(Vec2) -> toString # methods can still be declared normally
    `+`(Vec2, Vec2)
  Vec:
    vec2 -> new
```
```js
import "vec" for Vec

var a = Vec.new(10, 20)
var b = Vec.new(20, 30)
var c = a + b

System.print(c)
```

### Binding enums

```nim
type
  Fruit = enum
    fruitApple
    fruitBanana
    fruitGrape
  MenuOpt = enum
    optStart
    optHelp
    optExit
  ProgLanguage = enum
    langNim
    langWren
    langC

wren.foreign("enums"):
  # enums are bound by not specifying a body
  Fruit
  # the conventional prefix can be stripped by using ``-``
  MenuOpt - opt
  # enums can also be aliased
  ProgLanguage - lang -> Lang
wren.ready()
```
The generated class includes all the values of an enum, and additionally,
`low` and `high` for utility purposes. This also means that you should refrain
from naming your enums in `snake_case`, as your names may clash with the
built-in `low` and `high` properties. An option may be added in the future to
automatically convert your enum to `PascalCase`.

Here's an example of a generated module, based on the above input:
```js
class Fruit {
  static fruitApple { 0 }
  static fruitBanana { 1 }
  static fruitGrape { 2 }
  static low { 0 }
  static high { 2 }
}
class MenuOpt {
  static Start { 0 }
  static Help { 1 }
  static Exit { 2 }
  static low { 0 }
  static high { 2 }
}
class Lang {
  static Nim { 0 }
  static Wren { 1 }
  static C { 2 }
  static low { 0 }
  static high { 2 }
}
```
```js
import "enums" for Fruit, MenuOpt, Lang

System.print(Fruit.fruitGrape) // 2
System.print(MenuOpt.Start) // 0
System.print(Lang.Wren) // 1
```

### Compile-time flags
- `-d:euwrenDumpForeignModule` – dumps the Wren module source code generated by
  `foreign()` upon runtime. Useful for debugging code generation.

### Gotchas

- Three extra macros called `addProcAux`, `addClassAux`, and `genEnumAux` are
  exposed in the public API. **Do not use them in your code.** They are used
  internally by `foreign()`, and they make the DSL possible by deferring all
  binding to the semantic phase. There are lots of implementation details here,
  feel free to read the source code if you're interested.
- Currently, euwren uses a fork of Wren that fixes an issue related to slots
  in the VM. This fork is not the same as the current stable version of Wren,
  but it will be used until [Wren/#712](https://github.com/wren-lang/wren/pull/712)
  is merged.
- The generated glue code is assembled at run time, which is inefficient and
  possibly slows binding down. This will be fixed in a future release.

