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
- Supports proc, object, and enum (NYI) binding
- Does type checks for procedures
- Supports operator overloading

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
```
```d
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
    # procs can be aliased on the Wren side (NYI)
    subtract -> sub
  # we need to provide the module's actual source code
  module """
    class Math {
      foreign static add(a, b)
      foreign static sub(a, b)
    }
  """
```
```d
import "math" for Math
System.print(Math.add(2, 2)) // Output: 4
```

### Binding objects

Binding objects is very similar to procs. All *public* object fields are
exported to Wren.

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
  # objects can be aliased, just like procs (NYI)
  Foo -> Bar:
    # an object must have exactly one constructor or initializer, and it must
    # be the first thing that's bound
    # adding one changes the namespace to a foreign object
    # a constructor creates an object from scratch, an initializer initializes
    # an object in place
    [new] initFoo
    more
    [get] count
  module """
    foreign class Bar {
      construct new(name) {}

      foreign more()
      foreign count
      foreign name // bound implicitly (NYI)
    }
  """
```
```d
import "foo" for Bar

var foo = Bar.new()
foo.more()
System.print(foo.count)
```

### Binding enums (NYI)

Binding enums is very easy, since all glue code is generated for you.
In fact, an enum is nothing more than a class with a bunch of static getters.

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
  # if a foreign() block contains an enum, the module is prepended with the
  # given enums. this also means we don't need to provide a module() block here
```
```d
class Fruit {
  static fruitApple { 0 }
  static fruitBanana { 1 }
  static fruitGrape { 2 }
}
class MenuOpt {
  static Start { 0 }
  static Help { 1 }
  static Exit { 2 }
}
class Lang {
  static Nim { 0 }
  static Wren { 1 }
  static C { 2 }
}
```
```d
import "enums" for Fruit, MenuOpt, Lang

System.print(Fruit.fruitGrape) // 2
System.print(MenuOpt.Start) // 0
System.print(Lang.Wren) // 1
```

### Gotchas

- A couple of extra macros called `addProcAux` and `addClassAux` is exposed in
  the public API. **Do not use them in your code.** They are used internally by
  `foreign()`, and they make the DSL possible by deferring all binding to the
  semantic pass. There are lots of implementation details here, feel free to
  read the source code if you're interested.
