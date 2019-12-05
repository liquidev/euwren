import macros

import euwren

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

var wren = newWren()

proc printFruit(f: Fruit) = echo f
proc printOpt(o: MenuOpt) = echo o
proc printLang(l: ProgLanguage) = echo l

expandMacros:
  wren.foreign("enums"):
    Fruit
    MenuOpt - opt
    ProgLanguage - lang -> Lang
    Enums:
      printFruit
      printOpt
      printLang
  wren.ready()

wren.run("""
  import "enums" for Fruit, MenuOpt, Lang, Enums

  System.print(Fruit.fruitApple)
  System.print(MenuOpt.Help)
  System.print(Lang.C)

  Enums.printFruit(Fruit.fruitBanana)
  Enums.printOpt(MenuOpt.Exit)
  Enums.printLang(Lang.Wren)
""")

