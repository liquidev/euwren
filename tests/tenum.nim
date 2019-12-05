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

expandMacros:
  wren.foreign("enums"):
    Fruit 
    MenuOpt - opt
    ProgLanguage - lang -> Lang
  wren.ready()

wren.run("""
  import "enums" for Fruit, MenuOpt, Lang

  System.print(Fruit.fruitApple)
  System.print(MenuOpt.Help)
  System.print(Lang.C)
""")

