import euwren

var
  wren = newWren()
  callback: WrenRef

proc setCallback(cb: WrenRef) =
  callback = cb

wren.foreign("game"):
  Game:
    setCallback
  module """
    foreign class Game {
      foreign static setCallback(callback)
    }
  """
wren.ready()

wren.run("""
  import "game" for Game

  Game.setCallback {
    System.print("hello from callback!")
  }
""")

let fnCall = wren["call()"]
wren.call(fnCall, callback)

