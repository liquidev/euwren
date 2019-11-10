import euwren

var vm = newWren()
vm.module("code", """
  class Hi {
    there() {
      System.print("Hi there")
    }
  }
""")
vm.run("""
  import "code" for Hi
  Hi.there()
""")
