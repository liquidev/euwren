import os

import nimterop/build
import nimterop/cimport

const
  Base = getProjectCacheDir("euwren")
  Src = Base/"src"
  Vm = Src/"vm"
  Optional = Src/"optional"
  Include = Src/"include"

static:
  gitPull("https://github.com/liquid600pgm/wren.git", Base, """
src/vm/*
src/optional/*
src/include/*""", "fix-710")

cIncludeDir(Include)
cIncludeDir(Vm)
cIncludeDir(Optional)

cCompile(Vm/"wren_compiler.c")
cCompile(Vm/"wren_core.c")
cCompile(Vm/"wren_debug.c")
cCompile(Vm/"wren_primitive.c")
cCompile(Vm/"wren_utils.c")
cCompile(Vm/"wren_value.c")
cCompile(Vm/"wren_vm.c")
cCompile(Optional/"wren_opt_meta.c")
cCompile(Optional/"wren_opt_random.c")

cOverride:
  type
    WrenLoadModuleFn* = proc (vm: ptr WrenVM, name: cstring): cstring {.cdecl.}
    WrenReallocateFn* = proc (memory: pointer, size: csize): pointer {.cdecl.}
    WrenResolveModuleFn* =
      proc (vm: ptr WrenVM, importer, name: cstring): cstring {.cdecl.}

cImport(Include/"wren.h")
