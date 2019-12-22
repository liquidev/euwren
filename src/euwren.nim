import macros
import strutils
import tables

import euwren/private/wren

#--
# Definitions
#--

type
  RawVM* = ptr WrenVM

  MethodSign = tuple[module, class, name: string, isStatic: bool]
  ClassSign = tuple[module, name: string]

  WrenType* = enum
    wtBool = "bool"
    wtNumber = "number"
    wtForeign = "foreign"
    wtList = "list"
    wtNull = "null"
    wtString = "string"
    wtUnknown = "<wren type>"

  WrenRef* = ref object
    vm: Wren
    handle: ptr WrenHandle
  WrenValueKind = enum
    wvkBool
    wvkNumber
    wvkString
    wvkWrenRef
  WrenValue = object
    case kind: WrenValueKind
    of wvkBool: boolVal: bool
    of wvkNumber: numVal: float
    of wvkString: strVal: string
    of wvkWrenRef: wrenRef: WrenRef
  ModuleVar = tuple[module, variable: string]

  Wren* = ref object
    ## A Wren virtual machine used for executing code.
    handle: RawVM
    procWrite: proc (str: string)
    procResolveModule: proc (importer, name: string): string
    procLoadModule: proc (path: string): string

    methods: Table[MethodSign, WrenForeignMethodFn]
    classes: Table[ClassSign, WrenForeignClassMethods]

    typeNames: Table[uint16, string]
    parentTypeIds: Table[uint16, set[uint16]]

    compileErrors: seq[WrenError]
    rtError: WrenError
  WrenErrorKind* = enum
    weCompile ## A compilation error (eg. syntax error).
    weRuntime ## A runtime error (eg. ``Fiber.abort()``).
  WrenError* = object of CatchableError
    ## A Wren error. This is raised when an error occurs *inside the VM.*
    module*: string
    line*: int
    message*: string
    case kind*: WrenErrorKind
    of weCompile: discard
    of weRuntime:
      stackTrace*: seq[tuple[module: string, line: int, message: string]]

proc `$`*(vm: Wren): string =
  ## Return a string representation of the Wren instance. Keep in mind this
  ## doesn't really hold much useful information.
  result = "- Wren instance\n" &
           "VM: " & $cast[int](vm.handle) & '\n'

proc raw*(vm: Wren): RawVM =
  ## Returns the raw VM from the Wren instance.
  vm.handle

proc raw*(wr: WrenRef): ptr WrenHandle =
  ## Returns the raw WrenHandle.
  wr.handle

proc newWren*(): Wren =
  ## Creates a new VM.
  new(result) do (vm: Wren):
    wrenFreeVM(vm.handle)

  var config: WrenConfiguration
  wrenInitConfiguration(addr config)
  # debugging
  config.writeFn = proc (vm: RawVM, text: cstring) {.cdecl.} =
    cast[Wren](wrenGetUserData(vm)).procWrite($text)
  config.errorFn = proc (vm: RawVM, ty: WrenErrorType, module: cstring,
                         line: cint, msg: cstring) {.cdecl.} =
    var wvm = cast[Wren](wrenGetUserData(vm))
    case ty
    of WREN_ERROR_COMPILE:
      var err = WrenError(
        kind: weCompile,
        module: $module,
        line: line.int,
        message: $msg
      )
      wvm.compileErrors.add(err)
    of WREN_ERROR_RUNTIME:
      var err = WrenError(
        kind: weRuntime,
        message: $msg
      )
      wvm.rtError = err
    of WREN_ERROR_STACK_TRACE:
      wvm.rtError.stackTrace.add((module: $module,
                                  line: line.int,
                                  message: $msg))
    else: doAssert(false) # unreachable
  # modules
  config.loadModuleFn = proc (vm: RawVM, name: cstring): cstring {.cdecl.} =
    let
      source = cast[Wren](wrenGetUserData(vm)).procLoadModule($name)
      cssource = alloc0((source.len + 1) * sizeof(char))
    if source.len > 0:
      cssource.copyMem(source[0].unsafeAddr, source.len * sizeof(char))
    result = cast[cstring](cssource)
  config.resolveModuleFn = proc (vm: RawVM, importer,
                                 name: cstring): cstring {.cdecl.} =
    let
      source =
        cast[Wren](wrenGetUserData(vm)).procResolveModule($importer, $name)
      cssource = alloc0((source.len + 1) * sizeof(char))
    if source.len > 0:
      cssource.copyMem(source[0].unsafeAddr, source.len * sizeof(char))
      result = cast[cstring](cssource)
    else:
      result = nil
  # FFI
  config.bindForeignMethodFn = proc (vm: RawVM, module: cstring,
                                     class: cstring, isStatic: bool,
                                     name: cstring): WrenForeignMethodFn
                                    {.cdecl.} =
    var wvm = cast[Wren](wrenGetUserData(vm))
    let sign = ($module, $class, $name, isStatic).MethodSign
    if sign in wvm.methods:
      result = wvm.methods[sign]
    else:
      result = nil
  config.bindForeignClassFn = proc (vm: ptr WrenVM, module: cstring,
                                    class: cstring): WrenForeignClassMethods
                                   {.cdecl.} =
    var wvm = cast[Wren](wrenGetUserData(vm))
    let sign = ($module, $class).ClassSign
    if sign in wvm.classes:
      result = wvm.classes[sign]
    else:
      result = WrenForeignClassMethods()
  # memory
  config.reallocateFn = proc (mem: pointer, newSize: csize): pointer {.cdecl.} =
    result = realloc(mem, newSize.Natural)

  result.handle = wrenNewVM(addr config)
  wrenSetUserData(result.handle, cast[pointer](result))

  result.procWrite = proc (str: string) =
    stdout.write(str)
  result.procLoadModule = proc (path: string): string =
    result = ""
  result.procResolveModule = proc (importer, name: string): string =
    result = name

  result.rtError = WrenError(kind: weRuntime)

proc onWrite*(vm: Wren, callback: proc (str: string)) =
  ## Sets the write callback for the VM. This callback is called when the Wren
  ## VM wants to print something out to the console. The default callback simply
  ## writes to stdout.
  vm.procWrite = callback

proc onLoadModule*(vm: Wren, callback: proc (path: string): string) =
  ## Sets the load module callback for the VM. The callback is called when
  ## the VM occurs an ``import`` statement, and doesn't have the given module
  ## loaded yet. The callback should then return the source code of the module
  ## at ``path``. If the callback returns an empty string, a module that aborts
  ## the fiber will be loaded (using ``import`` will throw an error). The
  ## default implementation returns an empty string, so override this if you
  ## want imports to work.
  vm.procLoadModule = callback

proc onResolveModule*(vm: Wren,
                      callback: proc (importer, name: string): string) =
  ## Sets the resolve module callback for the VM. The callback is called when
  ## the VM occurs an ``import`` statement, to resolve what module should
  ## actually be loaded. This is usually used to implement relative imports.
  ## If the callback returns an empty string, the VM will raise an error saying
  ## that the requested module could not be found. The default implementation
  ## simply returns ``name`` without any side effects.
  vm.procResolveModule = callback

proc newRef(vm: Wren, handle: ptr WrenHandle): WrenRef =
  ## Create a new, Nim GC-managed WrenRef out of a raw WrenHandle pointer.
  assert vm != nil
  assert handle != nil
  new(result) do (wref: WrenRef):
    wrenReleaseHandle(wref.vm.handle, wref.handle)
  result.vm = vm
  result.handle = handle

#--
# Low-level APIs
#--

# Ultimately, you shouldn't need to use these APIs. They're inherently unsafe,
# and don't provide any guarantees or assertions. In fact, they're only a thin
# wrapper over the underlying Wren embedding API. They're exported only to
# make the high-level API possible.

# Use with care.

proc ensureSlots*(vm: RawVM, amount: int) =
  wrenEnsureSlots(vm, amount.cint)

proc slotCount*(vm: RawVM): int =
  wrenGetSlotCount(vm)

macro genericParam(T: typed, index: int): untyped =
  ## Get the generic param at position ``index`` from T.
  result = T.getTypeInst[1][index.intVal.int]
macro genericParam(T: typed): untyped =
  ## Get the actual type behind T.
  result = T.getTypeInst[1]

proc genTypeCheck(vm, ty, slot: NimNode): NimNode
macro checkType(vm, ty: typed, slot: int): untyped =
  result = genTypeCheck(vm, ty, slot)

proc getWrenName(typeSym: NimNode): string
macro wrenName(ty: typed): untyped =
  result = newLit(getWrenName(ty))

proc getSlotType*(vm: RawVM, slot: int): WrenType =
  result = wrenGetSlotType(vm, slot.cint).WrenType

proc getSlotForeignId*(vm: RawVM, slot: int): uint16 =
  result = cast[ptr uint16](wrenGetSlotForeign(vm, slot.cint))[]

proc getSlotTypeString*(vm: RawVM, slot: int): string =
  let ty = vm.getSlotType(slot)
  if ty != wtForeign: result = $ty
  else:
    let wvm = cast[Wren](wrenGetUserData(vm))
    result = wvm.typeNames[vm.getSlotForeignId(slot)]

proc getSlotForeign*[T](vm: RawVM, slot: int): ptr T =
  let raw = cast[ptr UncheckedArray[uint16]](wrenGetSlotForeign(vm, slot.cint))
  result = cast[ptr T](raw[1].unsafeAddr)

proc getSlot*[T](vm: RawVM, slot: int): T =
  when T is bool:
    result = wrenGetSlotBool(vm, slot.cint)
  elif T is SomeNumber:
    result = T(wrenGetSlotDouble(vm, slot.cint))
  elif T is enum:
    result = T(wrenGetSlotDouble(vm, slot.cint).int)
  elif T is string:
    var
      len: cint
      bytes = wrenGetSlotBytes(vm, slot.cint, addr len)
    result = newString(len.Natural)
    if len > 0:
      copyMem(result[0].unsafeAddr, bytes, len.Natural)
  elif T is array | seq:
    when T is array:
      const
        P = 2
        Min = ord(genericParam(T, 1).a)
        Max = ord(genericParam(T, 1).b)
        Len = Max - Min + 1
    else:
      const
        P = 1
        Min = 0
    let listLen = wrenGetListCount(vm, slot.cint)
    when T is seq:
      result.setLen(listLen)
    else:
      if listLen != Len:
        vm.abortFiber("got list of length " & $listLen & ", but the expected " &
                      "length is " & $Len)
        return
    let listHandle = wrenGetSlotHandle(vm, slot.cint)
    for i in 0..<listLen:
      wrenGetListElement(vm, slot.cint, i.cint, slot.cint)
      if checkType(vm, genericParam(T, P), slot):
        result[Min + i] = getSlot[genericParam(T, P)](vm, slot)
        wrenSetSlotHandle(vm, slot.cint, listHandle)
      else:
        wrenReleaseHandle(vm, listHandle)
        vm.abortFiber("got <" & vm.getSlotTypeString(slot) & "> in list, " &
                      "but expected <" & wrenName(genericParam(T, P)) & ">")
        return
    wrenReleaseHandle(vm, listHandle)
  elif T is WrenRef:
    result = cast[Wren](wrenGetUserData(vm))
      .newRef(wrenGetSlotHandle(vm, slot.cint))
  elif T is object | tuple | ref:
    result = getSlotForeign[T](vm, slot)[]
  else:
    {.error: "unsupported type for slot retrieval: " & $T.}

proc newForeign*(vm: RawVM, slot: int, size: Natural, classSlot = 0): pointer =
  result = wrenSetSlotNewForeign(vm, slot.cint, classSlot.cint, size.cuint)

proc getVariable*(vm: RawVM, slot: int, module, variable: string) =
  wrenGetVariable(vm, module, variable, slot.cint)

var wrenNames {.compileTime.}: Table[uint16, ModuleVar] ## \
  ## Maps unique type IDs to their corresponding variables in bound modules.

proc getTypeId(typeSym: NimNode): uint16
macro wrenVar(T: typed): untyped =
  let id = getTypeId(T)
  if id notin wrenNames:
    error("type <" & T.repr & "> is unknown to the VM", T)
  let v = wrenNames[id]
  result = newTree(nnkPar, newLit(v.module), newLit(v.variable))

proc genForeignObjectInit(vm, objType, expr, slot: NimNode,
                          exprIsInit = false): NimNode
macro foreignObjectInit(vm, objType, expr: typed, slot: int,
                        exprIsInit = false): untyped =
  result = genForeignObjectInit(vm, objType, expr, slot,
                                exprIsInit.boolVal)

proc setSlot*[T](vm: RawVM, slot: int, val: T) =
  when T is bool:
    wrenSetSlotBool(vm, slot.cint, val)
  elif T is SomeNumber:
    wrenSetSlotDouble(vm, slot.cint, val.cdouble)
  elif T is enum:
    wrenSetSlotDouble(vm, slot.cint, ord(val).cdouble)
  elif T is string:
    wrenSetSlotBytes(vm, slot.cint, val, val.len.cuint)
  elif T is array | seq:
    const P =
      when T is array: 2
      else: 1
    wrenEnsureSlots(vm, cint(slot + 1))
    wrenSetSlotNewList(vm, slot.cint)
    for x in val:
      setSlot[genericParam(T, P)](vm, slot + 1, x)
      wrenInsertInList(vm, slot.cint, -1, cint(slot + 1))
  elif T is WrenRef:
    wrenSetSlotHandle(vm, slot.cint, val.handle)
  elif T is object | tuple | ref:
    let varInfo = wrenVar(genericParam(T))
    vm.getVariable(slot, varInfo[0], varInfo[1])
    foreignObjectInit(vm, genericParam(T), val, slot)
  else:
    {.error: "unsupported type for slot assignment: " & $T.}

proc abortFiber*(vm: RawVM, message: string) =
  vm.setSlot[:string](0, message)
  wrenAbortFiber(vm, 0)

proc checkParent*(vm: RawVM, base, compare: uint16): bool =
  ## Check if the ``compare`` type is one of ``base``'s parent types.
  ## Used internally for type checking with inheritance.
  let wvm = cast[Wren](wrenGetUserData(vm))
  result = base in wvm.parentTypeIds[compare]

proc addTypeInfo*(vm: Wren, id: uint16, name: string, parents: set[uint16]) =
  ## This is an implementation detail used internally by the wrapper.
  ## You should not use this in your code.
  vm.typeNames[id] = name
  vm.parentTypeIds[id] = parents

proc addProc*(vm: Wren, module, class, signature: string, isStatic: bool,
              impl: WrenForeignMethodFn) =
  vm.methods[(module, class, signature, isStatic)] = impl

proc addClass*(vm: Wren, module, name: string,
               destroy: WrenFinalizerFn = nil) =
  vm.classes[(module, name)] = WrenForeignClassMethods(
    allocate: nil,
    finalize: destroy
  )

#--
# End user API - basics
#--

proc getError(vm: Wren, interpretResult: WrenInterpretResult): ref WrenError =
  case interpretResult
  of WREN_RESULT_SUCCESS: discard
  of WREN_RESULT_COMPILE_ERROR:
    var err = new(WrenError)
    err.msg = "compile error"
    for e in vm.compileErrors:
      err.msg &= '\n' & e.module & '(' & $e.line & "): " & e.message
    result = err
  of WREN_RESULT_RUNTIME_ERROR:
    var err = new(WrenError)
    err.msg = vm.rtError.message & "\nwren stack trace:"
    for t in vm.rtError.stackTrace:
      err.msg &= "\n  at " & t.module & '(' & $t.line & ')'
    result = err
  else: doAssert(false) # unreachable

proc checkRuntimeError(vm: Wren, interpretResult: WrenInterpretResult) =
  if interpretResult != WREN_RESULT_SUCCESS:
    raise vm.getError(interpretResult)

proc module*(vm: Wren, name, src: string) =
  ## Runs the provided source code inside of the specified module.
  vm.checkRuntimeError(wrenInterpret(vm.handle, name, src))

proc run*(vm: Wren, src: string) =
  ## Runs the provided source code inside of a module named "main". This should
  ## be used for the entry point of your program. Use ``module`` if you want to
  ## modify the module name (used in error messages and imports).
  vm.module("main", src)

proc `[]`*(vm: Wren, module, variable: string, T: typedesc = WrenRef): T =
  ## Retrieves a variable from the Wren VM. This works both for primitives and
  ## Wren objects (pass ``WrenRef`` to ``T`` to retrieve a Wren object).
  ## If the variable type does not match ``T``, an error will be thrown nagging
  ## the application user.
  wrenEnsureSlots(vm.handle, 1)
  wrenGetVariable(vm.handle, module, variable, 0)
  if not checkType(vm.handle, genericParam(T), 0):
    raise newException(WrenError,
                       "in wren module '" & module & "': variable '" &
                       variable & "' has type <" &
                       vm.handle.getSlotTypeString(0) & ">, but expected <" &
                       wrenName(genericParam(T)) & ">")
  result = getSlot[T](vm.handle, 0)

proc `{}`*(vm: Wren, signature: string): WrenRef =
  ## Creates a 'call handle' to the method denoted by ``methodSignature``.
  result = vm.newRef(wrenMakeCallHandle(vm.handle, signature))

converter toWrenValue*(val: bool): WrenValue =
  WrenValue(kind: wvkBool, boolVal: val)
converter toWrenValue*(val: int): WrenValue =
  WrenValue(kind: wvkNumber, numVal: val.float)
converter toWrenValue*(val: float): WrenValue =
  WrenValue(kind: wvkNumber, numVal: val)
converter toWrenValue*(val: string): WrenValue =
  WrenValue(kind: wvkString, strVal: val)
converter toWrenValue*(val: WrenRef): WrenValue =
  WrenValue(kind: wvkWrenRef, wrenRef: val)

proc call*[T](vm: Wren, theMethod: WrenRef,
              receiver: WrenRef, args: varargs[WrenValue]): T =
  ## Calls the given method with the given arguments. The first argument must
  ## always be present, and is the receiver of the method. The rest of the
  ## arguments is optional. The generic parameter decides on the return type of
  ## the method (which can be void).
  ##
  ## **Design note:** The ``receiver`` param only accepts ``WrenRef``, because
  ## it's pretty much never useful to call a method on a primitive type, since
  ## the native implementation is always faster.
  wrenEnsureSlots(vm.handle, cint(1 + args.len))
  vm.handle.setSlot[:WrenRef](0, receiver)
  for i, arg in args:
    case arg.kind
    of wvkBool: vm.handle.setSlot[:bool](i + 1, arg.boolVal)
    of wvkNumber: vm.handle.setSlot[:float](i + 1, arg.numVal)
    of wvkString: vm.handle.setSlot[:string](i + 1, arg.strVal)
    of wvkWrenRef: vm.handle.setSlot[:WrenRef](i + 1, arg.wrenRef)
  vm.checkRuntimeError(wrenCall(vm.handle, theMethod.handle))
  when T isnot void:
    result = vm.handle.getSlot[:T](0)

#--
# End user API - foreign()
#--

proc getParamList(formalParams: NimNode): seq[NimNode] =
  ## Flattens an nnkFormalParams into a C-like list of argument types,
  ## eg. ``x, y: int`` becomes ``@[int, int]``.
  for identDefs in formalParams[1..^1]:
    let ty =
      if identDefs[^2].kind != nnkEmpty: identDefs[^2]
      else:
        if identDefs[^1].kind != nnkIdent: identDefs[^1].getType
        else: newEmptyNode()
    for i in 0..<identDefs.len - 2:
      result.add(ty)

proc getParamNames(formalParams: NimNode): seq[string] =
  ## Get the names of the parameters in the formalParams as a single list.
  for identDefs in formalParams[1..^1]:
    for name in identDefs[0..^3]:
      result.add(name.strVal)

proc flattenTypeDesc(typeSym: NimNode): NimNode =
  result = typeSym
  while result.typeKind == ntyTypeDesc:
    if result.getTypeInst.kind != nnkBracketExpr:
      # workaround for ``typedesc``
      break
    result = result.getTypeInst[1]

proc eqType(a, b: NimNode): bool =
  ## Compares two ``NimNodes`` to determine if they represent the same type.
  ## Better than ``sameType``, because it deals with ``typedesc``s properly.
  result = sameType(a.flattenTypeDesc, b.flattenTypeDesc) or
           a.kind == nnkEmpty or b.kind == nnkNilLit

proc getOverload(choices: NimNode, params: varargs[NimNode]): NimNode =
  ## Finds an appropriate proc overload based on the provided parameters.
  for overload in choices:
    block check:
      let
        impl = overload.getImpl
        formalParams = impl[3]
        argTypes = getParamList(formalParams)
      # compare ``argTypes`` with ``params``
      if params[0].len != argTypes.len: break check
      for i, param in params[0]:
        if not eqType(argTypes[i], param):
          break check
      return overload
  error("couldn't find overload for given parameter types")

proc getParent(typeSym: NimNode): NimNode =
  ## Get the parent type for the given type symbol, or ``nil`` if the type has
  ## no parent type.
  var impl = typeSym.getImpl[2]
  if impl.kind == nnkTupleTy: return nil # tuples don't have parents
  impl.expectKind({nnkRefTy, nnkObjectTy, nnkTupleTy})
  while impl.kind == nnkRefTy:
    impl = impl[0]
  impl.expectKind(nnkObjectTy)
  if impl[1].kind != nnkEmpty:
    result = impl[1][0]

var
  # compile-time data about types
  typeIds {.compileTime.}: Table[string, uint16]
    ## Maps type hashes to unique integer IDs
  typeNames {.compileTime.}: Table[uint16, string]
    ## Maps the unique IDs to actual names
  parentTypeIds {.compileTime.}: Table[uint16, set[uint16]]
    ## Maps the unique IDs to their parents' IDs

proc getWrenVar(id: uint16): ModuleVar {.compileTime.} =
  result = wrenNames[id]

proc getTypeId(typeSym: NimNode): uint16 =
  ## Get a unique type ID for the given type symbol.
  if typeSym.kind != nnkSym:
    error("<" & typeSym.repr & "> is not a type", typeSym)
  let hash = typeSym.signatureHash
  if hash notin typeIds:
    let
      id = typeIds.len.uint16
      parent = typeSym.getParent
    typeIds[hash] = id
    typeNames[id] = typeSym.repr
    if parent == nil:
      parentTypeIds[id] = {}
    else:
      let parentId = getTypeId(parent)
      parentTypeIds[id] = parentTypeIds[parentId] + {parentId}
  result = typeIds[hash]

proc isRef(class: NimNode): bool =
  ## Checks if the given type symbol represents a ref type.
  result = class.flattenTypeDesc.typeKind == ntyRef

proc newCast(T, val: NimNode): NimNode =
  ## Create a new nnkCast node, which casts ``val`` to ``T``.
  newTree(nnkCast, T, val)

type
  Empty = object ## A dummy object to represent an nnkEmpty.

proc getSlotGetters(params: openArray[array[2, NimNode]],
                    isStatic: bool): seq[NimNode] =
  ## Get a list of getSlot() calls which extract the given parameters from
  ## the VM.
  for i, pair in params:
    let
      slot = newLit(i + ord(isStatic))
      paramType =
        if pair[0] != bindSym"Empty": pair[0]
        else: pair[1].getTypeInst
      getter =
        if paramType.typeKind in {ntyObject, ntyVar} or paramType.isRef:
          if paramType.typeKind == ntyVar:
            newCast(paramType,
                    newCall(newTree(nnkBracketExpr, ident"getSlotForeign",
                                    paramType[0]),
                            ident"vm", slot))
          else:
            newTree(nnkBracketExpr,
                    newCall(newTree(nnkBracketExpr, ident"getSlotForeign",
                                    paramType),
                            ident"vm", slot))
        else:
          newCall(newTree(nnkBracketExpr, ident"getSlot", paramType),
                  ident"vm", slot)
    result.add(getter)

proc genTypeCheck(vm, ty, slot: NimNode): NimNode =
  # type kind sets
  const
    Nums = {ntyInt..ntyUint64, ntyEnum}
    Lists = {ntyArray, ntySequence}
    Foreign = {ntyObject, ntyRef, ntyTuple}
  let ty = ty.flattenTypeDesc
  # generate the check
  let
    wrenType =
      if ty.typeKind == ntyBool: wtBool
      elif ty.typeKind in Nums: wtNumber
      elif ty.typeKind == ntyString: wtString
      elif ty == bindSym"WrenRef": wtUnknown
      elif ty.kind == nnkBracketExpr:
        let subTy = ty[0].flattenTypeDesc
        if subTy.typeKind in Lists: wtList
        else:
          error("generic types besides array and seq are not supported", ty)
          wtUnknown
      elif ty.typeKind in Foreign: wtForeign
      else:
        error("unsupported type kind: " & $ty.typeKind &
              " for <" & ty.repr & ">", ty)
        wtUnknown
    comparison = newTree(nnkInfix, ident"==",
                         newCall("getSlotType", vm, slot),
                         newLit(wrenType))
  result = comparison
  if wrenType == wtForeign:
    let
      typeId = getTypeId(ty)
      typeIdLit = newLit(typeId)
      slotId = newCall("getSlotForeignId", vm, slot)
      idCheck = newTree(nnkInfix, ident"==", slotId, typeIdLit)
      parentCheck = newCall(ident"checkParent", vm, typeIdLit, slotId)
    result = newTree(nnkInfix, ident"and", result,
                     newPar(newTree(nnkInfix, ident"or", idCheck, parentCheck)))

proc genTypeChecks(vm: NimNode, isStatic: bool,
                   types: varargs[NimNode]): NimNode =
  ## Generate a type check condition. This looks at all the params and assembles
  ## a big chain of conditions which check the type.
  ## This is a much better way of checking types compared to the 0.1.0
  ## ``checkTypes``, which simply looped through an array of ``WrenTypeData``
  ## structs and compared them. The current, macro-based version, has much lower
  ## runtime overhead, because it's just a simple chain of conditions.

  # there isn't any work to be done if the proc doesn't accept params
  if types.len == 0 or types.len == ord(not isStatic): return newLit(true)

  # if the first param is var, ignore that
  var types: seq[NimNode] = @types
  if types[0].kind == nnkVarTy:
    types[0] = types[0][0]

  # generate a list of checks
  var checks: seq[NimNode]
  for i, ty in types[ord(not isStatic)..^1]:
    let slot = i + 1
    checks.add(genTypeCheck(vm, ty, newLit(slot)))
  # fold the list of checks to an nnkInfix node
  result = checks[^1]
  for i in countdown(checks.len - 2, 0):
    result = newTree(nnkInfix, ident"and", checks[i], result)

proc getWrenName(typeSym: NimNode): string =
  ## Get the Wren name for the corresponding type. This aliases number types
  ## to ``number``, ``WrenRef`` to ``object``, and any Wren-bound types to
  ## their names in the Wren VM.
  let typeSym = typeSym.flattenTypeDesc
  if typeSym.typeKind in {ntyInt..ntyUint64}: result = "number"
  elif typeSym == bindSym"WrenRef": result = "object"
  elif typeSym.typeKind in {ntyObject, ntyRef} and
       getTypeId(typeSym) in wrenNames:
    let id = getTypeId(typeSym)
    result = wrenNames[id].variable
  else: result = typeSym.repr

proc genTypeError(theProc: NimNode, wrenName: string, arity: int,
                  overloads: varargs[NimNode]): NimNode =
  ## Generate a Nim-like type mismatch error.
  result = newStmtList()
  let
    errVar = newVarStmt(ident"err",
                        newLit("type mismatch: got <"))
    fiberAbort = newCall("abortFiber", ident"vm", ident"err")
  result.add([errVar])
  for i in 1..arity:
    result.add(newCall("add", ident"err",
                       newCall("getSlotTypeString", ident"vm", newLit(i))))
    if i != arity:
      result.add(newCall("add", ident"err", newLit", "))
  var expectedStr = ""
  for overload in overloads:
    let
      impl = overload.getImpl
      params = impl[3]
    expectedStr.add("  " & wrenName & '(')
    for i, defs in params[1..^1]:
      for j, def in defs[0..^3]:
        expectedStr.add(def.repr)
        if j < defs.len - 3:
          expectedStr.add(", ")
      expectedStr.add(": " & getWrenName(defs[^2]))
      if i < params.len - 2:
        expectedStr.add(", ")
    expectedStr.add("): " & getWrenName(params[0]))
  result.add(newCall("add", ident"err",
                     newLit(">\nbut expected one of:\n" & expectedStr)))
  result.add(fiberAbort)

proc genForeignObjectInit(vm, objType, expr, slot: NimNode,
                          exprIsInit = false): NimNode =
  ## Generate a statement list with the initialization procedure for a new
  ## foreign object. ``expr`` is the expression that needs to be called to
  ## initialize the given ``objType``. ``slot`` is the VM slot where the new
  ## foreign object should be stored. It is also the slot where the foreign
  ## class must be stored. If ``exprIsInit`` is true, ``expr`` will be treated
  ## as an initializer instead of a constructor.
  result = newStmtList()
  # create the foreign object
  let
    size = newTree(nnkInfix, ident"+",
                   newCall("sizeof", ident"uint16"),
                   newCall("sizeof", objType))
    dataSym = genSym(nskVar, "data")
    newForeignCall = newCall("newForeign", vm, slot, size, slot)
    u16array = newTree(nnkPtrTy,
                       newTree(nnkBracketExpr,
                              ident"UncheckedArray", ident"uint16"))
  result.add(newVarStmt(dataSym, newCast(u16array, newForeignCall)))
  # assign the type ID
  let typeId = getTypeId(objType)
  result.add(newTree(nnkAsgn,
                     newTree(nnkBracketExpr, dataSym, newLit(0)),
                     newLit(typeId)))
  # assign the data
  let
    objSym = genSym(nskVar, "objectData")
    ptrObjType = newTree(nnkPtrTy, objType)
    objAddr = newTree(nnkAddr, newTree(nnkBracketExpr, dataSym, newLit(1)))
    obj = newTree(nnkDerefExpr, objSym)
  result.add(newVarStmt(objSym, newCast(ptrObjType, objAddr)))
  if exprIsInit:
    let
      initSym = genSym(nskVar, "init")
      initVar = newTree(nnkVarSection,
                        newTree(nnkIdentDefs, initSym, objType, newEmptyNode()))
    result.add(initVar)
    var initExpr = expr
    initExpr[0] = initSym
    result.add(newTree(nnkAsgn, obj, initExpr))
  else:
    result.add(newTree(nnkAsgn, obj, expr))
    if objType.isRef:
      result.add(newCall("GC_ref", obj))

proc genForeignErrorCheck(expr: NimNode): NimNode =
  ## Wraps ``expr`` in a try…except statement, which, in case of error, aborts
  ## the current fiber with the caught exception's error message.
  ## If in a debug build, the message will also contain the stack traceback.
  result = newNimNode(nnkTryStmt)
  result.add(newStmtList(expr))
  var
    branch = newNimNode(nnkExceptBranch)
    branchStmts = newStmtList()
  let
    errSym = genSym(nskLet, "error")
    msgSym = genSym(nskVar, "errorMessage")
  branchStmts.add(newLetStmt(errSym, newCall("getCurrentException")))
  branchStmts.add(newVarStmt(msgSym, newTree(nnkDotExpr, errSym, ident"msg")))
  branchStmts.add(newCall("add", msgSym, newLit(" [")))
  branchStmts.add(newCall("add", msgSym,
                          newTree(nnkDotExpr, errSym, ident"name")))
  branchStmts.add(newCall("add", msgSym, newLit(']')))
  when compileOption("stacktrace"):
    branchStmts.add(newCall("add", msgSym, newLit("\nnim stack trace:\n")))
    branchStmts.add(newCall("add", msgSym, newCall("getStackTrace", errSym)))
  branchStmts.add(newCall("abortFiber", ident"vm", msgSym))
  branch.add(branchStmts)
  result.add(branch)

proc orEmpty(node: NimNode): NimNode =
  result =
    if node.kind == nnkEmpty: bindSym"Empty"
    else: node

proc genProcGlue(theProc: NimNode, wrenName: string,
                 isStatic, isGetter: bool,
                 params: openArray[array[2, NimNode]]): NimNode =
  ## Generate a glue procedure with type checks and VM slot conversions.

  # get some metadata about the proc
  let
    procImpl = theProc.getImpl
    procParams = getParamList(procImpl[3])
    procRetType = procImpl[3][0]
  # create a new anonymous proc; this is our resulting glue proc
  result = newProc(params = [newEmptyNode(),
                             newIdentDefs(ident"vm", ident"RawVM")])
  result.addPragma(ident"cdecl")
  var body = newStmtList()
  # generate the call
  let
    call = newCall(theProc, getSlotGetters(params, isStatic))
    callWithReturn =
      # no return type
      if procRetType.kind == nnkEmpty or eqIdent(procRetType, "void"): call
      # some return type
      else: newCall(newTree(nnkBracketExpr, ident"setSlot", procRetType),
                    ident"vm", newLit(0), call)
    callWithTry = genForeignErrorCheck(callWithReturn)
  # generate type check
  let typeCheck = genTypeChecks(ident"vm", isStatic, procParams)
  body.add(newIfStmt((cond: typeCheck, body: callWithTry))
           .add(newTree(nnkElse, genTypeError(theProc, wrenName,
                                              procParams.len, theProc))))
  result.body = body

proc genSignature(theProc: NimNode, wrenName: string,
                  isStatic, isGetter: bool, namedParams = false): string =
  ## Generate a Wren signature for the given proc and its properties.
  ## If ``namedParams`` is true, a 'nice' signature will be generated with
  ## parameter names embedded into it.

  var
    param = ord(not isStatic)
    paramNames = getParamNames(theProc.getImpl[3])
    arity = paramNames.len
  proc params(n: int): string =
    ## Generate a string of params like _,_,_,_
    if namedParams:
      for i in 1..n:
        result.add(paramNames[param])
        if i != n:
          result.add(',')
        inc(param)
    else:
      for i in 1..n:
        result.add('_')
        if i != n:
          result.add(',')

  var name = wrenName
  name.removePrefix('`')
  name.removeSuffix('`')
  if not isGetter:
    let arity = arity - ord(not isStatic)
    if name == "[]":
      result = '[' & arity.params & ']'
    elif name == "[]=":
      result = '[' & (arity - 1).params & "]=(" & params(1) & ")"
    else:
      result = name & '(' & arity.params & ')'
  else:
    result = name

macro genProcAux(vm: Wren, module: string, className: string,
                 theProc: typed, wrenName: static string,
                 isStatic, isGetter: static bool,
                 params: varargs[typed]): untyped =
  ## Second step of binding a proc, generates code which binds it to the
  ## provided Wren instance.

  # unpack ``params`` into the correct type
  var paramList: seq[array[2, NimNode]]
  for i in countup(0, params.len - 1, 2):
    paramList.add([params[i], params[i + 1]])
  # call ``addProc``
  let
    classLit = className
    nameLit = newLit(genSignature(theProc, wrenName, isStatic, isGetter))
  result = newStmtList()
  result.add(newCall("addProc", vm, module,
                     classLit, nameLit, newLit(isStatic),
                     genProcGlue(theProc, wrenName, isStatic, isGetter,
                                 paramList)))
  var wrenDecl = "foreign "
  if isStatic:
    wrenDecl.add("static ")
  wrenDecl.add(genSignature(theProc, wrenName, isStatic, isGetter,
                            namedParams = true))
  wrenDecl.add('\n')
  result.add(newCall("add", ident"classMethods", newLit(wrenDecl)))

proc resolveOverload(procSym: NimNode, overloaded: bool,
                     params: varargs[NimNode]): NimNode =
  ## Resolve the overload of ``procSym``. If ``overloaded`` is true, overload
  ## parameters were provided during binding, and ``params`` are the desired
  ## overload's parameters.
  result = procSym
  if procSym == nil: return
  if procSym.kind != nnkSym:
    if not overloaded:
      error("multiple overloads available; " &
            "provide the correct overload's parameters", procSym)
    result = getOverload(procSym, params)

macro addProcAux(vm: Wren, module: string, className: string,
                 procSym: typed, wrenName: string,
                 overloaded: static bool, isStatic, isGetter: bool,
                 params: varargs[typed]): untyped =
  ## First step of binding a proc, resolves the overload using the given params
  ## and defers the binding to ``genProcAux``. This is a workaround for
  ## Nim/#12942, to make the bound procedure's parameters ``typed``.

  # as a workaround for Nim/#12831, unwrap the procSym if it's
  # in an nnkTypeOfExpr
  var procSym = procSym
  if procSym.kind == nnkTypeOfExpr:
    procSym = procSym[0]
  # find the correct overload of the procedure, if applicable
  let
    theProc = resolveOverload(procSym, overloaded, params)
    procImpl = theProc.getImpl
  # defer to genProcAux
  result = newCall(bindSym"genProcAux", vm, module, className, theProc,
                   wrenName, isStatic, isGetter)
  # retrieve [type, defaultValue] pairs from the proc's params
  for defs in procImpl[3][1..^1]:
    let
      ty = defs[^2].orEmpty
      default = defs[^1].orEmpty
    for _ in 0..<defs.len - 2:
      result.add([ty, default])

proc genDestroyGlue(vm, class: NimNode): NimNode =
  ## Generates glue code for the destructor.
  ## Special action must be done when the object is a ref object, to call
  ## ``GC_unref`` and free its memory.
  ## Otherwise, this proc returns a nil literal.
  if class.isRef:
    # create the destructor proc
    result = newProc(params = [newEmptyNode(),
                               newIdentDefs(ident"rawPtr", ident"pointer")])
    result.addPragma(ident"cdecl")
    var body = newStmtList()
    # create some variables, which are conversions of ``rawPtr``
    let
      u16Var = newLetStmt(ident"u16",
                          newCast(parseExpr"ptr UncheckedArray[uint16]",
                                  ident"rawPtr"))
      dataPtr = newCall("unsafeAddr", newTree(nnkBracketExpr,
                                              ident"u16", newLit(1)))
      dataVar = newVarStmt(ident"foreignData",
                           newCast(newTree(nnkPtrTy, class), dataPtr))
    body.add([u16Var, dataVar])
    # if dealing with a GD'd type, unref it
    if class.isRef:
      body.add(newCall("GC_unref", newTree(nnkDerefExpr, ident"foreignData")))
    result.body = body
  else:
    result = newNilLit()

proc genFieldGetter(name, ty, field, fieldTy: NimNode): NimNode =
  ## Generates a wrapper procedure that retrieves a value from a field.
  result = newProc(name, params = [fieldTy])
  result.params.add(newTree(nnkIdentDefs, ident"x", ty, newEmptyNode()))
  result.body.add(newTree(nnkAsgn, ident"result",
                          newTree(nnkDotExpr, ident"x", field)))

proc genFieldSetter(name, ty, field, fieldTy: NimNode): NimNode =
  ## Generates a wrapper procedure that sets a field's value.
  result = newProc(name)
  let paramTy =
    if ty.isRef: ty
    else: newTree(nnkVarTy, ty)
  result.params.add(newTree(nnkIdentDefs, ident"x", paramTy, newEmptyNode()))
  result.params.add(newTree(nnkIdentDefs, ident"val", fieldTy, newEmptyNode()))
  result.body.add(newTree(nnkAsgn, newTree(nnkDotExpr, ident"x", field),
                          ident"val"))

proc genGetSet(vm, class, module, identDefs: NimNode,
               wrenClass: string): NimNode =
  ## Generates the necessary glue getters, glue setters, and ``addProcAux``
  ## calls for the given ``identDefs`` to wrap the fields of an object.
  result = newStmtList()
  for def in identDefs[0..^3]:
    if def.kind == nnkPostfix and def[0].strVal == "*":
      let
        getSym = genSym(nskProc, "objGet")
        setSym = genSym(nskProc, "objSet")
        getProc = genFieldGetter(getSym, class, def[1], identDefs[^2])
        setProc = genFieldSetter(setSym, class, def[1], identDefs[^2])
      result.add([getProc, setProc])
      result.add(newCall(bindSym"addProcAux", vm, module,
                         newLit(wrenClass), getSym, newLit(def[1].strVal),
                         newLit(false), newLit(false), newLit(true)))
      result.add(newCall(bindSym"addProcAux", vm, module,
                         newLit(wrenClass), setSym, newLit(def[1].strVal & '='),
                         newLit(false), newLit(false), newLit(false)))

proc genFieldGlue(vm, class, module: NimNode, wrenClass: string): NimNode =
  ## Generates code which binds the given object's fields to the VM, by
  ## generating glue procedures that get or set the fields and using
  ## ``addProcAux`` to bind them.
  result = newStmtList()
  var ty = class.getImpl[2]
  while ty.kind == nnkRefTy:
    ty = ty[0]
  if ty.kind != nnkObjectTy: return
  let parent = getParent(class)
  if parent != nil:
    result.add(genFieldGlue(vm, parent, module, wrenClass))
  for rec in ty[2]:
    # fields
    if rec.kind == nnkIdentDefs:
      result.add(genGetSet(vm, class, module, rec, wrenClass))
    elif rec.kind == nnkRecCase:
      for branch in rec:
        if branch.kind == nnkIdentDefs:
          result.add(genGetSet(vm, class, module, branch, wrenClass))
        elif branch.kind == nnkOfBranch:
          result.add(genGetSet(vm, class, module, branch[1], wrenClass))

proc genTupleConstrGlue(vm, class, module: NimNode,
                        wrenClass: string): NimNode =
  ## Generates a glue proc that constructs a tuple.
  result = newStmtList()
  let
    tupleImpl = class.getImpl[2]
    constrSym = genSym(nskProc, class.repr & "_init")
  var
    constrProc = newProc(constrSym, params = [class])
    tupleConstr = newNimNode(nnkPar)
  for defs in tupleImpl:
    constrProc.params.add(defs)
    for field in defs[0..^3]:
      tupleConstr.add(newTree(nnkExprColonExpr, field, field))
  constrProc.body.add(newTree(nnkAsgn, ident"result", tupleConstr))
  result.add(constrProc)
  result.add(newCall(bindSym"addProcAux", vm, module, newLit(wrenClass),
                     constrSym, newLit("new"),
                     newLit(false), newLit(true), newLit(false)))

macro addClassAux(vm: Wren, module: string, class: typed,
                  wrenClass: string, fields: bool): untyped =
  ## Generates code which binds a new class to the provided Wren instance.
  ## This is an implementation detail and you should not use it in your code.

  # generate all the glue procs
  let destroyGlue = genDestroyGlue(vm, class)
  result = newStmtList()
  if fields.boolVal:
    result.add(genFieldGlue(vm, class, module, wrenClass.strVal))
  if class.flattenTypeDesc.typeKind == ntyTuple:
    result.add(genTupleConstrGlue(vm, class, module, wrenClass.strVal))
  result.add(newCall("addClass", vm, module, wrenClass, destroyGlue))

macro saveWrenName(class: typed, module, wrenClass: string) =
  when defined(euwrenDumpClasses):
    echo "[euwren] ", escape(module.strVal), ".", wrenClass.strVal
  wrenNames[getTypeId(class)] = (module.strVal, wrenClass.strVal)

proc getOverloadParams(def: NimNode): seq[NimNode] =
  ## Returns the overload's parameters as a raw seq.
  for param in def[1..^1]:
    if param.kind == nnkIdent and param.strVal == "_":
      result.add(newNilLit())
    else:
      result.add(param)

proc getAddProcAuxCall(vm, module, class: NimNode, wrenClass: string,
                       theProc: NimNode, wrenName: string,
                       isStatic, isGetter = false): NimNode =
  # non-overloaded proc binding
  if theProc.kind in {nnkIdent, nnkAccQuoted, nnkSym}:
    # defer the binding to addProcAux
    result = newCall(bindSym"addProcAux", vm, module, newLit(wrenClass),
                     newCall("typeof", theProc), newLit(wrenName),
                     newLit(false), newLit(isStatic), newLit(isGetter))
  # overloaded/getter proc binding
  elif theProc.kind in {nnkCall, nnkCommand}:
    var callArgs = @[vm, module, newLit(wrenClass),
                     newCall("typeof", theProc[0]), newLit(wrenName),
                     newLit(true), newLit(isStatic), newLit(isGetter)]
    # bind the overloaded proc
    callArgs.add(getOverloadParams(theProc))
    result = newCall(bindSym"addProcAux", callArgs)

proc isWrenIdent(str: string): bool =
  ## Checks if the given string represents a valid Wren identifier.
  ## Wren identifiers have different rules from Nim identitiers—namely, the can
  ## have trailing underscores, and two or more consecutive underscores.
  if str.len == 0 or str[0] == '_' or str[0] notin IdentStartChars:
    return false
  for c in str:
    if c notin IdentChars:
      return false
  result = true

proc getAlias(decl: NimNode): tuple[nim: NimNode, wren: string] =
  ## Extract the Nim name and Wren name from the given declaration.
  if decl.kind == nnkInfix and decl[0].strVal == "->":
    decl[2].expectKind({nnkIdent, nnkStrLit, nnkAccQuoted})
    var alias = decl[2]
    if alias.kind == nnkAccQuoted:
      alias = alias[0]
    if alias.kind == nnkStrLit:
      if not alias.strVal.isWrenIdent:
        error("not a valid Wren identifier", alias)
    result = (nim: decl[1], wren: alias.strVal)
  elif decl.kind == nnkPrefix:
    if decl[1].kind == nnkCall:
      result = (nim: decl, wren: decl[1][0].repr)
    elif decl[1].kind in {nnkIdent, nnkAccQuoted}:
      result = (nim: decl, wren: decl[1].repr)
    else:
      error("invalid binding", decl)
  elif decl.kind == nnkCall:
    result = (nim: decl, wren: decl[0].repr)
  elif decl.kind in {nnkIdent, nnkAccQuoted}:
    result = (nim: decl, wren: decl.repr)
  else:
    error("invalid binding", decl)

proc getClassAlias(decl: NimNode): tuple[class, procs: NimNode,
                                         isNamespace: bool, wren: string] =
  ## A version of ``getAlias`` for class declarations. Uses ``getAlias``
  ## internally, so the syntax stays consistent.
  if decl.kind == nnkInfix:
    let (nim, wren) = getAlias(decl)
    result = (class: nim, procs: decl[3], isNamespace: false, wren: wren)
  elif decl.kind == nnkCall:
    result = (class: decl[0], procs: decl[1], isNamespace: false,
              wren: decl[0].repr)
    if result.class.kind == nnkBracket:
      result.class = result.class[0]
      result.isNamespace = true
      result.wren = result.class.repr
  else:
    error("invalid binding", decl)

proc genClassBinding(vm, module, decl: NimNode): NimNode =
  ## Generate a set of calls that binds a set of procedures, and optionally, a
  ## Nim object, from the AST provided in ``decl``. This is part of the
  ## ``foreign()`` DSL.
  var stmts = newStmtList()
  stmts.add(newVarStmt(ident"classMethods", newLit("")))
  let (class, procs, isNamespace, wrenClass) = getClassAlias(decl)
  var classDecl = "class " & wrenClass & "{\n"
  if not isNamespace:
    classDecl = "foreign " & classDecl
    stmts.add(newCall(bindSym"saveWrenName", class, module, newLit(wrenClass)))
  var bindFields = true
  for p in procs:
    if p.kind == nnkDiscardStmt: discard
    elif p.kind == nnkPragma:
      p[0].expectKind(nnkIdent)
      case p[0].strVal
      of "noFields": bindFields = false
    elif p.kind in {nnkStrLit..nnkTripleStrLit}:
      stmts.add(newCall("add", ident"classMethods", p))
    else:
      let (nim, wren) = getAlias(p)
      var
        theProc = nim
        isStatic = isNamespace
        isGetter = false
      if nim.kind == nnkPrefix:
        case nim[0].strVal
        of "*":
          isStatic = true
          if isNamespace:
            warning("explicit static marker in namespace; " &
                    "did you mean to create an object?", nim[0])
        of "?": isGetter = true
        of "*?", "?*":
          isStatic = true
          isGetter = true
        theProc = nim[1]
      if nim.kind in {nnkPrefix, nnkCall} and nim[^1].kind == nnkDo:
        var procDef = newNimNode(nnkProcDef)
        theProc = genSym(nskProc, wren)
        nim[^1].copyChildrenTo(procDef)
        procDef[0] = theProc
        stmts.add(procDef)
      stmts.add(getAddProcAuxCall(vm, module, class, wrenClass, theProc, wren,
                                  isStatic, isGetter))
  if not isNamespace:
    stmts.add(newCall(bindSym"addClassAux", vm, module, class,
                      newLit(wrenClass), newLit(bindFields)))
  stmts.add(newCall("add", ident"modSrc", newLit(classDecl)))
  stmts.add(newCall("add", ident"modSrc", ident"classMethods"))
  stmts.add(newCall("add", ident"modSrc", newLit("}\n")))
  result = newBlockStmt(stmts)

macro genEnumAux(theEnum: typed, wrenName, prefix: string): untyped =
  ## Generates the code required to bind to a Wren VM.
  result = newStmtList()
  let
    impl = theEnum.getImpl[2]
    classDecl = "class " & wrenName.strVal & "{\n"
  var
    i = -1
    enumFields = ""
  for field in impl[1..^1]:
    var name = ""
    if field.kind == nnkIdent:
      name = field.repr
      inc(i)
    elif field.kind == nnkEnumFieldDef:
      name = field[0].repr
      if field[1].kind == nnkIntLit:
        i = field[1].intVal.int
      elif field[1].kind == nnkTupleConstr:
        i = field[1][0].intVal.int
      else:
        inc(i)
    name.removePrefix(prefix.strVal)
    enumFields.add("static " & name & "{" & $i & "}\n")
  result.add(newCall("add", ident"modSrc", newLit(classDecl)))
  result.add(newCall("add", ident"modSrc", newLit(enumFields)))

  template propertyField(property: string) =
    result.add(newCall("add", ident"modSrc",
                       newLit("static " & property & "{")))
    result.add(newCall("add", ident"modSrc",
                       newTree(nnkPrefix, ident"$",
                               newCall("ord", newCall(property, theEnum)))))
    result.add(newCall("add", ident"modSrc", newLit("}\n")))
  propertyField("low")
  propertyField("high")

  result.add(newCall("add", ident"modSrc", newLit("}\n")))

proc getGenEnumAuxCall(theEnum: NimNode, wrenName, prefix: string): NimNode =
  ## Create a call to ``getEnumAux``, binding the given enum, with the given
  ## ``wrenName``, and optionally a ``prefix`` to be stripped from the enum. If
  ## ``prefix`` is ``""``, nothing will be stripped.
  result = newCall(bindSym"genEnumAux", theEnum, newLit(wrenName),
                   newLit(prefix))

proc genEnumBinding(decl: NimNode): NimNode =
  ## Generates an enum binding from the AST of ``decl``. This is part of the
  ## ``foreign()`` DSL.
  if decl.kind == nnkIdent:
    result = getGenEnumAuxCall(decl, decl.repr, "")
  elif decl.kind == nnkInfix:
    case decl[0].strVal
    of "-":
      decl[1].expectKind(nnkIdent)
      decl[2].expectKind(nnkIdent)
      result = getGenEnumAuxCall(decl[1], decl[1].repr, decl[2].strVal)
    of "->":
      let (nim, wren) = getAlias(decl)
      if nim.kind == nnkIdent:
        result = getGenEnumAuxCall(nim, wren, "")
      elif nim.kind == nnkInfix:
        nim[1].expectKind(nnkIdent)
        nim[2].expectKind(nnkIdent)
        result = getGenEnumAuxCall(nim[1], wren, nim[2].strVal)
      else:
        error("invalid enum declaration", nim)
    else: error("invalid enum declaration operator", decl[0])

macro foreign*(vm: Wren, module: string, body: untyped): untyped =
  ## Bind foreign things into the Wren VM. Refer to the README for details on
  ## how to use this.
  body.expectKind(nnkStmtList)
  var
    # we begin with an empty module
    # this module is later used in a ``module()`` call
    stmts = newTree(nnkStmtList, newVarStmt(ident"modSrc", newLit("")))
  for decl in body:
    case decl.kind
    of nnkCall:
      # class bindings
      stmts.add(genClassBinding(vm, module, decl))
    of nnkIdent, nnkInfix:
      # enums or aliased objects
      if decl.kind == nnkInfix and decl[^1].kind == nnkStmtList:
        stmts.add(genClassBinding(vm, module, decl))
      else:
        stmts.add(genEnumBinding(decl))
    of nnkStrLit..nnkTripleStrLit:
      # module code injections
      stmts.add(newCall("add", ident"modSrc", decl))
      stmts.add(newCall("add", ident"modSrc", newLit('\n')))
    of nnkDiscardStmt: discard # discard
    else:
      # any other bindings are invalid
      error("invalid foreign binding", decl)
  when defined(euwrenDumpForeignModule):
    stmts.add(newCall("echo", ident"modSrc"))
  stmts.add(newCall("module", vm, module, ident"modSrc"))
  result = newBlockStmt(stmts)

macro ready*(vm: Wren): untyped =
  ## Arms the VM for foreign code execution. This **must** be called after
  ## any and all ``foreign()`` calls are done and before you execute any code
  ## that uses foreign data inside Wren, otherwise type checking will not work
  ## properly.
  result = newStmtList()
  for id, name in typeNames:
    let parents = parentTypeIds[id]
    result.add(newCall(ident"addTypeInfo", vm,
                       newLit(id), newLit(name), newLit(parents)))
  typeIds.clear()
  typeNames.clear()
  parentTypeIds.clear()
  wrenNames.clear()

