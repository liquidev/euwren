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

  result.handle = wrenNewVM(addr config)
  wrenSetUserData(result.handle, cast[pointer](result))

  result.procWrite = proc (str: string) =
    stdout.write(str)

  result.rtError = WrenError(kind: weRuntime)

proc onWrite*(vm: Wren, callback: proc (str: string)) =
  ## Sets the write callback for the VM. This callback is called when the Wren
  ## VM wants to print something out to the console. The default callback simply
  ## writes to stdout.
  vm.procWrite = callback

#--
# Low-level APIs
#--

# Ultimately, you shouldn't need to use these APIs. They're inherently unsafe,
# and don't provide any guarantees or assertions. In fact, they're only a thin
# wrapper over the underlying Wren embedding API.

# Use with care.

proc ensureSlots*(vm: RawVM, amount: int) =
  wrenEnsureSlots(vm, amount.cint)

proc slotCount*(vm: RawVM): int =
  wrenGetSlotCount(vm)

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
  elif T is WrenRef:
    new(result) do (wr: WrenRef):
      wrenReleaseHandle(wr.vm.handle, wr.handle)
    result = WrenRef(vm: cast[Wren](wrenGetUserData(vm)),
                     handle: wrenGetSlotHandle(vm, slot.cint))
  elif T is object or T is ref object:
    let
      raw = cast[ptr UncheckedArray[uint16]](wrenGetSlotForeign(vm, slot.cint))
      obj = cast[ptr T](raw[1].unsafeAddr)
    result = obj[]
  else:
    {.error: "unsupported type for slot retrieval".}

proc getSlotForeign*[T](vm: RawVM, slot: int): ptr T =
  let raw = cast[ptr UncheckedArray[uint16]](wrenGetSlotForeign(vm, slot.cint))
  result = cast[ptr T](raw[1].unsafeAddr)

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

proc newForeign*(vm: RawVM, slot: int, size: Natural, classSlot = 0): pointer =
  result = wrenSetSlotNewForeign(vm, slot.cint, classSlot.cint, size.cuint)

proc getVariable*(vm: RawVM, slot: int, module, variable: string) =
  wrenGetVariable(vm, module, variable, slot.cint)

proc setSlot*[T](vm: RawVM, slot: int, val: T) =
  when T is bool:
    wrenSetSlotBool(vm, slot.cint, val)
  elif T is SomeNumber:
    wrenSetSlotDouble(vm, slot.cint, val.cdouble)
  elif T is enum:
    wrenSetSlotDouble(vm, slot.cint, ord(val).cdouble)
  elif T is string:
    wrenSetSlotBytes(vm, slot.cint, val, val.len.cuint)
  elif T is WrenRef:
    wrenSetSlotHandle(vm, slot.cint, val.handle)
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
               construct: WrenForeignMethodFn,
               destroy: WrenFinalizerFn = nil) =
  vm.classes[(module, name)] = WrenForeignClassMethods(
    allocate: construct,
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

proc `[]`*(vm: Wren, module, variable: string, T: typedesc): T =
  ## Retrieves a variable from the Wren VM. This is a version for primitives
  ## and foreign objects. If you need to access a Wren object, use the version
  ## that doesn't accept a ``typedesc``.
  # TODO: add checks that make sure the type is correct.
  wrenEnsureSlots(vm.handle, 1)
  wrenGetVariable(vm.handle, module, variable, 0)
  result = getSlot[T](vm.handle, 0)

proc `[]`*(vm: Wren, module, variable: string): WrenRef =
  ## Retrieves a variable from the Wren VM. This is only really useful for
  ## working with Wren objects, since you can't access them directly. For
  ## primitives, use the version that accepts an additional ``typedesc``.
  new(result) do (wr: WrenRef):
    wrenReleaseHandle(wr.vm.handle, wr.handle)
  wrenEnsureSlots(vm.handle, 1)
  wrenGetVariable(vm.handle, module, variable, 1)
  result.vm = vm
  result.handle = wrenGetSlotHandle(vm.handle, 1)

proc `{}`*(vm: Wren, signature: string): WrenRef =
  ## Creates a 'call handle' to the method denoted by ``methodSignature``.
  new(result) do (wm: WrenRef):
    wrenReleaseHandle(wm.vm.handle, wm.handle)
  result.vm = vm
  result.handle = wrenMakeCallHandle(vm.handle, signature)

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
    let ty = identDefs[^2]
    for i in 0..identDefs.len - 3:
      result.add(ty)

proc getParamNames(formalParams: NimNode): seq[string] =
  ## Get the names of the parameters in the formalParams as a single list.
  for identDefs in formalParams[1..^1]:
    for name in identDefs[0..^3]:
      result.add(name.strVal)

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
        if argTypes[i] != param:
          break check
      return overload
  error("couldn't find overload for given parameter types")

proc getParent(typeSym: NimNode): NimNode =
  ## Get the parent type for the given type symbol, or ``nil`` if the type has
  ## no parent type.
  var impl = typeSym.getImpl[2]
  impl.expectKind({nnkRefTy, nnkObjectTy})
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
  wrenNames {.compileTime.}: Table[uint16, ModuleVar]
    ## Maps the unique IDs to Wren names
  parentTypeIds {.compileTime.}: Table[uint16, set[uint16]]
    ## Maps the unique IDs to their parents' IDs

proc getTypeId(typeSym: NimNode): uint16 =
  ## Get a unique type ID for the given type symbol.
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
  if class.typeKind == ntyRef:
    result = true
  elif class.typeKind == ntyTypeDesc:
    let impl = class.getImpl
    if impl[2].kind == nnkRefTy:
      result = true

proc newCast(T, val: NimNode): NimNode =
  ## Create a new nnkCast node, which casts ``val`` to ``T``.
  newTree(nnkCast, T, val)

proc getSlotGetters(params: seq[NimNode], isStatic: bool): seq[NimNode] =
  ## Get a list of getSlot() calls which extract the given parameters from
  ## the VM.
  for i, paramType in params:
    let
      slot = newLit(i + ord(isStatic))
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

proc genTypeCheck(types: varargs[NimNode], isStatic: bool): NimNode =
  ## Generate a type check condition. This looks at all the params and assembles
  ## a big chain of conditions which check the type.
  ## This is a much better way of checking types compared to the 0.1.0
  ## ``checkTypes``, which simply looped through an array of ``WrenTypeData``
  ## structs and compared them. The current, macro-based version, has much lower
  ## runtime overhead, because it's just a simple chain of confitions.

  # type kind sets
  const
    Nums = {ntyInt..ntyUint64}
    Foreign = {ntyObject, ntyRef}

  # there isn't any work to be done if the proc doesn't accept params
  if types.len == ord(not isStatic): return newLit(true)

  # if the first param is var, ignore that
  var types: seq[NimNode] = @types
  if types[0].kind == nnkVarTy:
    types[0] = types[0][0]

  # generate a list of checks
  var checks: seq[NimNode]
  for i, ty in types[ord(not isStatic)..^1]:
    let
      slot = i + 1
      wrenType =
        if ty.typeKind == ntyBool: wtBool
        elif ty.typeKind in Nums or ty.typeKind == ntyEnum: wtNumber
        elif ty.typeKind == ntyString: wtString
        elif ty == bindSym"WrenRef": wtUnknown
        elif ty.typeKind in Foreign: wtForeign
        else:
          echo "[euwren] type is of an unsupported kind: ", ty.repr
          error("unsupported type kind: " & $ty.typeKind, ty)
          wtUnknown
      comparison = newTree(nnkInfix, ident"==",
                           newCall("getSlotType", ident"vm", newLit(slot)),
                           newLit(wrenType))
    checks.add(comparison)
    if wrenType == wtForeign:
      let
        typeId = getTypeId(ty)
        typeIdLit = newLit(typeId)
        slotId = newCall("getSlotForeignId", ident"vm", newLit(slot))
        idCheck = newTree(nnkInfix, ident"==", slotId, typeIdLit)
        parentCheck = newCall(ident"checkParent", ident"vm", typeIdLit, slotId)
      checks.add(newPar(newTree(nnkInfix, ident"or", idCheck, parentCheck)))

  # fold the list of checks to an nnkInfix node
  result = checks[^1]
  for i in countdown(checks.len - 2, 0):
    result = newTree(nnkInfix, ident"and", checks[i], result)

proc getWrenName(typeSym: NimNode): string =
  ## Get the Wren name for the corresponding type. This aliases number types
  ## to ``number``, ``WrenRef`` to ``object``, and any Wren-bound types to
  ## their names in the Wren VM.
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

proc genForeignObjectInit(vm, objType, expr: NimNode, slot: int,
                          exprIsInit = false): NimNode =
  ## Generate a statement list with the initialization procedure for a new
  ## foreign object. ``expr`` is the expression that needs to be called to
  ## initialize the given ``objType``. ``slot`` is the VM slot where the new
  ## foreign object should be stored. If ``exprIsInit`` is true, ``expr`` will
  ## be treated as an initializer instead of a constructor.
  result = newStmtList()
  # create the foreign object
  let
    size = newTree(nnkInfix, ident"+",
                   newCall("sizeof", ident"uint16"),
                   newCall("sizeof", objType))
    dataSym = genSym(nskVar, "data")
    slotLit = newLit(slot)
    newForeignCall = newCall("newForeign", vm, slotLit, size, slotLit)
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
    obj = newTree(nnkBracketExpr, objSym)
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
  when not defined(release) and not defined(danger):
    branchStmts.add(newCall("add", msgSym, newLit("\nnim stack trace:\n")))
    branchStmts.add(newCall("add", msgSym, newCall("getStackTrace", errSym)))
  branchStmts.add(newCall("abortFiber", ident"vm", msgSym))
  branch.add(branchStmts)
  result.add(branch)

proc genProcGlue(theProc: NimNode, wrenName: string,
                 isGetter, isStatic: bool): NimNode =
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
    call = newCall(theProc, getSlotGetters(procParams, isStatic))
    callWithReturn =
      # no return type
      if procRetType.kind == nnkEmpty or eqIdent(procRetType, "void"): call
      # some return type
      else:
        # object return type
        if (procRetType.typeKind == ntyObject or procRetType.isRef) and
           procRetType != bindSym"WrenRef":
          let retTypeId = getTypeId(procRetType)
          var stmts = newStmtList()
          if retTypeId notin wrenNames:
            error("[euwren] return type unknown to the VM, " &
                  "register a {.dataClass.}", procRetType)
          let varInfo = wrenNames[retTypeId]
          stmts.add(newCall("getVariable", ident"vm", newLit(0),
                            newLit(varInfo.module), newLit(varInfo.variable)))
          stmts.add(genForeignObjectInit(ident"vm", procRetType, call, 0))
          stmts
        # primitive return type
        else:
          newCall(newTree(nnkBracketExpr, ident"setSlot", procRetType),
                  ident"vm", newLit(0), call)
    callWithTry = genForeignErrorCheck(callWithReturn)
  # generate type check
  let typeCheck = genTypeCheck(procParams, isStatic)
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
          result.add(", ")
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

macro addProcAux*(vm: Wren, module: string, classSym: typed, className: string,
                  procSym: typed, wrenName: static string,
                  overloaded, isGetter: static bool,
                  params: varargs[typed]): untyped =
  ## Generates code which binds a procedure to the provided Wren instance.
  ## This is an implementation detail and you should not use it in your code.

  # as a workaround for Nim/#12831, unwrap the procSym if it's
  # in an nnkTypeOfExpr
  var procSym = procSym
  if procSym.kind == nnkTypeOfExpr:
    procSym = procSym[0]
  # find the correct overload of the procedure, if applicable
  var theProc = resolveOverload(procSym, overloaded, params)
  # get some metadata about the proc
  let
    procImpl = theProc.getImpl
    procParams = getParamList(procImpl[3])
  # generate glue and register the procedure in the Wren instance
  let
    classLit = className
    firstParam =
      if classSym.isRef: @[classSym]
      else: @[classSym, newTree(nnkVarTy, classSym)]
    isStatic = procParams.len < 1 or procParams[0] notin firstParam
    isStaticLit = newLit(isStatic)
    nameLit = newLit(genSignature(theProc, wrenName, isStatic, isGetter))
  result = newStmtList()
  result.add(newCall("addProc", vm, module,
                     classLit, nameLit, isStaticLit,
                     genProcGlue(theProc, wrenName, isGetter, isStatic)))
  var wrenDecl = "foreign "
  if isStatic:
    wrenDecl.add("static ")
  wrenDecl.add(genSignature(theProc, wrenName, isStatic, isGetter,
                            namedParams = true))
  wrenDecl.add('\n')
  result.add(newCall("add", ident"classMethods", newLit(wrenDecl)))

type
  InitProcKind = enum
    ipNone
    ipInit
    ipNew

proc genInitGlue(vm, class, theProc: NimNode, kind: InitProcKind): NimNode =
  ## Generates a glue init procedure with checks and type conversions.

  # get some metadata about the proc
  let
    procImpl = theProc.getImpl
    procParams = getParamList(procImpl[3])
    procRetType = procImpl[3][0]
  # do some extra checks to see if the passed proc is usable
  if kind == ipInit:
    if procParams[0] != newTree(nnkVarTy, class) and
       not procParams[0].isRef:
      error("first parameter of [init] proc must be var or ref", theProc)
    if not (procRetType.kind == nnkEmpty or procRetType == bindSym"void"):
      error("return type for [init] proc must be void", theProc)
  # create the resulting init proc
  result = newProc(params = [newEmptyNode(),
                             newIdentDefs(ident"vm", ident"RawVM")])
  result.addPragma(ident"cdecl")
  var body = newStmtList()
  # create the necessary variables and add type metadata
  let
    # the raw Wren instance
    rawVM = newDotExpr(vm, ident"handle")
    # raw memory, this includes the type ID prepended before the actual data
    sizeofU16 = newCall("sizeof", ident"uint16")
    sizeofClass = newCall("sizeof", class)
    foreignSize = newTree(nnkInfix, ident"+", sizeofU16, sizeofClass)
    newForeignCall = newCall("newForeign", rawVM, newLit(0), foreignSize)
    rawMemVar = newVarStmt(ident"rawMem",
                           newCast(parseExpr"ptr UncheckedArray[uint16]",
                                   newForeignCall))
    # the object pointer
    foreignData = newCall("unsafeAddr", newTree(nnkBracketExpr,
                                                ident"rawMem", newLit(1)))
    dataVar = newVarStmt(ident"foreignData",
                         newCast(newTree(nnkPtrTy, class), foreignData))
    # the type ID assignment
    typeIdAssign = newAssignment(newTree(nnkBracketExpr,
                                         ident"rawMem", newLit(0)),
                                 newLit(getTypeId(class)))
  body.add([rawMemVar, dataVar, typeIdAssign])
  # generate the type check
  let initParams =
    # the params for object construction, excluding the first param in case
    # of initializer
    if kind == ipInit: procParams[1..^1]
    else: procParams
  let typeCheck = genTypeCheck(initParams, isStatic = true)
  # finally, initialize or construct the object
  var initBody = newStmtList()
  case kind
  of ipNone: discard
  of ipInit:
    # initializer
    initBody.add(newCall("reset", newCast(newTree(nnkVarTy, class),
                                          ident"foreignData")))
    var initCallParams = @[newCast(newTree(nnkVarTy, class),
                                   ident"foreignData")]
    initCallParams.add(getSlotGetters(initParams, true))
    let initCall = newCall(theProc, initCallParams)
    initBody.add(initCall)
  of ipNew:
    # constructor
    let
      ctorCall = newCall(theProc, getSlotGetters(initParams, true))
      dataAssign = newAssignment(newTree(nnkBracketExpr, ident"foreignData"),
                                 ctorCall)
    initBody.add(dataAssign)
    if procRetType.isRef:
      initBody.add(newCall("GC_ref",
                           newTree(nnkBracketExpr, ident"foreignData")))
  body.add(newIfStmt((cond: typeCheck, body: initBody))
           .add(newTree(nnkElse, genTypeError(theProc, "new",
                                              procParams.len, theProc))))
  result.body = body

proc genDestroyGlue(vm, class, procSym: NimNode): NimNode =
  ## Generates glue code for the destructor.
  ## Special action must be done when:
  ## - a destructor is provided, to execute it
  ## - the object is a ref object, to call GC_unref and free its memory
  ## Otherwise, this proc returns a nil literal.
  if procSym.kind != nnkNilLit or class.isRef:
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
    # run the user-provided destructor, if applicable
    if procSym.kind != nnkNilLit:
      # resolve the overload
      var theProc = procSym
      if theProc.kind != nnkSym:
        let param =
          if class.isRef: class
          else: newTree(nnkVarTy, class)
        theProc = getOverload(theProc, param)
        if theProc == nil:
          error("no suitable destructor found", theProc)
      # call the destructor
      let destructorParam =
        if class.isRef: newTree(nnkBracketExpr, ident"foreignData")
        else: newCast(newTree(nnkVarTy, class), ident"foreignData")
      body.add(newCall(theProc, destructorParam))
    # if dealing with a GD'd type, unref it
    if class.isRef:
      body.add(newCall("GC_unref", newTree(nnkBracketExpr, ident"foreignData")))
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
      result.add(newCall("addProcAux", vm, module, class, newLit(wrenClass),
                         getSym, newLit(def[1].strVal),
                         newLit(false), newLit(true)))
      result.add(newCall("addProcAux", vm, module, class, newLit(wrenClass),
                         setSym, newLit(def[1].strVal & '='),
                         newLit(false), newLit(false)))

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

macro addClassAux*(vm: Wren, module: string, class: typed, wrenClass: string,
                   initProc, destroyProc: typed,
                   initProcKind: static InitProcKind,
                   initOverloaded: static bool,
                   initParams: varargs[typed]): untyped =
  ## Generates code which binds a new class to the provided Wren instance.
  ## This is an implementation detail and you should not use it in your code.

  # generate all the glue procs
  let
    initOverload = resolveOverload(initProc, initOverloaded, initParams)
    initGlue =
      if initProc == nil: newNilLit()
      else: genInitGlue(vm, class, initOverload, initProcKind)
    destroyGlue = genDestroyGlue(vm, class, destroyProc)
  result = newStmtList()
  result.add(genFieldGlue(vm, class, module, wrenClass.strVal))
  result.add(newCall("addClass", vm, module, wrenClass,
                     initGlue, destroyGlue))
  if initProc != nil:
    # generate the Wren glue code
    let ctorDecl = "construct " &
                   genSignature(initOverload, "new",
                                isStatic = true, isGetter = false,
                                namedParams = true) &
                   " {}\n"
    result.add(newCall("add", ident"classMethods", newLit(ctorDecl)))
  # save the Wren type name
  wrenNames[getTypeId(class)] = (module.strVal, wrenClass.strVal)

proc getOverloadParams(def: NimNode): seq[NimNode] =
  ## Returns the overload's parameters as a raw seq.
  for param in def[1..^1]:
    result.add(param)

proc getAddProcAuxCall(vm, module, class: NimNode, wrenClass: string,
                       theProc: NimNode, wrenName: string,
                       isObject: bool, isGetter = false): NimNode =
  # get the class metadata, depending on whether we're binding a namespace or
  # an object
  let
    classSym =
      if isObject: class
      else: newNilLit()
  # non-overloaded proc binding
  if theProc.kind in {nnkIdent, nnkAccQuoted}:
    # defer the binding to addProcAux
    # XXX: find a way which doesn't require addProcAux to be public
    result = newCall(ident"addProcAux", vm, module,
                     classSym, newLit(wrenClass),
                     newCall("typeof", theProc), newLit(wrenName),
                     newLit(false), newLit(isGetter))
    if isGetter:
      result[7] = newLit(true) # mark as overloaded
      result.add(classSym)
  # overloaded/getter proc binding
  elif theProc.kind in {nnkCall, nnkCommand}:
    var callArgs = @[vm, module, classSym, newLit(wrenClass),
                     newCall("typeof", theProc[0]), newLit(wrenName),
                     newLit(true), newLit(isGetter)]
    # bind the overloaded proc
    callArgs.add(getOverloadParams(theProc))
    result = newCall(ident"addProcAux", callArgs)

proc getAddClassAuxCall(vm, module, class: NimNode, wrenClass: string,
                        initProc, destroyProc: NimNode,
                        initProcKind: InitProcKind): NimNode =
  # non-overloaded or nil init proc
  if initProc == nil or initProc.kind == nnkIdent:
    result = newCall(ident"addClassAux", vm, module, class, newLit(wrenClass),
                     initProc, destroyProc, newLit(initProcKind), newLit(false))
  # overloaded init proc
  else:
    var callArgs = @[vm, module, class, newLit(wrenClass),
                     initProc[0], destroyProc,
                     newLit(initProcKind), newLit(true)]
    callArgs.add(getOverloadParams(initProc))
    result = newCall(ident"addClassAux", callArgs)

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
  elif decl.kind == nnkCall:
    result = (nim: decl, wren: decl[0].repr)
  elif decl.kind == nnkIdent:
    result = (nim: decl, wren: decl.repr)
  else:
    error("invalid binding", decl)

proc getClassAlias(decl: NimNode): tuple[class, procs: NimNode, wren: string] =
  ## A version of ``getAlias`` for class declarations. Uses ``getAlias``
  ## internally, so the syntax stays consistent.
  if decl.kind == nnkInfix:
    let (nim, wren) = getAlias(decl)
    result = (class: nim, procs: decl[3], wren: wren)
  elif decl.kind == nnkCall:
    result = (class: decl[0], procs: decl[1], wren: decl[0].repr)
  else:
    error("invalid binding", decl)

proc genClassBinding(vm, module, decl: NimNode): NimNode =
  ## Generate a set of calls that binds a set of procedures, and optionally, a
  ## Nim object, from the AST provided in ``decl``. This is part of the
  ## ``foreign()`` DSL.
  var stmts = newStmtList()
  stmts.add(newVarStmt(ident"classMethods", newLit("")))
  let (class, procs, wrenClass) = getClassAlias(decl)
  var classDecl = "class " & wrenClass & " {\n"
  # object-specific procedures
  # If at least procInit or procNew is not nil, a class will be created
  # ``procInit`` and ``procNew`` are mutually exclusive, only one can
  # be present at a time
  # ``procDestroy`` is optional but one of the initializer procs must
  # be present
  # If none are present, no foreign class will be created, and the procs
  # will be bound to a namespace
  var
    procInit, procDestroy: NimNode = nil
    initProcKind = ipNone
    isObject = false
  for p in procs:
    if p.kind == nnkCommand and p[0].kind == nnkBracket:
      # annotated binding
      # [init], [new], [destroy], [get]
      p.expectLen(2)
      p[0][0].expectKind(nnkIdent)
      let annotation = p[0][0].strVal
      case annotation
      of "init":
        if procInit != nil:
          error("class may only have one constructing proc", p)
        procInit = p[1]
        initProcKind = ipInit
        isObject = true
      of "new":
        if procInit != nil:
          error("class may only have one constructing proc", p)
        procInit = p[1]
        initProcKind = ipNew
        isObject = true
      of "destroy":
        procDestroy = p[1]
      of "get":
        let (nim, wren) = getAlias(p[1])
        stmts.add(getAddProcAuxCall(vm, module, class, wrenClass, nim, wren,
                                    isObject, isGetter = true))
      else: error("invalid annotation", p[0][0])
    elif p.kind in {nnkStrLit..nnkTripleStrLit}:
      # module code injection
      stmts.add(newCall("add", ident"classMethods", p))
    elif p.kind == nnkPragma:
      # pragmas
      for pragma in p:
        if pragma.kind == nnkIdent:
          case pragma.strVal
          of "dataClass":
            # make the class a 'data class' (an object without a constructor)
            isObject = true
            stmts.add(getAddClassAuxCall(vm, module, class, wrenClass,
                                         procInit, procDestroy, initProcKind))
    elif p.kind == nnkDiscardStmt: discard # discard statement
    else:
      # regular binding
      let (nim, wren) = getAlias(p)
      stmts.add(getAddProcAuxCall(vm, module, class, wrenClass,
                                  nim, wren, isObject))
  if procInit != nil:
    stmts.add(getAddClassAuxCall(vm, module, class, wrenClass,
                                 procInit, procDestroy, initProcKind))
    classDecl = "foreign " & classDecl
  stmts.add(newCall("add", ident"modSrc", newLit(classDecl)))
  stmts.add(newCall("add", ident"modSrc", ident"classMethods"))
  stmts.add(newCall("add", ident"modSrc", newLit("}\n")))
  result = newBlockStmt(stmts)

macro genEnumAux*(theEnum: typed, wrenName, prefix: string): untyped =
  ## Generates the code required to bind to a Wren VM. This is an implementation
  ## detail used by ``foreign()``, and may only be used in the context created
  ## by it; thus, you should not use it in your code.
  result = newStmtList()
  let
    impl = theEnum.getImpl[2]
    classDecl = "class " & wrenName.strVal & " {\n"
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
    enumFields.add("static " & name & " { " & $i & " }\n")
  result.add(newCall("add", ident"modSrc", newLit(classDecl)))
  result.add(newCall("add", ident"modSrc", newLit(enumFields)))

  template propertyField(property: string) =
    result.add(newCall("add", ident"modSrc",
                       newLit("static " & property & " { ")))
    result.add(newCall("add", ident"modSrc",
                       newTree(nnkPrefix, ident"$",
                               newCall("ord", newCall(property, theEnum)))))
    result.add(newCall("add", ident"modSrc", newLit(" }\n")))
  propertyField("low")
  propertyField("high")

  result.add(newCall("add", ident"modSrc", newLit("}\n")))

proc getGenEnumAuxCall(theEnum: NimNode, wrenName, prefix: string): NimNode =
  ## Create a call to ``getEnumAux``, binding the given enum, with the given
  ## ``wrenName``, and optionally a ``prefix`` to be stripped from the enum. If
  ## ``prefix`` is ``""``, nothing will be stripped.
  result = newCall("genEnumAux", theEnum, newLit(wrenName), newLit(prefix))

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
  ## that uses foreign data inside Wren, otherwise type checking will not work.
  result = newStmtList()
  for id, name in typeNames:
    let parents = parentTypeIds[id]
    result.add(newCall(ident"addTypeInfo", vm,
                       newLit(id), newLit(name), newLit(parents)))
  typeIds.clear()
  typeNames.clear()
  parentTypeIds.clear()

