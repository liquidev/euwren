import nimterop/build
import nimterop/cimport

const
  Base = getProjectCacheDir("euwren")

setDefines(@["wrenStatic", "wrenGit", "wrenSetVer=0.2.0"])

getHeader("src/include/wren.h",
          gitUrl = "https://github.com/wren-lang/wren.git",
          outdir = Base)

cImport(wrenPath)
