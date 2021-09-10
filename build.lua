module = "luapstricks"

typesetexe = "lualatex"
stdengine = "luatex"
checkengines = {"luatex"}

installfiles = {"luapstricks.lua"}
sourcefiles = {"luapstricks.lua", "pstricks.tex", "pstricks.pro"}
testsuppdir = "PSTricksFiles"
-- typesetfiles = {"luapstricks.tex"}

tdsroot = "luatex"

uploadconfig = {
  pkg = module,
  version = "v0.1",
  author = "Marcel Krüger",
  license = "lppl1.3",
  summary = "A pstricks backend for LuaLaTeX",
  ctanPath = "/macros/luatex/generic/luapstricks",
  update = false,
  repository = "https://github.com/zauguin/luapstricks",
  bugtracker = "https://github.com/zauguin/luapstricks/issues",
  topic = {"luatex", "pstricks"},
  announcement_file = "announce",
}
