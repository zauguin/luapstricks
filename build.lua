module = "luapstricks"

typesetexe = "lualatex"
stdengine = "luatex"
checkengines = {"luatex"}

installfiles = {"luapstricks.lua", "PSTricksDotFont.otf"}
sourcefiles = {"luapstricks.lua", "pstricks.tex", "PSTricksDotFont.otf"}
binaryfiles = {"*.zip", "*.pdf", "PSTricksDotFont.otf"}
testsuppdir = "PSTricksFiles"
-- typesetfiles = {"luapstricks.tex"}

tdsroot = "lualatex"

uploadconfig = {
  pkg = module,
  version = "v0.5",
  author = "Marcel Krüger",
  license = "lppl1.3",
  summary = "A PSTricks backend for LuaLaTeX",
  ctanPath = "/graphics/pstricks/contrib/luapstricks",
  update = true,
  repository = "https://github.com/zauguin/luapstricks",
  bugtracker = "https://github.com/zauguin/luapstricks/issues",
  topic = {"luatex", "pstricks"},
  -- announcement_file = "announce",
  description = "luapstricks enables the use of PSTricks directly in LuaLaTeX documents, without invoking external programmes. Therefore it does not require shell escape to be enabled or special environments and instead allows PSTricks to be used exactly like in dvips based documents.",
}
