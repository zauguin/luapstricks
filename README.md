# `luapstricks` -- A PSTricks backend for LuaTeX

Support PSTricks directly in LuaTeX, without requiring external processes, special environments or similar.

Since recent PSTricks versions load `luapstricks` automatically when LuaLaTeX is used, this does not require changes to the document.
Some advanced features require the `pdfmanagement-testphase` package to be loaded and activated.

## Usage

1. Make sure that you have the latest pstricks version installed.
2. Run `l3build install` *or* copy `luapstricks.lua` from this repository into the directory of your TeX files or another directory in your TeX search path.
3. Compile your document with `lualatex`.
4. Given that most people immediately stop reading after they get a result and you are still here, the previous stage probably failed. Write a bug report to the author. Otherwise go to step 7.
5. Wait for a fix.
6. Go back to step 2 and try again.
7. It worked? That's great. Feel free to inform the author anyway and share the awesome images you created.

## Issues and feature requests

Should you find a bug or want to request a feature, please report it using the [issue tracker](https://github.com/zauguin/luapstricks/issues).

## License

This code is licensed under the LaTeX Project Public License 1.3.

## Acknowledgments

This project would never have reached a usable state without all the testing and suggestions by Pablo González Luengo.
Also many thanks to Herbert Voß for adapting PSTricks and related packages to work with luapstricks.

## Demos

Documentation of PSTricks packages, created by `lualatex` and `luapstricks.lua`, can be found at <https://luapstricks.typesetting.eu/>.
