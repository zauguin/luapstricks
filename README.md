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

Documentation of PSTricks packages, created by `lualatex` and `luapstricks.lua`:

- [pst-user](https://hvoss.org/PSTexa/pst-user.pdf) Main (historical) documentation without overlay stuff (is now in pst-ovl)
- [pstricks-add](https://hvoss.org/PSTexa/pstricks-add-doc.pdf)
- [pst-3d](https://hvoss.org/PSTexa/pst-3d-doc.pdf)
- [pst-3dplot](https://hvoss.org/PSTexa/pst-3dplot-doc.pdf)
- [pst-am](https://hvoss.org/PSTexa/pst-am-doc.pdf)
- [pst-antiprism](https://hvoss.org/PSTexa/pst-antiprism-doc.pdf)
- [pst-barcode](https://hvoss.org/PSTexa/pst-barcode-doc.pdf)
- [pst-bezier](https://hvoss.org/PSTexa/pst-bezier-doc.pdf)
- [pst-cie](https://hvoss.org/PSTexa/pst-cie-doc.pdf)
- [pst-circ](https://hvoss.org/PSTexa/pst-circ-doc.pdf)
- [pst-coil](https://hvoss.org/PSTexa/pst-coil-doc.pdf)
- [pst-dart](https://hvoss.org/PSTexa/pst-dart-doc.pdf)
- [pst-eucl](https://hvoss.org/PSTexa/pst-eucl-doc.pdf)
- [pst-func](https://hvoss.org/PSTexa/pst-func-doc.pdf)
- [pst-knot](https://hvoss.org/PSTexa/pst-knot-doc.pdf)
- [pst-node](https://hvoss.org/PSTexa/pst-node-doc.pdf)
- [pst-ovl](https://hvoss.org/PSTexa/pst-ovl-doc.pdf)
- [pst-platon](https://hvoss.org/PSTexa/pst-platon-doc.pdf)
- [pst-plot](https://hvoss.org/PSTexa/pst-plot-doc.pdf)
- [pst-poly](https://hvoss.org/PSTexa/pst-poly-doc.pdf)
- [pst-solarsystem](https://hvoss.org/PSTexa/pst-solarsystem-doc.pdf)
- [pst-soroban](https://hvoss.org/PSTexa/pst-soroban-doc.pdf)
- [pst-spinner](https://hvoss.org/PSTexa/pst-spinner-doc.pdf)
- [pst-stru](https://hvoss.org/PSTexa/pst-stru-doc.pdf)
- [pst-tools](https://hvoss.org/PSTexa/pst-tools-doc.pdf)
- [pst-tree](https://hvoss.org/PSTexa/pst-tree-doc.pdf)
- [pst-turtle](https://hvoss.org/PSTexa/pst-turtle-doc.pdf)
- [pst-vehicle](https://hvoss.org/PSTexa/pst-vehicle-doc.pdf)
- [pst-venn](https://hvoss.org/PSTexa/pst-venn-doc.pdf)
