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

<!-- - [pst-user](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-user.pdf) Main (historical) documentation without overlay stuff (is now in pst-ovl) -->
- [pstricks-add](https://luapstricks.typesetting.eu/version/v0.9/doc/pstricks-add-doc.pdf)
- [pst-3d](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-3d-doc.pdf)
- [pst-3dplot](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-3dplot-doc.pdf)
- [pst-am](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-am-doc.pdf)
- [pst-antiprism](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-antiprism-doc.pdf)
- [pst-barcode](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-barcode-doc.pdf)
- [pst-bezier](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-bezier-doc.pdf)
- [pst-cie](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-cie-doc.pdf)
- [pst-circ](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-circ-doc.pdf)
- [pst-coil](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-coil-doc.pdf)
- [pst-dart](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-dart-doc.pdf)
- [pst-eucl](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-eucl-doc.pdf)
- [pst-func](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-func-doc.pdf)
- [pst-knot](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-knot-doc.pdf)
- [pst-node](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-node-doc.pdf)
- [pst-ovl](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-ovl-doc.pdf)
- [pst-platon](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-platon-doc.pdf)
- [pst-plot](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-plot-doc.pdf)
- [pst-poly](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-poly-doc.pdf)
- [pst-solarsystem](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-solarsystem-doc.pdf)
- [pst-soroban](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-soroban-doc.pdf)
- [pst-spinner](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-spinner-doc.pdf)
- [pst-stru](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-stru-doc.pdf)
- [pst-tools](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-tools-doc.pdf)
- [pst-tree](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-tree-doc.pdf)
- [pst-turtle](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-turtle-doc.pdf)
- [pst-vehicle](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-vehicle-doc.pdf)
- [pst-venn](https://luapstricks.typesetting.eu/version/v0.9/doc/pst-venn-doc.pdf)
