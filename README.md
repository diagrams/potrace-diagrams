## potrace-diagrams

Convert bitmap images to [diagrams] using [potrace]. Containers helpers
for converting [JuicyPixel] images to bitmaps.

### Installing

To install [bindings-potrace] you'll need to install the
[potrace-library]. This should be in your systems package manager.

### mac installation

With [homebrew]:

```
brew install potrace
```

`cabal` may complain about not finding the potrace library. In which can
you can add the `--extra-lib-dirs=/usr/local/lib
--extra-include-dirs=/usr/local/include` flags when `cabal install`ing.

[diagrams]: https://github.com/diagrams
[potrace]: https://github.com/cchalmers/potrace
[potrace-library]: http://potrace.sourceforge.net
[Hackage]: https://hackage.haskell.org
[JuicyPixel]: https://github.com/Twinside/Juicy.Pixels
[bindings-potrace]: https://github.com/rwbarton/bindings-potrace
[homebrew]: http://brew.sh

