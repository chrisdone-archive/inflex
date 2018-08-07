# template-psc-package

Reproducible template for a psc-package-using project

* The PureScript source code is in `src/Main.purs`.
* The dependencies are specified in `psc-package.json`.
* The compiler and build tool are both specified in `stack.yaml`, so
  that this repository is always reproducible.

This repo gets you started!

## Build

Build the PureScript tooling:

```
$ stack build
```

Build your PureScript project:

```
$ stack exec psc-package build
```

Produce an `app.js`:

```
$ stack exec -- purs bundle 'output/**/*.js' -m Main --main Main -o app.js
```

Now open `index.html` and look at the console.

## Stack version

    Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e85a579565 (5807 commits) x86_64 hpack-0.28.2
