# template-psc-package

## Build

Build the PureScript tooling:

```
$ stack build happy purescript psc-package
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

# Dev

    $ stack exec purs ide server

    $ watchexec -p -w --exts js -w output -- bash bundle.sh
