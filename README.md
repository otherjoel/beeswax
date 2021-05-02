beeswax
=======

(This is an unfinished proof of concept. It might or might not be something I decide to clean up,
publish and maintain.)

Beeswax is designed to fit seamlessly into [Pollen][pp] projects but could be applicable in other
projects. It gives you a `#lang` for string-based templates, as well as some facilities for applying
template modules to source modules. The advantages of this over Pollen’s string-based templates:

1. When templates have their own `#lang`, they can be better supported by IDEs like DrRacket: REPL
   interactions, syntax highlighting, etc.
2. Renders can be quite a bit faster.

A Beeswax template is identical to a Pollen template except for the addition of the `#lang` line at
the top:

    #lang beeswax/template

    <html>
    <head>
    <title>◊(hash-ref metas 'title)</title>
    </head><body>
    
    ◊(->html doc) </body></html>

This file is a Racket module that exports a function called `template-fill` which has a signature
like `(any/c hash? pagenode?) . -> . bytes?)` — that is, you pass it `doc`, `metas` and an output
path, and it will render them into the template, write the result to an output file, and give you
back the bytes of the rendered result.

Beeswax templates have a file naming scheme similar to Pollen’s: instead of `template.OUT[.p]` you
would use `template.OUT.rkt` (where `OUT` is the file extension for the target output format). Any
individual source file can also specify a template (of any name) using the `'template` key of its
`metas` export (just as with normal Pollen templates).

Pollen can be instructed to delegate renders to Beeswax by adding a couple of lines to the `setup`
submodule of your project’s `pollen.rkt`:

```racket
#lang racket

(module setup racket/base
  (provide external-renderer)
  (define external-renderer '(beeswax/for-pollen external-renderer)))
```

This way you can use both `raco pollen render` and the Pollen project server as you normally would,
and your output files will be rendered using the Beeswax template files. (Note, though, that
updating a Beeswax template file will not invalidate Pollen’s cache; you’ll have to bust the cache
manually, or reset it, or disable it in order to see your changes if all you’ve touched is the
template module.)

You can get an even bigger speed improvement by using `raco beeswax render source.html.pm`, which
works similarly to `raco pollen render`.


[pp]: https://pollenpub.com
[it]: https://docs.racket-lang.org/web-server/templates.html#%28form._%28%28lib._web-server%2Ftemplates..rkt%29._include-template%29%29
[ev]: https://docs.racket-lang.org/reference/eval.html#%28def._%28%28quote._~23~25kernel%29._eval%29%29
