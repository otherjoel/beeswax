beeswax
=======

This is an unfinished proof of concept. It might or might not be something I decide to clean up,
publish and maintain.

The idea is that you could create a module using Pollen syntax like this:

    #lang beeswax

    <html>
    <head>
    <title>◊(hash-ref metas 'title)</title>
    </head><body>
    
    ◊(->html doc) </body></html>

…and save it as template.html.rkt. This file would now be a Racket module that exports a function
called `render` which has a signature like `(any/c hash? . -> . bytes?)` — that is, you pass it
`doc`, `metas`, and it will render them into the template and give you back the bytes of the
rendered result.

That part is working now.

The next piece is implementing a `raco` command like `raco beeswax render file.html`, which would
work the same as `raco pollen render` with two differences:

1. When searching for the template to use, instead of looking for `template.html` it will look for
  `template.html.rkt`. (It will also check the metas for a value attached to `'template` the same
  way Pollen does.)
  
2. Instead of loading the contents of the file using [`include-template`][it] within [`eval`][ev], it will
  treat the template as a normal Racket module, and call its exported `render` function.
  
[it]: https://docs.racket-lang.org/web-server/templates.html#%28form._%28%28lib._web-server%2Ftemplates..rkt%29._include-template%29%29
[ev]: https://docs.racket-lang.org/reference/eval.html#%28def._%28%28quote._~23~25kernel%29._eval%29%29

## Advantages

* When templates have their own `#lang`, they can be better supported by IDEs like DrRacket: REPL
  interactions, syntax highlighting, etc.
* This approach *might** be faster.
