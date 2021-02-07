beeswax
=======

This doesn’t work yet.

The idea is that you can create a module using Pollen syntax like this:

    #lang beeswax

    <html>
    <head>
    <title>◊(hash-ref metas 'title)</title>
    </head><body>
    
    ◊(->html doc) </body></html>

…and save it as template.html.rkt. This file would now be a Racket module that exports a function
called `render` which has a signature like `(any/c hash? path-string? . -> . bytes?)` — that is, you
pass it `doc`, `metas`, and an output filename and it will render them into the template and save
the output to the specified file.

To render a Pollen document, you would do something like `raco beeswax render file.html` which works
the same as `raco pollen render` except it looks for a matching `.rkt` template and calls its
`render` function.
