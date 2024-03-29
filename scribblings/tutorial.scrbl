#lang scribble/manual

@require[@for-label[beeswax/template
                    beeswax/for-pollen
                    racket/base
                    pollen/core
                    pollen/pagetree
                    pollen/template]
         beeswax/private/constants
         racket/file
         racket/runtime-path
         "sandbox.rkt"
         scribble/example]

@title{Using Beeswax in a Pollen project}

The simplest way to understand Beeswax is to see it in use in its native habitat: a Pollen project.
So let’s make a very small, simple one.

In an empty folder, save a Pollen source document:

@(clear-sandbox-compile-cache!)
@filebox["fleas.html.pm" @codeblock{
#lang pollen

◊(define-meta title "Lines on the Antiquity of Microbes")

◊pre{
Adam
Had ’em.
}}]

In the same folder, save an HTML template:

@filebox["template.html" @verbatim{
<html>
  <title>[POLLEN was here]</title>
  <h1>◊(hash-ref metas 'title)</h1>
  ◊(->html doc)
</html>
}]

Our sample Pollen project is now fully operational; you can @exec{raco pollen render fleas.html} and
Pollen will create a @filepath{fleas.html} that matches our template.

@margin-note{You might have noticed we don’t have a @filepath{pollen.rkt} file in this project.
We’ll add it in a bit.}

@section{Beeswax conversion}

We’re now going to create a Beeswax template.

Step one: open the same @filepath{template.html} in DrRacket and add the line @racketmodfont{#lang
beeswax/template} to the top, like this:

@filebox["template.html.rkt" @codeblock{
#lang beeswax/template

<html>
  <title>[BEESWAX was here]</title>
  <h1>◊(hash-ref metas 'title)</h1>
  ◊(->html doc)
</html>
}]

(Also change @tt{POLLEN} to @tt{BEESWAX} in the @tt{<title>} tag as shown. This isn’t strictly
necessary, but it will help make clear later on which template file we’re using.)

Step two: save the modified template to a @bold{new} file with a @filepath{.rkt} extension:
@filepath{template.html.rkt}.

There, your template is now a Beeswax template! Beeswax templates use exactly the same syntax and
facilities as normal Pollen templates, so adding the @hash-lang[] line at the top and renaming the
file is generally all you’ll need to do to convert an existing template.

You’ll notice DrRacket is now giving you the white-glove treatment: syntax highlighting, definition
arrows, and a helpful toolbar button.

Within a @racketmodname[beeswax/template] file, you have access to all the same things you do inside
a normal Pollen template: @racketid[doc], @racketid[metas], @racketid[here], a number of Pollen
functions , and of course everything @racket[provide]d from a nearby @filepath{pollen.rkt} file.

@subsection{Your template is now a module}

Everything in a @code{#lang beeswax/template} file is wrapped in a function that is
@racket[provide]d to other files. (This is similar to how @code{#lang pollen} files wrap their
contents into the provided values @racketid[doc] and @racketid[metas].) The function is always named
@racket[#,template-proc-provide] and it takes three arguments: @racket[_doc] (which can be any
value), @racket[_metas] (a hash table), and @racket[_here], an output path in symbol form (i.e., a
@racket[pagenode?]).

You can see this by clicking the @onscreen{Run} button at the top of the DrRacket window for
@filepath{template.html.rkt} and poking around a little in the REPL:

@(sandbox '(require "template.html.rkt")) 
@examples[#:eval sandbox #:label #false
          apply-template]

When you call this function, it renders the values you pass it into the template and returns the
bytes of the rendered result. Try calling @racket[#,template-proc-provide] with some placeholder
values for @racket[_doc], @racket[_metas] and @racket[_here]:

@examples[#:eval sandbox
          #:label #false
          (display (apply-template "What’s up doc?"
                                   (hash 'title "Test")
                                   'test.html))]

It’s not much of a leap at this point to render the @filepath{fleas.html.pm} Pollen source file we
created:

@examples[#:eval sandbox
          #:label #false
          (code:comment @#,elem{Get doc and metas})
          (require "fleas.html.pm") 
          (display (apply-template doc metas 'fleas.html))]

@section{Pollen integration}

At this point, Pollen itself does not know anything about @filepath{template.html.rkt}. If you tell
Pollen to render @filepath{fleas.html} you’ll see it renders the file using the old
@filepath{template.html} template:

@ensure-pollen-rkt['absent]
@void[(sandbox-raco "pollen" '#("reset"))]

@terminal{
@:>{raco pollen render -f fleas.html}
@sandbox-raco["pollen" '#("render" "-f" "fleas.html")]

@:>{cat fleas.html}
@file->string[(sample-file "fleas.html")]
}

But there is a way to tell Pollen to use Beeswax for the rendering step. In the same folder as
@filepath{fleas.html.pm} create a new file @filepath{pollen.rkt}:

@filebox["pollen.rkt" @codeblock{
#lang racket/base

(module setup racket/base
  (define external-renderer '(beeswax/for-pollen external-renderer))
  (provide external-renderer))
}]

Now render the file again:

@ensure-pollen-rkt['present]
@terminal{
@:>{raco pollen render -f fleas.html}
@sandbox-raco["pollen" '#("render" "-f" "fleas.html")]

@:>{cat fleas.html}
@file->string[(sample-file "fleas.html")]
}

The output of the first command shows that Pollen has involved Beeswax in the rendering process
(“@exec{pollen: beeswax rendered …}”. Looking at the contents of @filepath{fleas.html} confirms that
the Beeswax template was used this time, not the Pollen one.

When Pollen sees a value for @racketid[external-renderer] provided by the @racket[setup] submodule
of a local @filepath{pollen.rkt} file, instead of rendering the file itself, it calls the function
specified, and uses whatever it gets back as the rendered result.

The function @racket[external-renderer] provided by @racketmodname[beeswax/for-pollen] does its own work
to find the right Beeswax template (in this case @filepath{template.html.rkt}). It then uses that file’s
@racket[#,template-proc-provide] function on the @racket[doc] and @racket[metas] from the source file,
(pretty much just as we did manually in the previous section).

If you start the Pollen project server with @exec{raco pollen start} and preview
@filepath{fleas.html} you’ll see the Beeswax template also being used to render the live preview.

@margin-note{Note that modifying a Beeswax template does not invalidate Pollen’s render cache. We
can tell Pollen to use Beeswax for rendering but it still does not know anything about our
alternative template files, so it isn’t watching them for changes when determining when to force a
re-render.}
