#lang scribble/manual

@require["sandbox.rkt"]

@title[#:style '(toc)]{Beeswax: a Pollen-friendly template language}
@author[(author+email "Joel Dueck" "joel@jdueck.net")]

@defmodule[beeswax]

Beeswax is a simple templating language. A Beeswax template wraps its contents in a function that
can be imported by other modules and applied to data to get a rendered document. Having a
@hash-lang[] for your templates has a few small benefits: it lights up all the little conveniences
of your IDE, if you use one (DrRacket, racket-mode, etc.); it’s a little easier to reason about and
reuse in other contexts; and it can make renders faster (because the template can be compiled).

Beeswax was designed to be easy to use within @seclink["top" #:doc '(lib
"pollen/scribblings/pollen.scrbl")]{Pollen} projects, but can be of use in other Racket projects as
well.

@bold{To install Beeswax} from the command line:

@terminal{
@:>{raco pkg install beeswax}}

Or using DrRacket: click the @onscreen{File} menu → @onscreen{Install Package …}.

@margin-note{@bold{Warning:} Beeswax isn’t done yet, and will probably change.}

@local-table-of-contents[]

@include-section["tutorial.scrbl"]
@include-section["reference.scrbl"]
@include-section["raco.scrbl"]
@include-section["ack.scrbl"]
