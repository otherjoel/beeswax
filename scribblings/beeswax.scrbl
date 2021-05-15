#lang scribble/manual

@title[#:style '(toc)]{Beeswax: a Pollen-friendly template language}
@author[(author+email "Joel Dueck" "joel@jdueck.net")]

@defmodule[beeswax]

Beeswax is a simple templating language. A Beeswax template wraps its contents in a function
that can be imported by other modules and applied to data to get a rendered document.
Having a @hash-lang[] for your templates has two minor benefits: it lights up all the little
conveniences of your IDE if you use one (DrRacket, racket-mode, etc.) and it can make
renders faster (because the template can be compiled).

Beeswax was designed to be easy to use within
@seclink["top" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{Pollen} projects, but can
be of use in other Racket projects as well.

@bold{Warning:} Beeswax isn’t done yet, and will probably change.

@local-table-of-contents[]

@include-section["tutorial.scrbl"]
@; Installation`
@; Module reference: beeswax/template, beeswax/render, beeswax/for-pollen
@include-section["raco.scrbl"]
@; Using outside of Pollen: beeswax/template/base

