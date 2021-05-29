#lang scribble/manual

@(require (for-label racket/base
                     beeswax/for-pollen
                     beeswax/template
                     beeswax/render
                     pollen/core
                     pollen/template
                     pollen/pagetree
                     pollen/setup
                     (prefix-in pollen: pollen/render)
                     racket/contract
                     racket/math)
          beeswax/private/constants)

@(define tproc @racket[#,template-proc-provide])

@title{Beeswax Reference}

@section{Templates}

@defmodulelang[beeswax/template]

@subsection{Syntax and bindings}

Beeswax templates use @secref["pollen-command-syntax" #:doc '(lib
"pollen/scribblings/pollen.scrbl")], which treats everything after the @hash-lang[] line as string
content that will be included in the output, except for embedded Racket expressions, which are
escaped with the command character @tt{◊}. Beeswax honors @racket[setup:command-char], so if you
have a custom command character configured in your @filepath{pollen.rkt}, any templates in that
project will use it as well. (See @secref["setup-overrides" #:doc '(lib
"pollen/scribblings/pollen.scrbl")] in the Pollen docs.)

Within a template, you have access to all the same things you do in a normal Pollen template:

@itemlist[
          
 @item{The @racketid[doc] and @racketid[metas] of the current source file}
  
 @item{The @racketid[here] variable, which is the path of the current output file in
  @racketlink[pagenode?]{pagenode} form}
 
 @item{Everything in the @racketmodname[pollen/core], @racketmodname[pollen/template] and @racketmodname[pollen/pagetree]
modules}
 
 @item{Everything @racket[provide]d by a @filepath{pollen.rkt} located in the same or any parent folder.}
 
 ]

@subsection{The @tproc function}

Beeswax templates wrap their contents inside a function named @tproc that is @racket[provide]d to
other files. Any @racket[require]-like forms within the template are lifted to the top level. Any
@racket[define]-like forms are moved to the start of the function --- so you should be careful that
definitions within the template don’t depend on side-effects from other expressions.

@defproc[(apply-template [doc any/c] [metas hash?] [here pagenode?]) bytes?]{

This is an implicit function provided by every @code{#lang beeswax/template} file. The function
definition surrounds the contents of the template; calling the function renders a document, using
@racket[_doc], @racket[_metas] and @racket[_here] wherever they are referred to in escaped Racket
expressions within the template.

Returns the bytes of the rendered result, stripped of any leading whitespace.

}

@subsection{Template filename conventions}

Your template’s filename should look like @tt{template.@italic{ext}.rkt}, where @italic{ext} is the
extension for the template’s output format. So for example, a template for HTML files should be
named @filepath{template.html.rkt}.

Your template will be useable (e.g., via @racket[dynamic-require]) no matter what you name it. But
if you are using Pollen with @racket[external-renderer], Beeswax’s @racket[render] function, or
@secref["raco-beeswax"] to apply your templates, you’ll want to name it like this, because those
tools will be unable to locate your template otherwise.

See also the documentation for @racket[render] to see how Beeswax determines which template to use.

@margin-note{This is a part of Beeswax about which I’m still undecided. Alternately, Beeswax could
specify that its template filenames should be identical to those of normal Pollen templates. This
would have the advantage of playing nicely with Pollen’s render cache, and it would mean slightly
fewer redundant cycles used to locate the correct template when doing a render within the context of
@exec{raco pollen render} (since Pollen always does the work of locating its own template even when
using an external renderer).}

@section{Rendering}

@defmodule[beeswax/render]

This module provides convenience functions.

@defproc[(get-template-proc [template (or/c path? path-string?)]) procedure?]{

Returns a binding for the @tproc procedure provided from @racket[_template]. If
@racket[_template-path] cannot be resolved to a module path, or if the template does not
@racket[provide] a binding named @racketid[#,template-proc-provide], or if that binding is not a
procedure, an exception is raised. The signature of the procedure is not checked.

}

@defproc[(render [source (or/c path? path-string?)]
                 [output (or/c path? path-string?)])
         (or/c bytes? any/c)]{

Locates the correct Beeswax template for the source and target output format, and returns the result
of calling that template’s @tproc procedure with the @racketid[doc] and @racketid[metas] provided by
the source document, and with @racket[_output] (in symbol form) as the third parameter.

The @racket[_source] file is assumed to be a valid Pollen source: its @racketid[doc] and
@racketid[metas] will be retrieved with @racket[get-doc] and @racket[get-metas].

If the template uses @code{#lang beeswax/template}, then the result will be @racket[bytes?],
otherwise it could be anything.

This function checks the following places in order to find the correct template:

@itemlist[#:style 'ordered

 @item{If the @racketid[metas] for @racket[_source] have a key for @racket['template], then Beeswax
will attempt to use the value of that key (regardless of whether the file specified in that key
actually exists). If the value is a list, it will select the value from the list matching the
current output target based on its penultimate extension.}

@item{The filesystem is searched for @filepath{template.@italic{ext}.rkt} starting with the same
folder in which @racket[_source] is located and moving up through its parent folders one at a time.}
                                                                                   
]

If neither of these methods yields a potential template, an exception is raised. (There are no
“default templates” in Beeswax as there are in Pollen.)

}

@section{For Pollen}

@defmodule[beeswax/for-pollen]

@defproc[(external-renderer [source path?] [template path?] [output path?]) (or/c string? bytes?)]{

Renders @racket[_source] into @racket[_output] and returns the string or bytes of the result. If
@racket[_source] has a @filepath{.pm} (Pollen markup) or @filepath{.pmd} (Pollen Markdown) file
extension, the render is done using Beeswax’s @racket[render] function, otherwise the parameters are
passed through to @racketlink[pollen:render]{Pollen’s @tt{render} function}.

This function is designed to be used by Pollen itself as an external renderer; you’ll probably never
use it directly. To designate Beeswax as the external renderer for a Pollen project, define and
provide a value called @racketid[external-renderer] in the @racketid[setup] module of your
@filepath{pollen.rkt} like so:

@filebox["pollen.rkt" @codeblock{
#lang racket/base

(module setup racket/base
  (define external-renderer '(beeswax/for-pollen external-renderer))
  (provide external-renderer))
}]

}
