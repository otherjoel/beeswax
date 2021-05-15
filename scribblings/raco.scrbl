#lang scribble/manual

@(require (for-label racket/base pollen/setup))
@(require "sandbox.rkt")

@title{@exec{raco beeswax render}}

This command renders a Pollen source (or any file that provides Pollen’s
@seclink["Standard_exports" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{standard identifiers}
@racket[_doc] and @racket[_metas]) using the appropriate Beeswax template.

Use @exec{raco beeswax render} in preference to @exec{raco pollen render} when you want speed over
convenience. This command is stupid but fast.

@terminal{
@:>{raco beeswax render @italic{source} ...}
}

As with Pollen:

@itemlist[
 @item{The optional @exec{-t} or @exec{--target} switch specifies the target
file type to use for multi-output source files and ambiguously-named files. (Files that specify a specific output target
will still be rendered as usual.) Like Pollen, this defaults to whatever appears first in @racket[(setup:poly-targets)].
}

 @item{This command makes use of
  @seclink["Cache" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{Pollen’s @emph{compile} cache}; if the source file
has not been compiled, it will be compiled and cached; if it has already been compiled, the cached copy is used.}
 ]

Unlike Pollen:

@itemlist[
          
@item{You can only specify @emph{source files} --- not directories and not output
files. If you want to render a bunch of files, list them out separately or use wildcards.
 In any case, each output filename is derived from the source filename, incorporating the @exec{--target} value
 to get the extension when necessary.
 }

@item{You cannot specify
@secref["Preprocessor___pp_extension_" #:doc '(lib "pollen/scribblings/pollen.scrbl")] files or
@seclink["top" #:doc'(lib "scribblings/scribble/scribble.scrbl")]{Scribble files} with this command.}

@item{There is not (yet) a parallel rendering option. All the files are rendered in sequence using a single core.
On my personal (not very new) laptop, this method can still produce 75 HTML files in 3–4 seconds. This
is fast enough that I haven’t bothered with the additional complexity of offering full use of all available
cores.}

@item{This command does not use Pollen’s (or any) @emph{render} caching; it will
always write the output file to disk, even if none of its dependencies have changed. If the output file exists,
it will always be silently overwritten.}

]

@section{Optimizing renders}

@subsection{Use @exec{raco make} on your templates}

You will get a noticeable speed increase by precompiling your Beeswax template with
@seclink["make" #:doc '(lib "scribblings/raco/raco.scrbl")]{@exec{raco make}}:

@terminal{
@:>{raco make template.html.rkt}
}

This will speed things up no matter what method you use to apply your Beeswax templates --- whether
that is @exec{raco beeswax render}, Pollen, or some custom scheme of your own.

Perhaps Beeswax will do this for you at some point, but for now, it doesn’t. You
can either do it manually, or automate it along with the rest of your build by using a
@hyperlink["https://www.gnu.org/software/make/manual/html_node/Simple-Makefile.html"]{makefile}.

@subsection{Example benchmarks}

Here we compare three render times for the same source document: one using normal Pollen template, one
using @exec{raco beeswax render}, and another using @exec{raco beeswax render} with a precompiled
template. The source document and Pollen and Beeswax templates used are the same ones shown in the
tutorial.

@margin-note{The commands shown are actually run each time this documentation is built, so the time
measurements reflect the performance of whatever environment was used to built this copy of this document.
If you’re viewing this document from a local Racket installation, these are @emph{your} computer’s numbers!}

@ensure-pollen-rkt['absent]
@; don’t use the actual output of this one since it will include a bunch of long messy paths:
@(void @sandbox-raco["pollen" '#("reset")]) 

@terminal{
@rem{Reset the compile cache}
@:>{raco pollen reset}
pollen: resetting cache ...

@rem{"Preheat the cache"}
@:>{raco pollen setup}
@sandbox-raco["pollen" '#("setup")]

@rem{Pollen render (without the Beeswax external-renderer)}
@:>{raco pollen render fleas.html}
@sandbox-raco["pollen" '#("render" "-f" "fleas.html")]

@rem{Beeswax render}
@:>{raco beeswax render fleas.html.pm}
@sandbox-raco["beeswax" '#("render" "fleas.html.pm")]

@rem{Make Beeswax go even faster by precompiling the template}
@:>{raco make template.html.rkt}
@sandbox-raco["make" '#("template.html.rkt")]
@:>{raco beeswax render fleas.html.pm}
@sandbox-raco["beeswax" '#("render" "fleas.html.pm")]
}
