%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Pattern-Matching Details}
\label{ch:patterns}

\index{pattern!matching||(} 
In this chapter we will look at Haskell's pattern-matching process in
greater detail.

Haskell defines a fixed set of patterns for use in case expressions
and function definitions.  Pattern matching is permitted using the
constructors of any type, whether user-defined or pre-defined in
Haskell.  This includes tuples, strings, numbers, characters, etc.
For example, here's a contrived function that matches against a tuple
of ``constants:''
\begin{spec}
contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
contrived ([],  'b',  (1,   2.0),   "hi",   True) = False
\end{spec}
This example also demonstrates that {\em nesting} of patterns is
permitted (to arbitrary depth).

Technically speaking, {\em formal parameters} to functions are also
patterns---it's just that they {\em never fail to match a value}.  As
a ``side effect'' of a successful match, the formal parameter is bound
to the value it is being matched against.  For this reason patterns in
any one equation are not allowed to have more than one occurrence of
the same formal parameter.
\index{pattern!refutable}
\index{pattern!irrefutable}

A pattern that may fail to match is said to be {\em refutable}; for
example, the empty list \hs{[]} is refutable.  Patterns such as formal
parameters that never fail to match are said to be {\em irrefutable}.
There are three other kinds of irrefutable patterns, which are
summarized below.

\index{pattern!as-pattern}
\paragraph*{As-Patterns} Sometimes it is convenient to name a
pattern for use on the right-hand side of an equation.  For example, a
function that duplicates the first element in a list might be written
as: 
\begin{spec}
f (x:xs)                = x:x:xs
\end{spec}
Note that \hs{x:xs} appears both as a pattern on the left-hand side,
and as an expression on the right-hand side.  To improve readability,
we might prefer to write \hs{x:xs} just once, which we can achieve
using an {\em as-pattern} as follows:\footnote{Another advantage to
doing this is that a naive implementation might otherwise completely
reconstruct \hs{x:xs} rather than re-use the value being matched
against.}
\begin{spec}
f s@(x:xs) = x:s
\end{spec}
Technically speaking, as-patterns always result in a successful match,
although the sub-pattern (in this case \hs{x:xs}) could, of course,
fail.

\index{pattern!wildcard}
\paragraph*{Wildcards} Another common situation is matching against
a value we really care nothing about.  For example, the functions
\hs{head} and \hs{tail} can be written as:
\begin{spec}
head (x:_)             = x
tail (_:xs)            = xs
\end{spec}
in which we have ``advertised'' the fact that we don't care what a
certain part of the input is.  Each wildcard will independently match
anything, but in contrast to a formal parameter, each will bind
nothing; for this reason more than one are allowed in an equation.

\index{pattern!lazy}
\paragraph*{Lazy Patterns}
There is one other kind of pattern allowed in Haskell.  It is called a
{\em lazy pattern}, and has the form \hs{~pat}.  Lazy patterns are
{\em irrefutable}: matching a value $v$ against \hs{~pat} always
succeeds, regardless of \hs{pat}.  Operationally speaking, if an
identifier in \hs{pat} is later ``used'' on the right-hand-side, it
will be bound to that portion of the value that would result if \hs{v}
were to successfully match \hs{pat}, and $\bot$ otherwise.

Lazy patterns are useful in contexts where infinite data structures
are being defined recursively.  For example, infinite lists are an
excellent vehicle for writing {\em simulation} programs, and in this
context the infinite lists are often called {\em streams}.  
%% Streams were discussed at length in Chapter \ref{ch:streams}.

\section*{Pattern-Matching Semantics}

So far we have discussed how individual patterns are matched, how some
are refutable, some are irrefutable, etc.  But what drives the overall
process?  In what order are the matches attempted?  What if none
succeed?  This section addresses these questions.

Pattern matching can either {\em fail}, {\em succeed} or {\em
diverge}.  A successful match binds the formal parameters in the
pattern.  Divergence occurs when a value needed by the pattern
diverges (i.e.\ is non-terminating) or results in an error ($\bot$).
The matching process itself occurs ``top-down, left-to-right.''
Failure of a pattern anywhere in one equation results in failure of
the whole equation, and the next equation is then tried.  If all
equations fail, the value of the function application is $\bot$, and
results in a run-time error.

For example, if \hs{bot} is a divergent or erroneous computation, and
if \hs{[1,2]} is matched against \hs{[0,bot]}, then \hs{1} fails to
match \hs{0}, so the result is a failed match.  But if \hs{[1,2]} is
matched against \hs{[bot,0]}, then matching \hs{1} against \hs{bot}
causes divergence (i.e.~$\bot$).

\index{pattern!guard}

The only other twist to this set of rules is that top-level patterns
may also have a boolean {\em guard}, as in this definition of a
function that forms an abstract version of a number's sign:
\begin{spec}
sign x |  x >  0        =   1
       |  x == 0        =   0
       |  x <  0        =  -1
\end{spec}
Note here that a sequence of guards is given for a single pattern; as
with patterns, these guards are evaluated top-down, and the first that
evaluates to \hs{True} results in a successful match.

\paragraph*{An Example}

The pattern-matching rules can have subtle effects on the meaning of
functions.  For example, consider this definition of \hs{take}:
\begin{spec}
take  0     _           =  []
take  _     []          =  []
take  n     (x:xs)      =  x : take (n-1) xs
\end{spec}
and this slightly different version (the first 2 equations have been
reversed):
\begin{spec}
take1  _     []         =  []
take1  0     _          =  []
take1  n    (x:xs)      =  x : take1 (n-1) xs
\end{spec}
Now note the following:
\[\begin{array}{lcl}
  \hs{take  0 bot}  &\ \ \ \red\ \ \ & \hs{[]} \\
  \hs{take1 0 bot}  &\ \ \ \red\ \ \ & \bot \\[.1in]
  \hs{take  bot []} &\ \ \ \red\ \ \ & \bot \\
  \hs{take1 bot []} &\ \ \ \red\ \ \ & \hs{[]}
\end{array}\]
We see that \hs{take} is ``more defined'' with respect to its second
argument, whereas \hs{take1} is more defined with respect to its first.
It is difficult to say in this case which definition is better.  Just
remember that in certain applications, it may make a difference.  (The
Standard Prelude includes a definition corresponding to \hs{take}.)

\section*{Case Expressions}
\indexkw{case}

Pattern matching provides a way to ``dispatch control'' based on
structural properties of a value.  However, in many circumstances we
don't wish to define a {\em function} every time we need to do this.
Haskell's {\em case expression} provides a way to solve this problem.
Indeed, the meaning of pattern matching in function definitions is
specified in the Haskell Report in terms of case expressions, which
are considered more primitive.  In particular, a function definition
of the form:
\[\begin{array}{l}
\hs{f} p_{11} ... p_{1k} \hs{=} e_{1} \\
... \\
\hs{f} p_{n1} ... p_{nk} \hs{=} e_{n}
\end{array}\]
where each $p_{ij}$ is a pattern, is semantically equivalent to:
\[ \hs{f x1 x2 ... xk = case (x1, ..., xk) of}
   \begin{array}[t]{l}
   (p_{11}, ..., p_{1k}) \rightarrow e_{1} \\
   ... \\
   (p_{n1}, ..., p_{nk}) \rightarrow e_{n}
   \end{array}
\]
where the \hs{xi} are new identifiers.  For example, the
definition of \hs{take} given earlier is equivalent to:
\begin{spec}
take m ys = case (m,ys) of
              (0,_)       ->  []
              (_,[])      ->  []
              (n,x:xs)    ->  x : take (n-1) xs
\end{spec}
For type correctness, the types of the right-hand sides of a case
expression or set of equations comprising a function definition must
all be the same; more precisely, they must all share a common
principal type.

The pattern-matching rules for case expressions are the same as we
have given for function definitions.
\index{pattern!matching||)}
