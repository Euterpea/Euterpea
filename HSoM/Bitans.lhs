%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Built-in Types Are Not Special}
\label{ch:bitans}

\index{type}
Throughout this text we have introduced many ``built-in'' types such
as lists, tuples, integers, and characters.  We have also shown how
new user-defined types can be defined.  Aside from special syntax, you
might be wondering if the built-in types are in any way more special
than the user-defined ones.  The answer is {\em no}.  The special
syntax is for convenience and for consistency with historical
convention, but has no semantic consequence.

We can emphasize this point by considering what the type
declarations would look like for these built-in types if in fact we
were allowed to use the special syntax in defining them.  For example,
the \indexwdhs{Char} type might be written as:
\begin{spec}
data Char       = 'a' | 'b' | 'c' | ...         -- This is not valid
                | 'A' | 'B' | 'C' | ...         -- Haskell code!
                | '1' | '2' | '3' | ...
\end{spec}
These constructor names are not syntactically valid; to fix them we
would have to write something like:
\begin{spec}
data Char       = Ca | Cb | Cc | ...
                | CA | CB | CC | ...
                | C1 | C2 | C3 | ...
\end{spec}
Even though these constructors are actually more concise, they are
quite unconventional for representing characters, and thus the special
syntax is used instead.

In any case, writing ``pseudo-Haskell'' code in this way helps us to
see through the special syntax.  We see now that \hs{Char} is just a
data type consisting of a large number of nullary (meaning they take
no arguments) constructors.  Thinking of \hs{Char} in this way makes
it clear why, for example, we can pattern-match against characters;
i.e., we would expect to be able to do so for any of a data type's
constructors.

Similarly, using pseudo-Haskell, we could define \indexwdhs{Int} and
\indexwdhs{Integer} by:
\begin{spec}
               -- more pseudo-code:
data Int     = (-2^29) | ... | -1 | 0 | 1 | ... | (2^29-1)
data Integer =       ... -2 | -1 | 0 | 1 | 2 ...
\end{spec}
(Recall that $-2^{29}$ to $2^{29-1}$ is the minimum range for the
\hs{Int} data type.)  \hs{Int} is clearly a much larger enumeration
than \hs{Char}, but it's still finite!  In contrast, the pseudo-code
for \hs{Integer} (the type of arbitrary precision integers) is
intended to convey an {\em infinite} enumeration (and in that sense
only, the \hs{Integer} data type {\em is} somewhat special).

\index{unit type}
Haskell has a data type called \hs{unit} which has exactly one value:
\hs{()}.  The name of this data type is also written \hs{()}.  This is
trivially expressed in Haskell pseudo-code:
\begin{spec}
data () = ()		-- more pseudo-code
\end{spec}
Tuples are also easy to define playing this game:
\index{tuples}
\begin{spec}
data (a,b)         = (a,b)         -- more pseudo-code
data (a,b,c)       = (a,b,c)
data (a,b,c,d)     = (a,b,c,d)
\end{spec}
and so on.  Each declaration above defines a tuple type of a
particular length, with parentheses playing a role in both the
expression syntax (as data constructor) and type-expression syntax (as
type constructor).  By ``and so on'' we mean that there are an
infinite number of such declarations, reflecting the fact that tuples
of all finite lengths are allowed in Haskell.

The list data type is also easily handled in pseudo-Haskell, and more
interestingly, it is recursive:
\begin{spec}
data [a]          = [] | a : [a]   -- more pseudo-code
infixr 5 :
\end{spec}
We can now see clearly what we described about lists earlier: \hs{[]}
is the empty list, and \hs{(:)} is the infix list constructor; thus
\hs{[1,2,3]} must be equivalent to the list \hs{1:2:3:[]}.  (Note that
\hs{(:)} is right associative.)  The type of \hs{[]} is \hs{[a]}, and
the type of \hs{(:)} is \hs{a->[a]->[a]}.

\syn{The way \hs{(:)} is defined here is actually legal syntax---\indexwd{infix
constructors} are permitted in \hs{data} declarations, and are
distinguished from infix operators (for pattern-matching purposes) by
the fact that they must begin with a colon (a property trivially
satisfied by ``\hs{:}'').}

At this point the reader should note carefully the differences between
tuples and lists, which the above definitions make abundantly clear.
In particular, note the recursive nature of the list type whose
elements are homogeneous and of arbitrary length, and the
non-recursive nature of a (particular) tuple type whose elements are
heterogeneous and of fixed length.  The typing rules for tuples and
lists should now also be clear:

For \hs{(e1,e2,...,en)}, $n\geq2$, if \hs{Ti} is the type of \hs{ei},
then the type of the tuple is \hs{(T1,T2,...,Tn)}.

For \hs{[e1,e2,...,en]},$n\geq0$, each \hs{ei} must have the same type
$T$, and the type of the list is \hs{[T]}.

