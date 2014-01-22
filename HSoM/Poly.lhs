%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter[Polymorphic \& Higher-Order Functions]
{Polymorphic and Higher-Order Functions}
\label{ch:poly}

Several examples of polymorphic data types were introduced in the last
couple of chapters.  In this chapter the focus is on {\em polymorphic
  functions}, which are most commonly defined over polymorphic data
types.

The already familiar {\em list} is the protoypical example of a
polymorphic data type, and it will be studied in depth in this
chapter.  Although lists have no direct musical connection, they are
perhaps the most commonly used data type in Haskell, and have many
applications in computer music programming.  But in addition the
|Music| data type is polymorphic, and several new functions that
operate on it polymorphiccally will also be defined,

(A more detailed discussion of predefined polymorphic functions that
operate on lists can be found in Appendix \ref{ch:list-tour}.)

This chapter also introduces {\em higher-order functions}, which are
functions that take one or more functions as arguments or return a
function as a result (functions can also be placed in data
structures).  Higher-order functions permit the elegant and concise
expression of many musical concepts.  Together with polymorphism,
higher-order functions substantially increase the programmer's
expressive power and ability to reuse code.

Both of these new ideas follow naturally the foundations that have
already been established.

\section{Polymorphic Types}
\label{sec:poly-types}

\index{polymorphism||(} \index{type!polymorphic||(} 

In previous chapters, examples of lists containing several different
kinds of elements---integers, characters, pitch classes, and so
on---were introduced, and we can well imagine situations requiring
lists of other element types.  Sometimes, however, it is not necessary
to be so particular about the type of the elements.  For example,
suppose we wish to define a function |length| that determines the
number of elements in a list.  It does not really matter whether the
list contains integers, pitch classes, or even other lists---we can
imagine computing the length in exactly the same way in each case.
The obvious definition is: \indexhs{length}
\begin{spec}
length []      = 0
length (x:xs)  = 1 + length xs
\end{spec}
This recursive definition is self-explanatory.  Indeed, we can read
the equations as saying: ``The length of the empty list is 0, and the
length of a list whose first element is |x| and remainder is |xs| is 1
plus the length of |xs|.''

But what should the type of |length| be?  Intuitively, we would like
to say that, for {\em any} type |a|, the type of |length| is |[a] ->
Integer|.  In mathematics we might write this as:
\begin{spec}
length :: (forall a) [a] -> Integer
\end{spec}
But in Haskell this is written simply as:
\begin{spec}
length :: [a] -> Integer
\end{spec}
In other words, the universal quantification of the type variable |a|
is implicit.
\index{type!variable}
\syn{Generic names for types, such as |a| above, are called {\em
type variables}, and are uncapitalized to distinguish them from
concrete types such as |Integer|.}

So |length| can be applied to a list containing elements of {\em
any} type.  For example:
\begin{code}
length [1,2,3]            ===> 3
length [C,D,Ef ]         ===> 3
length [[1],[],[2,3,4]]   ===> 3
\end{code}
%% length "def"              ===> 3

Note that the type of the argument to |length| in the last example
is |[[Integer]]|; that is, a list of lists of integers.

Here are two other examples of polymorphic list functions, which
happen to be predefined in Haskell:
\indexhs{head}
\indexhs{tail}
\begin{spec}
head         :: [a] -> a
head (x:_)   =  x

tail         :: [a] -> [a]
tail (_:xs)  =  xs
\end{spec}
\syn{The |_| on the left-hand side of these equations is called a
  \emph{wildcard} pattern.  It matches any value, and binds no
  variables.  It is useful as a way of documenting the fact that we
  do not care about the value in that part of the pattern.  Note
  that we could (perhaps should) have used a wildcard in place of the
  variable |x| in the definition of |length|.  } 

These two functions take the ``head'' and ``tail,'' respectively, of
any non-empty list.  For example:
\begin{spec}
head [ 1, 2, 3 ]    ==> 1
head [ C, D, Ef ]  ==> C
tail [ 1, 2, 3 ]    ==> [ 2, 3 ]
tail [ C, D, Ef ]  ==> [ D, Ef ]
\end{spec}
Note that, for any non-empty list |xs|, |head| and |tail| obey the
following law:
\begin{spec}
head xs : tail xs = xs
\end{spec}

Functions such as |length|, |head|, and |tail| are said to be
{\em polymorphic}.  Polymorphic functions arise naturally when
defining functions on lists and other polymorphic data types,
including the |Music| data type defined in the last chapter.

%% In the remainder of this chapter we will continue studying polymorphic
%% lists, but in Chapter \ref{ch:trees}, for example, we will look at
%% another polymorphic data structure, namely a {\em tree}.

\index{type!polymorphic||)}
\index{polymorphism||)}

\section{Abstraction Over Recursive Definitions}
\label{sec:rec-abstraction}

Given a list of pitches, suppose we wish to convert each pitch into
an absolute pitch.  We could define a function:
\begin{code}
toAbsPitches         :: [Pitch] -> [AbsPitch]
toAbsPitches []      = []
toAbsPitches (p:ps)  = absPitch p : toAbsPitches ps
\end{code}

We might also want to convert a list of absolute pitches to a list of
pitches:
\begin{code}
toPitches         :: [AbsPitch] -> [Pitch]
toPitches []      = []
toPitches (a:as)  = pitch a : toPitches as
\end{code}

These two functions are different, but share something in common:
there is a repeating pattern of operations.  But the pattern is not
quite like any of the examples studied earlier, and therefore it is
unclear how to apply the abstraction principle.  What distinguishes
this situation is that there is a repeating pattern of {\em
  recursion}.

In discerning the nature of a repeating pattern, recall that it is
sometimes helpful to first identify those things that {\em are not}
repeating---i.e.\ those things that are {\em changing}---since these
will be the sources of {\em parameterization}: those values that must
be passed as arguments to the abstracted function.  In the case above,
these changing values are the functions |absPitch| and |pitch|;
consider them instances of a new name, |f|.  Rewriting either of the
above functions as a new function---call it |map|---that takes an
extra argument |f|, yields:
\begin{spec}
map f []      = []
map f (x:xs)  = f x : map f xs
\end{spec}
This recursive pattern of operations is so common that |map| is
predefined in Haskell (and is why the name |map| was chosen in the
first place).

With |map|, we can now redefine |toAbsPitches| and |toPitches| as:
\indexhs{map}
\begin{spec}
toAbsPitches     :: [Pitch] -> [AbsPitch]
toAbsPitches ps  = map absPitch ps

toPitches     :: [AbsPitch] -> [Pitch]
toPitches as  = map pitch as
\end{spec}
Note that these definitions are non-recursive; the common pattern of
recursion has been abstracted away and isolated in the definition of
|map|.  They are also very succinct; so much so, that it seems
unnecessary to create new names for these functions at all!  One of
the powers of higher-order functions is that they permit concise yet
easy-to-understand definitions such as this, and you will see many
similar examples throughout the remainder of the text.

A proof that the new versions of these two functions are equivalent to
the old ones can be done via calculation, but requires a proof
technique called {\em induction}, because of the recursive nature of
the original function definitions.  Inductive proofs are discussed in
detail, including for these two examples, in Chapter
\ref{ch:induction}.

\subsection{Map is Polymorphic}

What should the type of |map| be?  Looking first at its use in
|toAbsPitches|, note that it takes the function |absPitch :: Pitch ->
AbsPitch| as its first argument and a list of |Pitch|s as its second
argument, returning a list of |AbsPitch|s as its result.  So its type
must be:
\begin{spec}
map :: (Pitch -> AbsPitch) -> [Pitch] -> [AbsPitch]
\end{spec}
Yet a similar analysis of its use in |toPitches| reveals that
|map|'s type should be:
\begin{spec}
map :: (AbsPitch -> Pitch) -> [AbsPitch] -> [Pitch]
\end{spec}
This apparent anomaly can be resolved by noting that |map|, like
|length|, |head|, and |tail|, does not really care what its
list element types are, {\em as long as its functional argument can be
applied to them}.  Indeed, |map| is {\em polymorphic}, and its most
general type is:
\begin{spec}
map :: (a -> b) -> [a] -> [b]
\end{spec}
This can be read: ``|map| is a function that takes a function from
any type |a| to any type |b|, and a list of |a|'s, and
returns a list of |b|'s.''  The correspondence between the two
|a|'s and between the two |b|'s is important: a function that
converts |Int|'s to |Char|'s, for example, cannot be mapped over
a list of |Char|'s.  It is easy to see that in the case of
|toAbsPitches|, |a| is instantiated as |Pitch| and |b| as
|AbsPitch|, whereas in |toPitches|, |a| and |b| are
instantiated as |AbsPitch| and |Pitch|, respectively.

Note, as we did in Section \ref{sec:music}, that the above reasoning
can be viewed as the abstraction principle at work at the type level.

\syn{In Chapter \ref{ch:intro} it was mentioned that every expression
  in Haskell has an associated type.  But with polymorphism, we might
  wonder if there is just one type for every expression.  For example,
  |map| could have any of these types:
\begin{spec}
(a->b) -> [a] -> [b]
(Integer->b) -> [Integer] -> [b]
(a->Float) -> [a] -> [Float]
(Char->Char) -> [Char] -> [Char]
\end{spec}
and so on, depending on how it will be used.  However, notice that the
first of these types is in some fundamental sense more general than
the other three.  In fact, every expression in Haskell has a unique
type known as its \index{type!principal}{\em principal type}: the
least general type that captures all valid uses of the expression.
The first type above is the principal type of |map|, since it captures
all valid uses of |map|, yet is less general than, for example, the
type |a->b->c|.  As another example, the principal type of |head| is
|[a]->a|; the types |[b]->a|, |b->a|, or even |a| are too general,
whereas something like |[Integer]->Integer| is too specific.  (The
existence of unique principal types is the hallmark feature of the
{\em Hindley-Milner type system} \cite{hindley69,milner78} that forms
the basis of the type systems of Haskell, ML \cite{ML-definition} and
several other functional languages \cite{huda89a}.)}
\index{Hindley-Milner type system}

\subsection{Using map}

%% Now that we can picture |map| as a polymorphic function, it is
%% useful to look back on some of the examples we have worked through to
%% see if there are any situations where |map| might have been useful.
%% For example, recall from Section \ref{sec:basic-list-abstraction} the
%% definition of |totalArea|:
%% \begin{spec}
%% totalArea = listSum [circleArea r1, circleArea r2, circleArea r3]
%% \end{spec}
%% It should be clear that this can be rewritten as:
%% \begin{spec}
%% totalArea = listSum (map circleArea [r1,r2,r3])
%% \end{spec}
%% A simple calculation is all that is needed to show that these are the
%% same:
%% \begin{spec}
%% map circleArea [r1, r2, r3]  
%% ==> circleArea r1 : map circleArea [r2, r3]
%% ==> circleArea r1 : circleArea r2 : map circleArea [r3]
%% ==> circleArea r1 : circleArea r2 : circleArea r3 : map circleArea []
%% ==> circleArea r1 : circleArea r2 : circleArea r3 : []
%% ==> [circleArea r1, circleArea r2, circleArea r3]
%% \end{spec}

For a musical example involving |map|, consider the task of generating
a six-note whole-tone scale starting at a given pitch:\footnote{A
  whole-tone scale is a sequence of six ascending notes, with each
  adjacent pair of notes separated by two semitones, i.e.\ a whole
  note.}
\begin{code}
wts    :: Pitch -> [Music Pitch]
wts p  =  let f ap = note qn (pitch (absPitch p + ap))
          in map f [0,2,4,6,8]
\end{code}
For example:
\begin{spec}
wts a440
===> [  note qn (A,4),   note qn (B,4),  note qn (C#,4), 
        note qn (D#,4),  note qn (F,4),  note qn (G,4) ]
\end{spec}

%% \syn{A list |[a,b..c]| is called an 
%% {\em \indexwd{arithmetic sequence}}, and is special syntax for the
%% list |[a, a+d, a+2*d, ..., c]| where |d = b-a|.}

\vspace{.1in}\hrule

\begin{exercise}{\em
Using |map|, define:
\begin{enumerate}
\item A function |f1 :: Int -> [Pitch] -> [Pitch]| that transposes
  each pitch in its second argument by the amount specified in its
  first argument.
\item A function |f2 :: [Dur] -> [Music a]| that turns a list
  of durations into a list of rests, each having the corresponding
  duration.
\item A function |f3 :: [Music Pitch] -> [Music Pitch]| that
  takes a list of music values (that are assumed to be single notes),
  and for each such note, halves its duration and places a rest of
  that same duration after it.  For example:
\begin{spec}
f3 [c 4 qn, d 4 en, e 4 hn]
===> [c 4 en :+: rest en, d 4 sn :+: rest sn, e 4 qn :+: rest qn]
\end{spec}
  You can think of this as giving a staccato interpretation of the
  notes.
\end{enumerate} }
\end{exercise} 

\vspace{.1in}\hrule

\section{Append}
\label{sec:append}

Consider now the problem of {\em concatenating} or {\em
  appending} two lists together; that is, creating a third list that
consists of all of the elements from the first list followed by all of
the elements of the second.  Once again the type of list elements does
not matter, so we can define this as a polymorphic infix operator
|(++)|: \indexhs{(++)}
\begin{spec}
(++) :: [a] -> [a] -> [a]
\end{spec}
For example, here are two uses of |(++)| on different types:
\begin{spec}
[1,2,3] ++ [4,5,6]   ===>  [1,2,3,4,5,6]
[C,E,G] ++ [D,F,A]   ===>  [C,E,G,D,F,A]
\end{spec}

As usual, we can approach this problem by considering the various
possibilities that could arise as input.  But in the case of |(++)|
there are {\em two} inputs---so which should be considered first?  In
general this is not an easy question to answer, so we could just try
the first list first: it could be empty, or non-empty.  If it is empty
the answer is easy:
\begin{spec}
[] ++ ys = ys
\end{spec}
and if it is not empty the answer is also straightforward:
\begin{spec}
(x:xs) ++ ys = x : (xs++ys)
\end{spec}
Note the recursive use of |(++)|.  The full definition is thus:
\begin{spec}
(++)           :: [a] -> [a] -> [a]
[]      ++ ys  = ys
(x:xs)  ++ ys  = x : (xs++ys)
\end{spec}

\syn{Note that an infix operator can be defined just as any other
  function, including pattern-matching, except that on the
  left-hand-side it is written using its infix syntax.

Also be aware that this textbook takes liberty in typesetting by
displaying the append operator as |++|.  When you type your code,
however, you will need to write {\tt ++}.  Recall that infix operators
in Haskell must not contain any numbers or letters of the alphabet,
and also must not begin with a colon (because those are reserved to be
infix constructors).}

If we were to consider instead the second list first, then the
first equation would still be easy:
\begin{spec}
xs ++ [] = xs
\end{spec}
but the second is not so obvious:
\begin{spec}
xs ++ (y:ys) = ??
\end{spec}
So it seems that the right choice was made to begin with.

Like |map|, the concatenation operator |(++)| is used so often that it
is predefined in Haskell.

\subsection{[Advanced] The Efficiency and Fixity of Append}

In Chapter \ref{ch:induction} the following simple property about
|(++)| will be proved:
\begin{spec}
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
\end{spec}
That is, |(++)| is {\em associative}.

\index{efficiency}
But what about the efficiency of the left-hand and right-hand sides of
this equation?  It is easy to see via calculation that appending two
lists together takes a number of steps proportional to the length of
the first list (indeed the second list is not evaluated at all).  For
example:
\begin{spec}
[1,2,3] ++ xs
==> 1 : ([2,3] ++ xs)
==> 1 : 2 : ([3] ++ xs)
==> 1 : 2 : 3 : ([] ++ xs)
==> 1 : 2 : 3 : xs
\end{spec}
Therefore the evaluation of |xs ++ (ys ++ zs)| takes a number of
steps proportional to the length of |xs| plus the length of
|ys|.  But what about |(xs ++ ys) ++ zs|?  The leftmost append
will take a number of steps proportional to the length of |xs|, but
then the rightmost append will require a number of steps proportional
to the length of |xs| plus the length of |ys|, for a total cost
of:
\begin{spec}
2 * length xs + length ys
\end{spec}
Thus |xs ++ (ys ++ zs)| is more efficient than |(xs ++ ys) ++ zs|.
This is why the Standard Prelude defines the fixity of |(++)| as:
\begin{spec}
infixr 5 !++
\end{spec}
In other words, if you just write |xs ++ ys ++ zs|, you will get the
most efficient association, namely the right association |xs ++ (ys ++
zs)|.  In the next section a more dramatic example of this property
will be introduced.

\section{Fold}
\label{sec:fold}
\indexhs{fold}

Suppose we wish to take a list of notes (each of type |Music a|) and
convert them into a \emph{line}, or \emph{melody}.  We can define a
recursive function to do this as follows:
\begin{spec}
line         :: [Music a] -> Music a
line []      = rest 0
line (m:ms)  = m :+: line ms
\end{spec}
Note that this function is polymorphic---the first example so far, in
fact, of a polymorphic function involving the |Music| data type.

We might also wish to have a function |chord| that operates in an
analogous way, but using |(:=:)| instead of |(:+:)|:
\begin{spec}
chord         :: [Music a] -> Music a
chord []      = rest 0
chord (m:ms)  = m :=: chord ms
\end{spec}
This function is also polymorphic.

In a completely different context we might wish to compute the highest
pitch in a list of pitches, which we might capture in the following
way:
\begin{code}
maxPitch         :: [Pitch] -> Pitch
maxPitch []      = pitch 0
maxPitch (p:ps)  = p !!! maxPitch ps
\end{code}
where |(!!!)| is defined as:
\begin{code}
p1 !!! p2 = if absPitch p1 > absPitch p2 then p1 else p2
\end{code}

\indexkw{if then else}
\index{conditional expression}
\syn{An expression |if pred then cons else alt| is called a {\em
conditional expression}.  If |pred| (called the {\em predicate}) is
true, then |cons| (called the {\em consequence}) is the result; if
|pred| is false, then |alt| (called the {\em alternative}) is
the result.}

Once again we have a situation where several definitions share
something in common: a repeating recursive pattern.  Using the process
used earlier to discover |map|, we first identify those things that
are changing.  There are two situations: the |rest 0| and |pitch 0|
values (for which the generic name |init|, for ``initial value,'' will
be used), and the |(:+:)|, |(:=:)|, and |(!!!)| operators (for which
the generic name |op|, for ``operator,'' will be used).  Now rewriting
any of the above three functions as a new function---call it
|fold|---that takes extra arguments |op| and |init|, we arrive
at:\footnote{The use of the name ``|fold|'' for this function is
  historical (within the functional programming community), and has
  nothing to do with the use of ``fold'' and ``unfold'' in
  Chapter~\ref{ch:intro} to describe steps in a calculation.}
\begin{spec}
fold op init []      = init
fold op init (x:xs)  = x `op` fold op init xs
\end{spec}
\syn{Any normal binary function name can be used as an infix operator
  by enclosing it in backquotes; |x `f` y| is equivalent to |f x y|.
  Using infix application here for |op| better reflects the
  structure of the repeating pattern that is being abstracted, but
  could also have been written |op x (fold op init xs)|.}

With this definition of |fold| we can now rewrite the definitions of
\indexwdhs{line}, \indexwdhs{chord}, and \indexwdhs{maxPitch} as:
\begin{code}
line, chord :: [Music a] -> Music a
line   ms = fold (:+:)  (rest 0) ms
chord  ms = fold (:=:)  (rest 0) ms
\end{code}

\begin{spec}
maxPitch     :: [Pitch] -> Pitch
maxPitch ps  = fold (!!!) (pitch 0) ps
\end{spec}

\syn{Just as we can turn a function into an operator by enclosing it
  in backquotes, we can turn an operator into a function by enclosing
  it in parentheses.  This is required in order to pass an operator as
  a value to another function, as in the examples above.  (If we wrote
  |fold !!! 0 ps| instead of |fold (!!!) 0 ps| it would look like we
  were trying to apply |(!!!)| to |fold| and |0 ps|, which is
  nonsensical and ill-typed.)}

In Chapter \ref{ch:induction} we will use induction to prove that
these new definitions are equivalent to the old.

|fold|, like |map|, is a highly useful---reusable---function, as will
be seen through several other examples later in the text.  Indeed, it
too is polymorphic, for note that it does not depend on the type of
the list elements.  Its most general type---somewhat trickier than
that for |map|---is:
\begin{spec}
fold :: (a->b->b) -> b -> [a] -> b
\end{spec}
This allows us to use |fold| whenever we need to ``collapse'' a
list of elements using a binary (i.e.\ two-argument) operator.

As a final example, recall the definition of |hList| from Chapter
\ref{ch:intro}:
\begin{spec}
hList           :: Dur -> [Pitch] -> Music Pitch
hList d []      = rest 0
hList d (p:ps)  = hNote d p :+: hList d ps
\end{spec}
A little thought should convince the reader that this can be rewritten as:
\begin{spec}
hList d ps =  let f p = hNote d p
              in line (map f ps)
\end{spec}
This version is more modular, in that it avoids explicit recursion,
and is instead built up from smaller building blocks, namely |line|
(which uses |fold|) and |map|.

\subsection{Haskell's Folds}

Haskell actually defines two versions of |fold| in the Standard
Prelude.  The first is called \indexwdhs{foldr}
(``fold-from-the-right'') whose definition is the same as that of
|fold| given earlier:
\begin{spec}
foldr                 :: (a->b->b) -> b -> [a] -> b
foldr op init []      = init
foldr op init (x:xs)  = x `op` foldr op init xs
\end{spec}
A good way to think about |foldr| is that it replaces all
occurrences of the list operator |(:)| with its first argument (a
function), and replaces |[]| with its second argument.  In other
words:
\begin{spec}
foldr op init (x1 : x2 : ... : xn : [])  
===> x1 `op` (x2 `op` (...(xn `op` init)...))
\end{spec}
This might help in better understanding the type of |foldr|, and also
explains its name: the list is ``folded from the right.''  Stated
another way, for any list |xs|, the following always
holds:\footnote{This will be formally proved in Chapter
  \ref{ch:induction}.}
\begin{spec}
foldr (:) [] xs  ===>  xs
\end{spec}
Haskell's second version of |fold| is called \indexwdhs{foldl}:
\begin{spec}
foldl                 :: (b->a->b) -> b -> [a] -> b
foldl op init []      = init
foldl op init (x:xs)  = foldl op (init `op` x) xs
\end{spec}
A good way to think about |foldl| is to imagine ``folding the list
from the left:''
\begin{spec}
foldl op init (x1 : x2 : ... : xn : [])
===> (...((init `op` x1) `op` x2)...) `op` xn
\end{spec}

\subsection{[Advanced] Why Two Folds?}

Note that if we had used |foldl| instead of |foldr| in the
definitions given earlier then not much would change; |foldr| and
|foldl| would give the same result.  Indeed, judging from their types, it
looks like the only difference between |foldr| and |foldl| is
that the operator takes its arguments in a different order.

So why does Haskell have two versions of |fold|?  It turns out that
there are situations where using one is more efficient, and possibly
``more defined'' (that is, the function terminates on more values of
its input domain) than the other.  \index{efficiency}

Probably the simplest example of this is a generalization of the
associativity of |(++)| discussed in the last section.  Suppose
we wish to collapse a list of lists into one list.  The Standard
Prelude defines the polymorphic function \indexwdhs{concat} for this
purpose:
\begin{spec}
concat      :: [[a]] -> [a]
concat xss  = foldr (++) [] xss
\end{spec}
For example:
\begin{spec}
concat [[1],[3,4],[],[5,6]]
===> [1]++([3,4]++([]++([5,6]++[])))
===> [1,3,4,5,6]
\end{spec}
More generally, we have that:
\begin{spec}
concat [xs1,xs2,...,xsn]
==>   foldr (++) [] [xs1,xs2,...,xsn]
===>  xs1 ++ (xs2 ++ ( ... (xn ++ [])) ... )
\end{spec}
The total cost of this computation is proportional to the sum of the
lengths of all of the lists.  If each list has the same length
|len|, and there are |n| lists, then this cost is |(n-1)*len|.

On the other hand, if we had defined |concat| this way:
\begin{spec}
slowConcat xss = foldl (++) [] xss
\end{spec}
then:
\begin{spec}
slowConcat [xs1,xs2,...,xsn]
==>   foldl (++) [] [xs1,xs2,...,xsn]
===>  ( ... (([] ++ x1) ++ x2) ... ) ++ xn
\end{spec}
If each list has the same length |len|, then the cost of this
computation will be:
\begin{spec}
len + (len+len) + (len+len+len) + ... + (n-1)*len
= n*(n-1)*len/2
\end{spec}
which is considerably worse than |(n-1)*len|.  Thus the choice of
|foldr| in the definition of |concat| is quite important.

Similar examples can be given to demonstrate that |foldl| is
sometimes more efficient than |foldr|.  On the other hand, in many
cases the choice does not matter at all (consider, for example,
|(+)|).  The moral of all this is that care must be taken in the
choice between |foldr| and |foldl| if efficiency is a concern.

% (consider, for example, |flip (++)|!)

\subsection{Fold for Non-empty Lists}

In certain contexts it may be understood that the functions |line| and
|chord| should not be applied to an empty list.  For such situations
the Standard Prelude provides functions |foldr1| and |foldl1|, which
return an error if applied to an empty list.  And thus we may also
desire to define versions of |line| and |chord| that adopt this
behavior:

\pagebreak

\begin{code}
line1, chord1  :: [Music a] -> Music a
line1  ms      = foldr1 (:+:)  ms
chord1 ms      = foldr1 (:=:)  ms
\end{code}
Note that |foldr1| and |foldl1| do not take an |init| argument.

In the case of |maxPitch| we could go a step further and say that the
previous definition is in fact flawed, for who is to say what the
maximum pitch of an empty list is?  The choice of 0 was indeed
arbitrary, and in a way it is nonsensical---how can 0 be the maximum
if it is not even in the list?  In such situations we might wish to
define only one function, and to have that function return an error
when presented with an empty list.  For consistency with |line| and
|chord|, however, that function is defined here with a new name:
\begin{code}
maxPitch1      :: [Pitch] -> Pitch
maxPitch1 ps   = foldr1 (!!!) ps
\end{code}

\section{[Advanced] A Final Example: Reverse}
\label{sec:reverse}

As a final example of a useful list function, consider the problem of
{\em reversing} a list, which will be captured in a function called
\indexwdhs{reverse}.  This could be useful, for example, when
constructing the \emph{retrograde} of a musical passage, i.e.\ the
music as if it were played backwards.  For example, |reverse
[C,D,Ef]| is |[Ef,D,C]|.

Thus |reverse| takes a single list argument, whose possibilities
are the normal ones for a list: it is either empty, or it is not.  And
thus:
\begin{spec}
reverse         :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]
\end{spec}
This, in fact, is a perfectly good definition for |reverse|---it is
certainly clear---except for one small problem: it is terribly
inefficient!  To see why, first recall that the number of steps needed
to compute |xs ++ ys| is proportional to the length of |xs|.
Now suppose that the list argument to |reverse| has length $n$.
The recursive call to |reverse| will return a list of length $n-1$,
which is the first argument to |(++)|.  Thus the cost to reverse a
list of length of $n$ will be proportional to $n-1$ plus the cost to
reverse a list of length $n-1$.  So the total cost is proportional to
$(n-1)+(n-2)+\cdots+1 = n(n-1)/2$, which in turn is proportional to
the square of $n$.

Can we do better than this?  In fact, yes.

There is another algorithm for reversing a list, which can be
described intuitively as follows: take the first element, and put it
at the front of an empty auxiliary list; then take the next element
and add it to the front of the auxiliary list (thus the auxiliary list
now consists of the first two elements in the original list, but in
reverse order); then do this again and again until the end of the
original list is reached.  At that point the auxiliary list will be
the reverse of the original one.

This algorithm can be expressed recursively, but the auxiliary list
implies the need for a function that takes {\em two} arguments---the
original list and the auxiliary one---yet |reverse| only takes one.
This can be solved by creating an auxiliary function |rev|:
\begin{spec}
reverse xs =  let  rev acc []      = acc
                   rev acc (x:xs)  = rev (x:acc) xs
              in rev [] xs
\end{spec}
The auxiliary list is the first argument to |rev|, and is called
|acc| since it behaves as an ``accumulator'' of the intermediate
results.  Note how it is returned as the final result once the end of
the original list is reached.
\index{accumulator}

A little thought should convince the reader that this function does
not have the quadratic ($n^2$) behavior of the first algorithm, and
indeed can be shown to execute a number of steps that is directly
proportional to the length of the list, which we can hardly expect to
improve upon.

But now, compare the definition of |rev| with the definition of
|foldl|:
\begin{spec}
foldl op init []      = init
foldl op init (x:xs)  = foldl op (init `op` x) xs
\end{spec}
They are somewhat similar.  In fact, suppose we were to slightly
revise the definition of |rev| as follows:
\begin{spec}
rev op acc []      = acc
rev op acc (x:xs)  = rev op (acc `op` x) xs
\end{spec}
Now |rev| looks strongly like |foldl|, and the question becomes
whether or not there is a function that can be substituted for |op|
that would make the latter definition of |rev| equivalent to the
former one.  Indeed there is: 
\begin{spec}
revOp a b = b : a
\end{spec}
For note that:
\begin{spec}
acc `revOp` x  
==> revOp acc x 
==> x : acc
\end{spec}
So |reverse| can be rewritten as:
\begin{spec}
reverse xs =  let  rev op acc []      = acc
                   rev op acc (x:xs)  = rev op (acc `op` x) xs
              in rev revOp [] xs
\end{spec}
which is the same as:
\begin{spec}
reverse xs = foldl revOp [] xs
\end{spec}

If all of this seems like magic, well, you are starting to see the
beauty of functional programming!

\section{Currying}
\label{sec:currying}

\index{function!currying||(}
We can improve further upon some of the definitions given in this
chapter using a technique called \emph{currying simplification}.  To
understand this idea, first look closer at the notation used to write
function applications, such as |simple x y z|.  Although, as noted
earlier, this is similar to the mathematical notation
$\mathit{simple}(x,y,z)$, in fact there is an important difference,
namely that |simple x y z| is actually shorthand for |(((simple x) y)
z)|.  In other words, function application is {\em left associative},
taking one argument at a time.

\index{function!application}
Now look at the expression |(((simple x) y) z)| a bit closer: there is
an application of |simple| to |x|, the result of which is applied to
|y|; so |(simple x)| must be a function!  The result of this
application, |((simple x) y)|, is then applied to |z|, so |((simple x)
y)| must also be a function!

Since each of these intermediate applications yields a function, it
seems perfectly reasonable to define a function such as:
\begin{spec}
multSumByFive = simple 5
\end{spec}
What is |simple 5|?  From the above argument it is clear that it must
be a function.  And from the definition of |simple| in Section
\ref{ch:intro} we might guess that this function takes two arguments,
and returns 5 times their sum.  Indeed, we can {\em calculate} this
result as follows:
\begin{spec}
multSumByFive a b 
==> (simple 5) a b 
==> simple 5 a b 
==> 5*(a+b)
\end{spec}
\index{Curry, Haskell B.}  
The intermediate step with parentheses is included just for clarity.
This method of applying functions to one argument at a time, yielding
intermediate functions along the way, is called {\em currying}, after
the logician Haskell B.\ Curry who popularized the idea.\footnote{It
was actually Sch\"{o}nfinkel who first called attention to this idea
\cite{scho24}, but the word ``sch\"{o}nfinkelling'' is rather a
mouthful!}  It is helpful to look at the types of the intermediate
functions as arguments are applied:
\begin{spec}
simple        :: Float -> Float -> Float -> Float
simple 5      :: Float -> Float -> Float
simple 5 a    :: Float -> Float
simple 5 a b  :: Float
\end{spec}

For a musical example of this idea, recall the function |note :: Dur
-> Pitch -> Music Pitch|.  So |note qn| is a function that, given a
pitch, yields a quarter note rendition of that pitch.  A common use of
this idea is simplifying something like:
\begin{spec}
note qn p1 :+: note qn p2 :+: ... :+: note qn pn
\end{spec}
to:
\begin{spec}
line (map (note qn) [ p1, p2, ..., pn ])
\end{spec}
Indeed, this idea is used extentively in the larger example in the
next chapter.

\subsection{Currying Simplification}
\label{sec:currying-simplification}

We can also use currying to improve some of the previous function
definitions as follows.  Suppose that the values of |f x| and |g x|
are the same, for all values of |x|.  Then it seems clear that the
functions |f| and |g| are equivalent.\footnote{In mathematics, we
  would say that the two functions are \emph{extensionally
  equivalent}.}  So, if we wish to define |f| in terms of |g|,
instead of writing:
\begin{spec}
f x = g x
\end{spec}
We could instead simply write:
\begin{spec}
f = g
\end{spec}

We can apply this reasoning to the definitions of |line| and |chord|
from Section \ref{sec:fold}:
\begin{spec}
line  ms  = fold (:+:)  (rest 0) ms
chord ms  = fold (:=:)  (rest 0) ms
\end{spec}
Since function application is left associative, we can rewrite these as:
\begin{spec}
line  ms  = (fold (:+:)  (rest 0)) ms
chord ms  = (fold (:=:)  (rest 0)) ms
\end{spec}
But now applying the same reasoning here as was used for |f| and |g|
above means that we can write these simply as:
\begin{spec}
line   = fold (:+:)  (rest 0)
chord  = fold (:=:)  (rest 0)
\end{spec}

Similarly, the definitions of |toAbsPitches| and |toPitches| from
Section \ref{sec:rec-abstraction}:
\begin{spec}
toAbsPitches  ps  = map absPitch ps
toPitches     as  = map pitch as
\end{spec}
can be rewritten as:
\begin{spec}
toAbsPitches  = map absPitch
toPitches     = map pitch
\end{spec}

Furthermore, the definition |hList|, most recently defined as:
\begin{spec}
hList d ps =  let f p = hNote d p
              in line (map f ps)
\end{spec}
can be rewritten as:
\begin{spec}
hList d ps =  let f = hNote d
              in line (map f ps)
\end{spec}
and since the definition of |f| is now so simple, we might as well
``in-line'' it:
\begin{spec}
hList d ps = line (map (hNote d) ps)
\end{spec}

This kind of simplification will be referred to as ``currying
simplification'' or just ``currying.''\footnote{In the Lambda Calculus
  this is called ``eta contraction.''}

\syn{Some care should be taken when using this simplification idea.
  In particular, note that an equation such as |f x = g x y x|
  cannot be simplified to |f = g x y|, since then the remaining
  |x| on the right-hand side would become undefined!}

\subsection{[Advanced] Simplification of |reverse|}

Here is a more interesting example, in which currying simplification
is used three times.  Recall from Section \ref{sec:reverse} the
definition of |reverse| using |foldl|:
\begin{spec}
reverse xs =  let revOp acc x = x : acc
              in foldl revOp [] xs
\end{spec}
Using the polymorphic function \indexwdhs{flip} which is defined in the
Standard Prelude as:
\begin{spec}
flip        :: (a -> b -> c) -> (b -> a -> c)
flip f x y  = f y x
\end{spec}
it should be clear that |revOp| can be rewritten as:
\begin{spec}
revOp acc x = flip (:) acc x
\end{spec}
But now currying simplification can be used twice to reveal that:
\begin{spec}
revOp = flip (:)
\end{spec}
This, along with a third use of currying, allows us to rewrite the
definition of \indexwdhs{reverse} simply as:
\begin{spec}
reverse = foldl (flip (:)) []
\end{spec}
This is in fact the way |reverse| is defined in the Standard Prelude.

\vspace{.1in}\hrule

\begin{exercise}{\em
Show that |flip (flip f)| is the same as |f|.}
\end{exercise} 

\begin{exercise}{\em
What is the type of |ys| in:
\begin{spec}
xs  = [1,2,3] :: [Integer]
ys  = map (+) xs
\end{spec}
}
\end{exercise} 

\begin{exercise}{\em
Define a function |applyEach| that, given a list of functions,
applies each to some given value.  For example:
\begin{spec}
applyEach [simple 2 2, (+3)] 5  ===>  [14, 8]
\end{spec}
where |simple| is as defined in Chapter \ref{ch:intro}.}
\end{exercise} 

\begin{exercise}{\em
Define a function |applyAll| that, given a list of functions
|[f1, f2, ..., fn]| and a value |v|, returns the result
|f1 (f2 (...(fn v)...))|.  For example:
\begin{spec}
applyAll [simple 2 2, (+3)] 5  ===>  20
\end{spec}
}
\end{exercise} 

\begin{exercise}{\em
Recall the discussion about the efficiency of |(++)| and
|concat| in Chapter \ref{ch:poly}.  Which of the following
functions is more efficient, and why?
\begin{spec}
appendr, appendl :: [[a]] -> [a]
appendr  = foldr  (flip (++)) []
appendl  = foldl  (flip (++)) []
\end{spec}
}
\end{exercise} 

\vspace{.1in}\hrule

\index{function!currying||)}

\section{Errors}
\label{sec:errors}

The last section suggested the idea of ``returning an error'' when the
argument to |foldr1| is the empty list.  As you might imagine, there
are other situations where an error result is also warranted.

\index{errors}\index{bottom} 
There are many ways to deal with such situations, depending on the
application, but sometimes all we want to do is stop the program,
signalling to the user that some kind of an error has occurred.
In Haskell this is done with the Standard Prelude function
\indexhs{error}|error :: String -> a|.  Note that |error| is
polymorphic, meaning that it can be used with any data type.  The
value of the expression |error s| is |bottom|, the completely
undefined, or ``bottom'' value that was discussed in Section
\ref{sec:expressions}.  As an example of its use, here is the
definition of |foldr1| from the Standard Prelude:
\begin{spec}
foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 f []      =  error "Prelude.foldr1: empty list"
\end{spec}
Thus if the anomalous situation arises, the program will terminate
immediately, and the string |"Prelude.foldr1: empty list"| will be
printed.

%% \syn{Strings, i.e.\ sequences of characters, were briefly introduced
%%   in Chapter \ref{ch:intro}.  They are written between double quotes,
%%   as in |"Hello World"|.  
%%   %% When typed on your computer, however, it will look a little
%%   %% differently, as in {\tt "Hello World"} (the double-quote
%%   %% character is the same at both ends of the string).  
%%   Strings have type \indexwdhs{String}.  The |"\n"| at the end of
%%   the string above is a ``newline'' character; that is, if another
%%   string were printed just after this one, it would appear beginning
%%   on the next line, rather than just after ``Hello World.''}

\vspace{.1in}\hrule

\begin{exercise}{\em
Rewrite the definition of |length| non-recursively.}
\end{exercise}

\begin{exercise}{\em
Define a function that behaves as each of the following:
\begin{enumerate}[a)]
\item Doubles each number in a list.  For example:
\begin{spec}
doubleEach [1,2,3] ===> [2,4,6]
\end{spec}
\item Pairs each element in a list with that number and one plus
that number.  For example:
\begin{spec}
pairAndOne [1,2,3] ===> [(1,2),(2,3),(3,4)]
\end{spec}
\item Adds together each pair of numbers in a list.  For example:
\begin{spec}
addEachPair [(1,2),(3,4),(5,6)] ===> [3,7,11]
\end{spec}
\item Adds ``pointwise'' the elements of a list of pairs.  For example:
\begin{spec}
addPairsPointwise [(1,2),(3,4),(5,6)] ===> (9,12)
\end{spec}
\end{enumerate} 
}
\end{exercise}

\begin{exercise}{\em
Define a polymorphic function |fuse :: [Dur] -> [Dur -> Music a] ->
[Music a]| that combines a list of durations with a list of notes
lacking a duration, to create a list of complete notes.  For example:
\begin{spec}
fuse [qn, hn, sn] [c 4, d 4, e 4]
===> [c 4 qn, d 4 hn, e 4 sn]
\end{spec}
You may signal an error if the lists have unequal lengths. }
\label{ex:fuse}
\end{exercise}

In the next two exercises, give both recursive and (if possible)
non-recursive definitions, and be sure to include type signatures.

\begin{exercise}{\em
Define a function |maxAbsPitch| that determines the maximum absolute
pitch of a list of absolute pitches.  Define |minAbsPitch|
analogously.  Both functions should return an error if applied to the
empty list.}
\end{exercise} 

\begin{exercise}
\label{ex:chrom}{\em
Define a function |chrom :: Pitch -> Pitch -> Music Pitch| such that
|chrom p1 p2| is a chromatic scale of quarter-notes whose first pitch
is |p1| and last pitch is |p2|.  If |p1 > p2|, the scale should be
descending, otherwise it should be ascending.  If |p1 == p2|, then the
scale should contain just one note.  (A chromatic scale is one whose
successive pitches are separated by one absolute pitch (i.e.\ one
semitone)).}
\end{exercise}

\begin{exercise}
\label{ex:mkscale}{\em
Abstractly, a scale can be described by the intervals between
successive notes.  For example, the 7-note major scale can be defined
as the sequence of 6 intervals |[2,2,1,2,2,2]|, and the 12-note
chromatic scale by the 11 intervals |[1,1,1,1,1,1,1,1,1,1,1]|.  Define
a function |mkScale :: Pitch -> [Int] -> Music Pitch| such that
|mkScale p ints| is the scale beginning at pitch |p| and having the
intervallic structure |ints|.}
\end{exercise} 

\begin{exercise}{\em
Define an enumerated data type that captures each of the standard
major scale modes: Ionian, Dorian, Phrygian, Lydian, Mixolydian,
Aeolian, and Locrian.  Then define a function |genScale| that, given
one of these contructors, generates a scale in the intervalic form
described in Exercise \ref{ex:mkscale}.}
\end{exercise}

\begin{exercise}{\em
Write the melody of ``Fr\`{e}re Jacques'' (or, ``Are You Sleeping'')
in Euterpea.  Try to make it as succinct as possible.  Then, using
functions already defined, generate a traditional four-part round,
i.e.\ four identical voices, each delayed successively by two
measures.  Use a different instrument to realize each voice.}
\label{ex:frere-jacques}
\end{exercise} 

\begin{exercise}{\em
Freddie the Frog wants to communicate privately with his girlfriend
Francine by {\em encrypting} messages sent to her.  Frog brains are
not that large, so they agree on this simple strategy: each character
in the text shall be converted to the character ``one greater'' than
it, based on the representation described below (with wrap-around from
255 to 0).  Define functions |encrypt| and |decrypt| that will
allow Freddie and Francine to communicate using this strategy.}
\end{exercise} 

\syn{Characters are often represented inside a computer as some kind
  of an integer; in the case of Haskell, a 16-bit unicode
  representation is used.  However, the standard keyboard is
  adequately represented by a standard byte (eight bits), and thus we
  only need to consider the first 256 codes in the unicode
  representation.  For the above exercise, you will want to use two
  Haskell functions, |toEnum| and |fromEnum|.  The first will convert
  an integer into a character, the second will convert a character
  into an integer.}

\out{
\begin{exercise}{\em
Suppose you are given a non-negative integer |amt| representing a
sum of money, and a list of coin denominations |[v1, v2, ..., vn]|,
each being a positive integer.  Your job is to make change for
|amt| using the coins in the coin supply.  Define a function
|makeChange| to solve this problem.  For example, your function may
behave like this:
\begin{spec}
makeChange 99 [5,1] ==> [19,4]
\end{spec}
where |99| is the amount and |[5,1]| represents the types of coins
(say, nickels and pennies in US currency) that are available.  The
answer |[19,4]| means that we can make the exact change with |19|
|5|-unit coins and |4| single-unit coins; this is the best possible
solution (in terms of the total number of coins).

To make things slightly easier, you may assume that the list
representing the coin denominations is given in descending order, and
that the single-unit coin is always one of the coin types.}
\end{exercise}
}
