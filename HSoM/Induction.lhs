%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Proof by Induction}
\label{ch:induction}

In this chapter we will study a powerful proof technique based on
\emph{mathematical induction}.  With it we will be able to prove
complex and important properties of programs that cannot be
accomplished with proof-by-calculation alone.  The inductive proof
method is one of the most powerful and common methods for proving
program properties.

\section{Induction and Recursion}
\index{induction} \index{recursion}

\emph{Induction} is very closely related to \emph{recursion}.  In
fact, in certain contexts the terms are used interchangeably; in
others, one is preferred over the other primarily for historical
reasons.  Think of them as being duals of one another: induction is
used to describe the process of starting with something small and
simple, and building up from there, whereas recursion describes the
process of starting with something large and complex, and working
backward to the simplest case.

For example, although we have previously used the phrase
\emph{recursive data type}, in fact data types are often described
\emph{inductively}, such as a list:
\begin{quote}
A \emph{list} is either empty, or it is a pair consisting of a value
and another list.
\end{quote}
On the other hand, we usually describe functions that manipulate
lists, such as |map| and |foldr|, as being recursive.  This is
because when you apply a function such as |map|, you apply it
initially to the whole list, and work backwards toward |[]|.  

But these differences between induction and recursion run no deeper:
they are really just two sides of the same coin.

This chapter is about \emph{inductive properties} of programs (but
based on the above argument could just as rightly be called
\emph{recursive properties}) that are not usually proven via
calculation alone.  Proving inductive properties usually involves the
inductive nature of data types and the recursive nature of functions
defined on the data types.

As an example, suppose that |p| is an inductive property of a list.
In other words, |p(l)| for some list |l| is either true or false (no
middle ground!).  To prove this property inductively, we do so based
on the length of the list: starting with length 0, we first prove
|p([])| (using our standard method of proof-by-calculation).

Now for the key step: assume for the moment that |p(xs)| is true for
any list |xs| whose length is less than or equal to |n|.  Then if we
can prove (via calculation) that |p(x:xs)| is true for any |x|---i.e.\
that |p| is true for lists of length |n+1|---then the claim is that
|p| is true for lists of \emph{any} (finite) length.

Why is this so?  Well, from the first step above we know that |p| is
true for length 0, so the second step tells us that it is also true for
length 1.  But if it is true for length 1 then it must also be true for
length 2; similarly for lengths 3, 4, etc.\  So |p| is true for lists
of any length!

(It it important to realize, however, that a property being true for
every finite list does not necessarily imply that it is true for every
infinite list.  The property ``the list is finite'' is a perfect
example of this!  We will see how to prove properties of infinite lists
in Chapter \ref{ch:streams}.)  \index{list!infinite}

To summarize, to prove a property |p| by induction on the length of a
list, we proceed in two steps: 
\begin{enumerate} 
\item Prove |p([])| (this is called the \emph{base case}).
\item Assume that |p(xs)| is true (this is called the \emph{induction
hypothesis}, and prove that |p(x:xs)| is true (this is called the
\emph{induction step}).
\end{enumerate} 

\section{Examples of List Induction}
\label{sec:list-examples}

Ok, enough talk, let's see this idea in action.  Recall in Section
\ref{sec:poly-types} the following property about \indexwdhs{foldr}:
\[(\forall|xs|)\ \ |foldr (:) [] xs  ===>  xs|\]
We will prove this by induction on the length of |xs|.  Following
the ideas above, we begin with the base case by proving the property
for length 0; i.e.\ for |xs = []|:
\begin{spec}
foldr (:) [] [] 
==> { unfold foldr }
[]
\end{spec}
This step is immediate from the definition of |foldr|.  Now for
the induction step: we first \emph{assume} that the property is true
for all lists |xs| of length |n|, and then prove the property for
list |x:xs|.  Again proceeding by calculation:
\begin{spec}
foldr (:) [] (x:xs) 
==> { unfold foldr }
x : foldr (:) [] xs
==> { induction hypothesis }
x : xs
\end{spec}
And we are done; the induction hypothesis is what justifies the second
step.

Now let's do something a bit harder.  Suppose we are interested in
proving the following property:
\[(\forall|xs,ys|)\ \ |length (xs ++ ys) = length xs + length ys|\]
Our first problem is to decide which list to perform the induction
over.  A little thought (in particular, a look at how the definitions
of \indexwdhs{length} and |(++)| are structured) should convince you that
|xs| is the right choice.  (If you do not see this, you are
encouraged to try the proof by induction over the length of |ys|!)
Again following the ideas above, we begin with the base case by
proving the property for length 0; i.e.\ for |xs = []|:
\begin{spec}
length ([] ++ ys) 
==> { unfold (++) }
length ys 
==> { fold (+) }
0 + length ys 
==> { fold length }
length [] + length ys
\end{spec}
For the induction step, we first assume that the property is true for
all lists |xs| of length |n|, and then prove the property for list
|x:xs|.  Again proceeding by calculation:
\begin{spec}
length ((x:xs) ++ ys) 
==> { unfold (++) }
length (x : (xs ++ ys))
==> { unfold length }
1 + length (xs ++ ys) 
==> { induction hypothesis }
1 + (length xs + length ys)
==> { associativity of (+) }
(1 + length xs) + length ys
==> { fold length }
length (x:xs) + length ys
\end{spec}
And we are done.  The transition from the 3rd line to the 4th is where
the induction hypothesis is used.

\section{Proving Function Equivalences}

At this point it is a simple matter to return to Chapter~\ref{ch:poly}
and supply the proofs that functions defined using |map| and |fold|
are equivalent to the recursively defined versions.  In particular,
recall these two definitions of |toAbsPitches|:
\begin{spec}
toAbsPitches1 []      = []
toAbsPitches1 (p:ps)  = absPitch p : toAbsPitches1 ps

toAbsPitches2 = map absPitch
\end{spec}
We want to prove that |toAbsPitches1 = toAbsPitches2|.  To do so, we
use the extensionality principle (briefly discussed in
Section~\ref{sec:currying-simplification}), which says that two
functions are equal if, when applied to the same value, they always
yield the same result.  We can change the specification slightly to
reflect this.  For any finite list |ps|, we want to prove:
\begin{spec}
toAbsPitches1 ps = toAbsPitches2 ps
\end{spec}

We proceed by induction, starting with the base case |ps = []|:
\begin{spec}
toAbsPitches1 []
==> []
==> map absPitch []
==> toAbsPitches2 []
\end{spec}
Next we assume that |toAbsPitches1 ps = toAbsPitches2 ps| holds, and
try to prove that |toAbsPitches1 (p:ps) = toAbsPitches2 (p:ps)|:
\begin{spec}
toAbsPitches1 (p:ps)
==> absPitch p : toAbsPitches1 ps
==> absPitch p : toAbsPitches2 ps
==> absPitch p : map absPitch ps
==> map absPitch (p:ps)
\end{spec}
Note the use of the induction hypothesis in the second step.

%% The proof that the two versions of |toPitches| given in
%% Chapter~\ref{ch:poly} is very similar, and is left as an exercise.

For a proof involving |foldr|, recall from Chapter~\ref{ch:poly} this
recursive definition of |line|:
\begin{spec}
line1 []      = rest 0
line1 (m:ms)  = m :+: line1 ms
\end{spec}
and this non-recursive version:
\begin{spec}
line2 = foldr (:+:) (rest 0)
\end{spec}
We can prove that these two functions are equivalent by induction.
First the base case:
\begin{spec}
line1 []
==> rest 0
==> foldr (:+:) (rest 0) []
==> line2 []
\end{spec}
Then the induction step:
\begin{spec}
line1 (m:ms)
==> m :+: line1 ms
==> m :+: line2 ms
==> m :+: foldr (:+:) (rest 0) ms
==> foldr (:+:) (rest 0) (m:ms)
==> line2 (m:ms)
\end{spec}

The proofs of equivalence of the definitions of |toPitches|, |chord|,
|maxPitch|, and |hList| from Chapter~\ref{ch:poly} are similar, and
left as an exercise.

\vspace{.1in}\hrule

\begin{exercise}{\em
From Chapter~\ref{ch:poly}, prove that the original recursive versions
of the following functions are equivalent to the versions using |map|
or |fold|: |toPitches|, |chord|, |maxPitch|, and |hList|.}
\end{exercise}

\vspace{.1in}\hrule

\subsection{[Advanced] Reverse}

The proofs of function equivalence in the last section were fairly
straightforward.  For something more challenging, consider the
definition of \indexwdhs{reverse} given in Section \ref{sec:reverse}:
\begin{spec}
reverse1  []     = []
reverse1 (x:xs)  = reverse1 xs ++ [x]
\end{spec}
and the version given in Section \ref{sec:currying}:
\begin{spec}
reverse2 xs = foldl (flip (:)) [] xs
\end{spec}
We would like to show that these are the same; i.e.\ that 
|reverse1 xs = reverse2 xs| for any finite list |xs|.  In
carrying out this proof one new idea will be demonstrated, namely
 the need for an \emph{auxiliary property}
which is proved independently of the main result.

%% the first being that induction can be used to prove the equivalence of
%% two programs.  The second is

The base case is easy, as it often is:
\begin{spec}
reverse1 [] 
==> []
==> foldl (flip (:)) [] []
==> reverse2 []
\end{spec}
Assume now that |reverse1 xs = reverse2 xs|.  The induction step
proceeds as follows:
\begin{spec}
reverse1 (x:xs)
==> reverse1 xs ++ [x]
==> reverse2 xs ++ [x]
==> foldl (flip (:)) [] xs ++ [x]
==> ???
\end{spec}
But now what do we do?  Intuitively, it seems that the following
property, which we will call property (1), should hold:
\begin{spec}
foldl (flip (:)) [] xs ++ [x]
==> foldl (flip (:)) [] (x:xs)
\end{spec}
in which case we could complete the proof as follows:
\begin{spec}
...
==> foldl (flip (:)) [] xs ++ [x]
==> foldl (flip (:)) [] (x:xs)
==> reverse2 (x:xs)
\end{spec}

The ability to see that if we could just prove one thing, then perhaps
we could prove another, is a useful skill in conducting proofs.  In
this case we have reduced the overall problem to one of proving
property (1), which simplifies the structure of the proof, although
not necessarily the difficulty.  These auxiliary properties are often
called \emph{lemmas} in mathematics, and in many cases their proofs
become the most important contributions, since they are often at the
heart of a problem.

In fact if you try to prove property (1) directly, you will run into a
problem, namely that it is not \emph{general} enough.  So first let's
generalize property (1) (while renaming |x| to |y|), as follows:
\index{generalization}
\begin{spec}
foldl (flip (:)) ys xs ++ [y]
==> foldl (flip (:)) (ys++[y]) xs
\end{spec}
Let's call this property (2).  If (2) is true for any finite |xs|
and |ys|, then property (1) is also true, because:
\begin{spec}
foldl (flip (:)) [] xs ++ [x]
==> { property (2) }
foldl (flip (:)) ([]++[x]) xs
==> { unfold (++) }
foldl (flip (:)) [x] xs
==> { fold (flip (:)) }
foldl (flip (:)) (flip (:) [] x) xs
==> { fold foldl }
foldl (flip (:)) [] (x:xs)
\end{spec}

You are encouraged to try proving property (1) directly, in which case
you will likely come to the same conclusion, namely that the property
needs to be generalized.  This is not always easy to see, but is
sometimes an important step is constructing a proof, because, despite
being somewhat counterintuitive, it is often the case that making a
property more general (and therefore more powerful) makes it easier to
prove.

In any case, how do we prove property (2)?  Using induction, of
course!  Setting |xs| to |[]|, the base case is easy:
\begin{spec}
foldl (flip (:)) ys [] ++ [y]
==> { unfold foldl }
ys++[y]
==> { fold foldl }
foldl (flip (:)) (ys++[y]) []
\end{spec}
and the induction step proceeds as follows:
\begin{spec}
foldl (flip (:)) ys (x:xs) ++ [y]
==> { unfold foldl }
foldl (flip (:)) (flip (:) ys x) xs ++ [y]
==> { unfold flip }
foldl (flip (:)) (x:ys) xs ++ [y]
==> { induction hypothesis }
foldl (flip (:)) ((x:ys)++[y]) xs
==> { unfold (++) }
foldl (flip (:)) (x:(ys++[y])) xs
==> { fold foldl }
foldl (flip (:)) (ys++[y]) (x:xs)
\end{spec}

\out{
Here is why the generalization is needed.  If we just try to prove:

foldl (flip (:)) [] xs ++ [x]
==> foldl (flip (:)) [x] xs

Then the base case is fine:

foldl (flip (:)) [] [] ++ [x]
==> { unfold foldl }
[]++[x]
==> { unfold (++) }
[x]
==> { fold foldl }
foldl (flip (:)) [x] []

But the induction step runs into trouble:

foldl (flip (:)) [] (x:xs) ++ [y]
==> { unfold foldl }
foldl (flip (:)) (flip (:) [] x) xs ++ [y]
==> { unfold flip }
foldl (flip (:)) [x] xs ++ [y]
==> ???

What now?  We are stuck.  In particular, we cannot apply the induction
hypothesis because foldl's third argument is |[x]| and not |[]|.
}

\section{Useful Properties on Lists}
\label{sec:list-properties}

There are many useful properties of functions on lists that require
inductive proofs.  Figures \ref{fig:list-props1} and
\ref{fig:list-props2} list a number of them involving functions used
in this text, but their proofs are left as exercises (except for one;
see below).  You may assume that these properties are true, and use
them freely in proving other properties of your programs.  In fact,
some of these properties can be used to simplify the proof that
|reverse1| and |reverse2| are the same; see if you can find
them!\footnote{More thorough discussions of these properties and their
  proofs may be found in \cite{birdwadler88,bird98}.}

(Note, by the way, that in the first rule for |map| in Figure
\ref{fig:list-props1}, the type of |\x -> x| on the left-hand
side is |a->b|, whereas on the right-hand side it is |[a]->[b]|;
i.e. these are really two different functions.)

\begin{figure}
\cbox{
\begin{minipage}{4.75in}
{\bf Properties of |map|:}

\begin{spec}
map (\x->x)       = \x->x
map (f . g)       = map f . map g
map f . tail      = tail . map f
map f . reverse   = reverse . map f
map f . concat    = concat . map (map f)
map f (xs ++ ys)  = map f xs ++ map f ys
\end{spec}
For all strict |f|:
\begin{spec}
f . head = head . map f
\end{spec}
\vspace{0.1in}

{\bf Properties of the |fold| functions:}

\begin{enumerate}
\item If |op| is associative, and |e `op` x = x| and |x `op` e = x|
for all |x|, then for all finite |xs|:
\begin{spec}
foldr op e xs = foldl op e xs
\end{spec}
\item If the following are true:
\begin{spec}
x `op1` (y `op2` z)  = (x `op1` y) `op2` z
x `op1` e            = e `op2` x
\end{spec}
then for all finite |xs|:
\begin{spec}
foldr op1 e xs = foldl op2 e xs
\end{spec}
\item For all finite |xs|:
\begin{spec}
foldr op e xs = foldl (flip op) e (reverse xs)
\end{spec}
\end{enumerate}
\end{minipage}}
\caption{Some Useful Properties of |map| and |fold|.}
\label{fig:list-props1}
\end{figure}

\begin{figure}
\cbox{
\begin{minipage}{4.75in}
{\bf Properties of |(++)|:}

\vspace{0.1in} For all |xs|, |ys|, and |zs|:
\begin{spec}
(xs ++ ys) ++ zs  = xs ++ (ys ++ zs)
xs ++ []          = [] ++ xs = xs
\end{spec}

\vspace{0.1in}
{\bf Properties of |take| and |drop|:}

\vspace{0.1in} 
\begin{spec}
take m . take n         = take (min m n)
drop m . drop n         = drop (m + n)
take m . drop n         = drop n . take (m + n)
\end{spec}
For all non-negative |m| and |n| such that $n \geq m$:
\begin{spec}
drop m . take n = take (n - m) . drop m
\end{spec}
For all non-negative |m| and |n|, and finite |xs|:
\begin{spec}
take n xs ++ drop n xs  = xs
\end{spec}

\vspace{0.1in}
{\bf Properties of |reverse|:}

\vspace{0.1in} For all finite |xs|:
\begin{spec}
reverse (reverse xs)  = xs
head (reverse xs)     = last xs
last (reverse xs)     = head xs
\end{spec}
\end{minipage}}
\caption{Useful Properties of Other Functions Over Lists}
\label{fig:list-props2}
\end{figure}

\subsection{[Advanced] Function Strictness}

\index{function!strict} \index{bottom}
Note that the last rule for |map| in Figure \ref{fig:list-props1}
is only valid for \emph{strict} functions.  A function |f| is said to
be strict if |f bottom| $=$ |bottom|.  Recall from Section
\ref{sec:expressions} that |bottom| is the value associated with a
non-terminating computation.  So another way to think about a strict
function is that it is one that, when applied to a non-terminating
computation, results in a non-terminating computation.  For example,
the successor function |(+1)| is strict, because |(+1) bottom|
$=$ |bottom + 1| $=$ |bottom|.  In other words, if you apply
|(+1)| to a non-terminating computation, you end up with a
non-terminating computation.

Not all functions in Haskell are strict, and we have to be careful to
say on which argument a function is strict.  For example, |(+)| is
strict on both of its arguments, which is why the section |(+1)| is
also strict.  On the other hand, the constant function:
\begin{spec}
const x y = x
\end{spec}
is strict on its first argument (why?), but not its second, because
|const x bottom| $=$ |x|, for any |x|.

\indexhs{(\&\&)} 
\syn{Understanding strictness requires a careful understanding of
Haskell's pattern-matching rules.  For example, consider the
definition of |(&&)| from the Standard Prelude:
\begin{spec}
(&&)         :: Bool -> Bool -> Bool
True  && x   = x
False && _   = False
\end{spec}

\index{pattern!matching}
When choosing a pattern to match, Haskell starts with the top,
left-most pattern, and works to the right and downward.  So in the
above, |(&&)| first evaluates its left argument.  If that value is
|True|, then the first equation succeeds, and the second argument
gets evaluated because that is the value that is returned.  But if the
first argument is |False|, the second equation succeeds.  In
particular, \emph{it does not bother to evaluate the second argument at
all}, and simply returns |False| as the answer.  This means that
|(&&)| is strict in its first argument, but not its second.

A more detailed discussion of pattern matching is found in Appendix
\ref{ch:patterns}.
}

Let's now look more closely at the last law for |map|, which says
that for all strict |f|:
\begin{spec}
f . head = head . map f
\end{spec}
Let's try to prove this property, starting with the base case, but
ignoring for now the strictness constraint on |f|:
\begin{spec}
f (head [])
==> f bottom
\end{spec}
|head []| is an error, which you will recall has value |bottom|.
So you can see immediately that the issue of strictness might play a
role in the proof, because without knowing anything about |f|,
there is no further calculation to be done here.  Similarly, if we
start with the right-hand side:
\begin{spec}
head (map f [])
==> head []
==> bottom
\end{spec}
It should be clear that for the base case to be true, it must be that
|f bottom| $=$ |bottom|; i.e., |f| must be strict.  Thus we
have essentially ``discovered'' the constraint on the theorem through
the process of trying to prove it!  (This is not an uncommon
phenomenon.)

The induction step is less problematic:
\begin{spec}
f (head (x:xs))
==> f x
==> head (f x : map f xs)
==> head (map f (x:xs))
\end{spec}
and we are done.

\vspace{.1in}\hrule

\begin{exercise}{\em
Prove as many of the properties in Figures \ref{fig:list-props1} and
\ref{fig:list-props2} as you can.}
\end{exercise}

\begin{exercise}{\em
Which of the following functions are strict (if the function takes
more than one argument, specify on which arguments it is strict):
|reverse|, |simple|, |map|, |tail|, |dur|, |revM|,
|(&&)|, |(True &&)|, |(False &&)|, and
the following function:
\begin{spec}
ifFun                :: Bool -> a -> a -> a
ifFun pred cons alt  = if pred then cons else alt
\end{spec}
}
\end{exercise}

\vspace{.1in}\hrule
\vspace{.1in}

\section{Induction on the Music Data Type}
\label{sec:induction-others}

Proof by induction is not limited to lists.  In particular, we can use
it to reason about |Music| values.

For example, recall this property intuitively conjectured in
Section~\ref{sec:music-fold}:
\begin{spec}
mFold Prim (:+:) (:=:) Modify m = m
\end{spec}
To prove this, we again use the extensionality principle, and then
proceed by induction.  But what is the base case?  Recall that the
|Music| data type is defined as:

\begin{spec}
data Music a  = 
       Prim (Primitive a)
    |  Music a :+: Music a
    |  Music a :=: Music a
    |  Modify Control (Music a)
\end{spec}
The only constructor that does not take a |Music| value as an argument
is |Prim|, so that in fact is the only base case.

So, starting with this base case:
\begin{spec}
mFold Prim (:+:) (:=:) Modify (Prim p)
==> Prim p
==> id (Prim p)
\end{spec}
That was easy!  Next, we develop an induction step for each of the
three non-base cases:
\begin{spec}
mFold Prim (:+:) (:=:) Modify (m1 :+: m2)
==>  mFold Prim (:+:) (:=:) Modify m1 :+: 
     mFold Prim (:+:) (:=:) Modify m2
==> m1 :+: m2
==> id (m1 :+: m2)
\end{spec}

\begin{spec}
mFold Prim (:+:) (:=:) Modify (m1 :=: m2)
==>  mFold Prim (:+:) (:=:) Modify m1 :=:
     mFold Prim (:+:) (:=:) Modify m2
==> m1 :=: m2
==> id (m1 :=: m2)
\end{spec}

\begin{spec}
mFold Prim (:+:) (:=:) Modify (Modify c m)
==> Modify c (mFold Prim (:+:) (:=:) Modify m)
==> Modify c m
==> id (Modify c m)
\end{spec}
These three steps were quite easy as well, but is not something we
could have done without induction.

For something more challenging, let's consider the following:
\begin{spec}
dur (revM m) = dur m
\end{spec}
%% ,  if dur m /= bottom
%% The side condition adds an extra twist to this problem.
Again we proceed by induction, starting with the base case:
\begin{spec}
dur (revM (Prim p))
==> dur (Prim p)
\end{spec}
Sequential composition is straightforward:
\begin{spec}
dur (revM (m1 :+: m2))
==> dur (revM m2 :+: revM m1)
==> dur (revM m2) + dur (revM m1)
==> dur m2 + dur m1
==> dur m1 + dur m2
==> dur (m1 :+: m2)
\end{spec}

But things get more complex with parallel composition:
\begin{spec}
dur (revM (m1 :=: m2))
==> dur (  let  d1 = dur m1
                d2 = dur m2
           in if d1>d2  then revM m1 :=: (rest (d1-d2) :+: revM m2)
                        else (rest (d2-d1) :+: revM m1) :=: revM m2)
==>  let  d1 = dur m1
          d2 = dur m2
     in if d1>d2  then dur (revM m1 :=: (rest (d1-d2) :+: revM m2))
                  else dur ((rest (d2-d1) :+: revM m1) :=: revM m2)
...
\end{spec}
At this point, to make things easier to understand, we will consider
each branch of the conditional in turn.  First the consequent branch:
\begin{spec}
dur (revM m1 :=: (rest (d1-d2) :+: revM m2))
==> max (dur (revM m1)) (dur (rest (d1-d2) :+: revM m2))
==> max (dur m1) (dur (rest (d1-d2) :+: revM m2))
==> max (dur m1) (dur (rest (d1-d2)) + dur (revM m2))
==> max (dur m1) ((d1-d2) + dur m2)
==> max (dur m1) (dur m1)
==> dur m1
\end{spec}
And then the alternative:
\begin{spec}
dur ((rest (d2-d1) :+: revM m1) :=: revM m2)
==> max (dur ((rest (d2-d1) :+: revM m1)) (dur (revM m2))
==> max (dur ((rest (d2-d1) :+: revM m1)) (dur m2)
==> max (dur (rest (d2-d1)) + dur (revM m1)) (dur m2)
==> max ((d2-d1) + dur m1) (dur m2)
==> max (dur m2) (dur m2)
==> dur m2
\end{spec}

Now we can continue the proof from above:
\begin{spec}
...
==>  let  d1 = dur m1
          d2 = dur m2
     in if d1>d2  then dur m1
                  else dur m2
==> max (dur m1) (dur m2)
==> dur (m1 :=: m2)
\end{spec}

The final inductive step involves the |Modify| constructor, but recall
that |dur| treats a |Tempo| modification specially, and thus we treat
it specially as well:
\begin{spec}
dur (revM (Modify (Tempo r) m))
==> dur (Modify (Tempo r) (revM m))
==> dur (revM m) / r
==> dur m / r
==> dur (Modify (Tempo r) m)
\end{spec}
Finally, we consider the case that |c /= Tempo r|:
\begin{spec}
dur (revM (Modify c m))
==> dur (Modify c (revM m))
==> Modify c (dur (revM m))
==> Modify c (dur m)
==> dur (Modify c m)
\end{spec}
And we are done.

\vspace{.1in}\hrule
\vspace{.1in}

\begin{exercise}{\em
Recall Exercises \ref{ex:chrom} and \ref{ex:mkscale}.  Prove that, if
|p2 >= p1|:
\begin{spec}
chrom p1 p2 = mkScale p1 (take  (absPitch p2 - absPitch p1) 
                                (repeat 1))
\end{spec}
using the lemma:
\begin{spec}
[m..n] = scanl (+) m (take (n-m) (repeat 1))
\end{spec}
}
\end{exercise}

%% \begin{exercise}{\em
%% Prove that:}
%% \begin{code}
%% mFold (:+:) (:=:) Prim Modify = id
%% \end{code}
%% \end{exercise}

\begin{exercise}{\em
Prove the following facts involving |dur|:}
\begin{spec}
dur (timesM n m)  = n * dur m
dur (takeM d m)   = d, if d <= dur m
\end{spec}
%% dur (revM m)      = dur m,  if dur m /= bottom
\end{exercise}

\begin{exercise}{\em
Prove the following facts involving |mMap|:}
\begin{spec}
mMap id m          = m
mMap f (mMap g m)  = mMap (f . g) m
\end{spec}
\end{exercise}

\begin{exercise}{\em
Prove that, for all |pmap|, |c|, and |m|:
\begin{spec}
perf pmap c m = (perform pmap c m, dur m)
\end{spec}
where |perform| is the function defined in Figure \ref{fig:perform}.
}
\end{exercise}

\vspace{.1in}\hrule
\vspace{.1in}

\subsection{The Need for Musical Equivalence}

In Chapter \ref{ch:intro} we discussed the need for a notion of
\emph{musical equivalence}, noting that, for example, |m :+: rest 0|
``sounds the same'' as |m|, even if the two |Music| values are not
equal as Haskell values.  That same issue can strike us here as we try
to prove intuitively natural properties such as:
\begin{spec}
revM (revM m) = m
\end{spec}
To see why this property cannot be proved without a notion of musical
equivalence, note that:
\begin{spec}
revM (revM (c 4 en :=: d 4 qn))
===> revM ((rest en :+: c 4 en) :=: d 4 qn)
===> (rest 0 :+: c 4 en :+: rest en) :=: d 4 qn
\end{spec}
Clearly the last line above is not equal, as a Haskell value, to |c 4
en :=: d 4 qn|.  But somehow we need to show that these two values
``sound the same'' as musical values.  In the next chapter we will
formally develop the notion of musical equivalence, and with it be
able to prove the validity of our intuitions regarding |revM|, as well
as many other important musical properties.

\section{[Advanced] Induction on Other Data Types}

Proof by induction can be used to reason about many data types.  For
example, we can use it to reason about natural
numbers.\footnote{Indeed, one could argue that a proof by induction
  over finite lists is really an induction over natural numbers, since
  it is an induction over the \emph{length} of the list, which is a
  natural number.}  Suppose we define an exponentiation function as
follows: \index{|(^)|}
\begin{spec}
(^)  :: Integer -> Integer -> Integer
x^0  = 1
x^n  = x * x^(n-1)
\end{spec}

\syn{|(*)| is defined in the Standard Prelude to have precedence
level 7, and recall that if no |infix| declaration is given for an
operator it defaults to \indexwd{precedence} level 9, which means that
|(^)| has precedence level 9, which is higher than that for
|(*)|.  Therefore no parentheses are needed to disambiguate the
last line in the definition above, which corresponds nicely to
mathematical convention.}

Now suppose that we want to prove that:
\[(\forall x, n\geq0, m\geq0)\ \ |x^(n+m) = x^n * x^m|\]
We proceed by induction on |n|, beginning with |n=0|:
\begin{spec}
x^(0+m) 
==> x^m 
==> 1 * (x^m) 
==> x^0 * x^m
\end{spec}

Next we assume that the property is true for numbers less than or
equal to |n|, and prove it for |n+1|:
\begin{spec}
x^((n+1)+m) 
==> x * x^(n+m) 
==> x * (x^n * x^m)
==> (x * x^n) * x^m
==> x^(n+1) * x^m
\end{spec}
and we are done.

Or are we?  What if, in the definition of |(^)|, |x| or |n|
is \emph{negative}?  Since a negative integer is not a natural number,
we could dispense with the problem by saying that these situations
fall beyond the bounds of the property we are trying to prove.  But
let's look a little closer.  If |x| is negative, the property we
are trying to prove still holds (why?).  But if |n| is negative,
|x^n| will not terminate (why?).  As diligent programmers we may
wish to defend against the latter situation by writing:
\begin{spec}
(^)               :: Integer -> Integer -> Integer
x^0               = 1
x^n  | n<0        = error "negative exponent"
     | otherwise  = x * x^(n-1)
\end{spec}
If we consider non-terminating computations and ones that produce an
error to both have the same value, namely |bottom|, then these two
versions of |(^)| are equivalent.  Pragmatically, however, the
latter is clearly superior.

Note that the above definition will test for |n<0| on every
recursive call, when actually the only call in which it could happen
is the first.  Therefore a slightly more efficient version of this
program would be:

\begin{spec}
(^)             :: Integer -> Integer -> Integer
x^n  | n<0        = error "negative exponent"
     | otherwise  = f x n
    where  f x 0   = 1
           f x n   = x * f x (n-1)
\end{spec}
Proving the property stated earlier for this version of the program is
straightforward, with one minor distinction: what we really need to
prove is that the property is true for |f|; that is:
\[(\forall x, n\geq0, m\geq0)\ \ |f x (n+m) = f x n * f x m|\]
from which the proof for the whole function follows trivially.

\subsection{A More Efficient Exponentiation Function}

\index{efficiency}
But in fact there is a more serious inefficiency in our exponentiation
function: we are not taking advantage of the fact that, for any even
number $n$, $x^n = (x*x)^{n/2}$.  Using this fact, here is a more
clever way to accomplish the exponentiation task, using the names
|(^!)| and |ff| for our functions to distinguish them from the
previous versions:
\begin{spec}
(^!)              :: Integer -> Integer -> Integer
x^!n  | n<0        = error "negative exponent"
      | otherwise  = ff x n
     where ff x n   | n==0       = 1
                    | even n     = ff (x*x) (n `quot` 2)
                    | otherwise  = x * ff x (n-1)
\end{spec}

\syn{\indexwdhs{quot} is Haskell's \emph{quotient} operator, which
returns the integer quotient of the first argument divided by the
second, rounded toward zero.}

You should convince yourself that, intuitively at least, this version
of exponentiation is not only correct, but also more efficient.  More
precisely, |(^)| executes a number of steps proportional to |n|,
whereas |(^!)| executes a number of steps proportional to the
$\log_2$ of |n|.  The Standard Prelude defines |(^)| similarly
to the way in which |(^!)| is defined here.

Since intuition is not always reliable, let's \emph{prove} that this
version is equivalent to the old.  That is, we wish to prove that
|x^n = x^!n| for all |x| and |n|.

A quick look at the two definitions reveals that what we really need
to prove is that |f x n = ff x n|, from which it follows
immediately that |x^n = x^!n|.  We do this by induction on |n|,
beginning with the base case |n=0|:
\begin{spec}
f x 0 ==> 1 ==> ff x 0
\end{spec}
so the base case holds trivially.  The induction step, however, is
considerably more complicated.  We must consider two cases: |n+1|
is either even, or it is odd.  If it is odd, we can show that:
\begin{spec}
f x (n+1)
==> x * f x n
==> x * ff x n
==> ff x (n+1)
\end{spec}
and we are done (note the use of the induction hypothesis in the
second step).

If |n+1| is even, we might try proceeding in a similar way:
\begin{spec}
f x (n+1)
==> x * f x n
==> x * ff x n
\end{spec}
But now what shall we do?  Since |n| is odd, we might try
unfolding the call to |ff|:
\begin{spec}
x * ff x n
==> x * (x * ff x (n-1))
\end{spec}
but this does not seem to be getting us anywhere.  Furthermore,
\emph{folding} the call to |ff| (as we did in the odd case) would
involve \emph{doubling} |n| and taking the square root of |x|,
neither of which seems like a good idea!

We could also try going in the other direction:
\begin{spec}
ff x (n+1)
==> ff (x*x) ((n+1) `quot` 2)
==>  f (x*x) ((n+1) `quot` 2)
\end{spec}
The use of the induction hypothesis in the second step needs to be
justified, because the first argument to |f| has changed from
|x| to |x*x|.  But recall that the induction hypothesis states
that for \emph{all} values |x|, and all natural numbers up to |n|,
|f x n| is the same as |ff x n|.  So this is OK.

But even allowing this, we seem to be stuck again!  

Instead of pushing this line of reasoning further, let's pursue a
different tact based on the (valid) assumption that if |m| is even,
then:
\[ |m = m `quot` 2 + m `quot` 2| \]
Let's use this fact together with the property that we proved in the
last section:
\begin{spec}
f x (n+1) 
==> f x ((n+1) `quot` 2 + (n+1) `quot` 2)
==> f x ((n+1) `quot` 2) * f x ((n+1) `quot` 2)
\end{spec}
Next, as with the proof in the last section involving |reverse|,
let's make an assumption about a property that will help us along.
Specifically, what if we could prove that |f x n * f x n| is equal
to |f (x*x) n|?  If so, we could proceed as follows:
\begin{spec}
f x ((n+1) `quot` 2) * f x ((n+1) `quot` 2)
==> f (x*x) ((n+1) `quot` 2)
==> ff (x*x) ((n+1) `quot` 2)
==> ff x (n+1)
\end{spec}
and we are finally done.  Note the use of the induction hypothesis in
the second step, as justified earlier.  The proof of the auxiliary
property is not difficult, but also requires induction; it is shown in
Figure \ref{fig:exp-lemma}.

\begin{figure}
\cbox{
\begin{minipage}{4.75in}
Base case (|n=0|):
\begin{spec}
f x 0 * f x 0
==> 1 * 1 
==> 1
==> f (x*x) 0
\end{spec}
Induction step (|n+1|):
\begin{spec}
f x (n+1) * f x (n+1)
==> (x * f x n) * (x * f x n)
==> (x*x) * (f x n * f x n)
==> (x*x) * f (x*x) n
==> f (x*x) (n+1)
\end{spec}
\end{minipage}}
\caption{Proof that |f x n * f x n = f (x*x) n|.}
\label{fig:exp-lemma}
\end{figure}

Aside from improving efficiency, one of the pleasant outcomes of
proving that |(^)| and |(^!)| are equivalent is that
\emph{anything that we prove about one function will be true for the
  other}.  For example, the validity of the property that we proved
earlier:
\[ |x^(n+m) = x^n * x^m| \]
immediately implies the validity of:
\[ |x^!(n+m) = x^!n * x^!m| \]
Although |(^!)| is more efficient than |(^)|, it is also more
complicated, so it makes sense to try proving new properties for
|(^)|, since the proofs will likely be easier.

The moral of this story is that you should not throw away old code that
is simpler but less efficient than a newer version.  That old code can
serve at least two good purposes: First, if it is simpler, it is
likely to be easier to understand, and thus serves a useful role in
documenting your effort.  Second, as we have just discussed, if it is
provably equivalent to the new code, then it can be used to simplify
the task of proving properties about the new code.

\vspace{.1in}\hrule

\begin{exercise}\em
The function |(^!)| can be made more efficient by noting that in
the last line of the definition of |ff|, |n| is odd, and
therefore |n-1| must be even, so the test for |n| being even on
the next recursive call could be avoided.  Redefine |(^!)|  so that
it avoids this (minor) inefficiency.
\end{exercise}

\begin{exercise}\em
Consider this definition of the \emph{factorial} function:\footnote{The
factorial function is defined mathematically as:
\[\mathit{factorial}(n) = 
    \left\{ \begin{array}{ll}
            1                        & \mbox{if $n=0$} \\
            n * \mathit{factorial} (n-1) & \mbox{otherwise}
            \end{array}
    \right.
\] }
\begin{spec}
fac1    :: Integer -> Integer
fac1 0  = 1
fac1 n  = n * fac1 (n-1)
\end{spec}
and this alternative definition that uses an ``accumulator:''
\begin{spec}
fac2    :: Integer -> Integer
fac2 n  = fac' n 1
  where  fac' 0 acc  = acc
         fac' n acc  = fac' (n-1) (n*acc)
\end{spec}
Prove that |fac1 = fac2|.
\end{exercise}

\vspace{.1in}\hrule

\out{

There are several points that you should remember about this proof process:
\begin{enumerate} 
\item Two programs can be proved equivalent using induction.
\item It is often the case that auxiliary properties are needed to
prove certain properties, whose proofs in turn can be treated
separately.
\item The most obvious proof strategy is not always best.
\end{enumerate} 

Indeed, the full definition of |(^)| as given in the Standard
Prelude is:
\begin{spec}
(^)              :: (Num a, Integral b) => a -> b -> a
x ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n
                              where g x n  | even n     = g (x*x) (n `quot` 2)
                                           | otherwise  = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"
\end{spec}
}
