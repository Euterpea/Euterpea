%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

% To do:
% change glue.eps to include dotted box for f.g

\chapter{Syntactic Magic}
\label{ch:hof}

This chapter introduces several more of Haskell's syntactic devices
that faciliate writing concise and intuitive programs.  These devices
will be used frequently in the remainder of the text.

%% You have now seen several examples where functions are passed as
%% arguments to other functions, such as with |fold| and |map|.  In
%% this chapter we will see several examples where functions are also
%% returned as values.  This will lead to several techniques for
%% improving definitions that we have already written, techniques that we
%% will use often in the remainder of the text.

\section{Sections}
\label{sec:sections}

The use of currying was introduced in Chapter \ref{ch:poly} as a way
to simplify programs.  This is a syntactic device that relies on the
way that normal functions are applied, and how those applications are
parsed.

With a bit more syntax, we can also curry applications of infix
operators such as |(+)|.  This syntax is called a {\em
  \indexwd{section}}, and the idea is that, in an expression such as
|(x+y)|, we can omit either the |x| or the |y|, and the result (with
the parentheses still intact) is a function of that missing argument.
If {\em both} variables are omitted, it is a function of {\em two}
arguments.  In other words, the expressions |(x+)|, |(+y)| and |(+)|
are equivalent, respectively, to the functions:
\begin{spec}
f1 y    = x+y
f2 x    = x+y
f3 x y  = x+y
\end{spec}

For example, suppose we wish to remove all absolute pitches greater
than 99 from a list, perhaps because everything above that value is
assumed to be unplayable.  There is a pre-defined function in Haskell
that can help to achieve this:
\begin{spec}
filter :: (a -> Bool) -> [a] -> [a]
\end{spec}
|filter p xs| returns a list for which each element |x| satisfies the
predicate |p|; i.e.\ |p x| is |True|.

Using |filter|, we can then write:
\begin{spec}
playable     :: [AbsPitch] -> [AbsPitch]
playable xs  =  let test ap = ap < 100
                in filter test xs
\end{spec}
But using a section, we can write this more succinctly as:
\begin{spec}
playable     :: [AbsPitch] -> [AbsPitch]
playable xs  =  filter (<100) xs
\end{spec}
which can be further simplified using currying:
\begin{spec}
playable   :: [AbsPitch] -> [AbsPitch]
playable   =  filter (<100)
\end{spec}

This is an extremely concise definition.  As you gain experience with
higher-order functions you will not only be able to start writing
definitions such as this directly, but you will also start {\em
  thinking} in ``higher-order'' terms.  Many more examples of this
kind of reasoning will appear throughout the text.

%% ... determine, for each absolute
%% pitch in a list, whether it is higher than 57 (which corresponds to
%% concert A).  Instead of writing:

%% \begin{spec}
%% gtConcertA     :: [AbsPitch] -> [Bool]
%% gtConcertA xs  =  let test ap = ap > 57
%%                   in map test xs
%% \end{spec}
%% we can simply write:
%% \begin{spec}
%% gtConcertA     :: [AbsPitch] -> [Bool]
%% gtConcertA xs  = map (>57) xs
%% \end{spec}
%% which can be further simplified using currying:
%% \begin{spec}
%% gtConcertA  :: [AbsPitch] -> [Bool]
%% gtConcertA  = map (>57)
%% \end{spec}

\vspace{.1in}\hrule

\begin{exercise}{\em
Define a function |twice| that, given a function |f|, returns a
function that applies |f| twice to its argument.  For example:
\begin{spec}
(twice (+1)) 2 ==> 4
\end{spec}
What is the principal type of |twice|?  Describe what |twice twice|
does, and give an example of its use.  Also consider the functions
|twice twice twice| and |twice (twice twice)|?  }
\end{exercise}

\begin{exercise}{\em
Generalize |twice| defined in the previous exercise by defining a
function |power| that takes a function |f| and an integer
|n|, and returns a function that applies the function |f| to its
argument |n| times.  For example:
\begin{spec}
power (+2) 5 1  ===>  11
\end{spec}
Use |power| in a musical context to define something useful.
}
\end{exercise}

\vspace{.1in}\hrule

\section{Anonymous Functions}
\label{sec:anonymous}

\index{function!anonymous} 

Another way to define a function in Haskell is in some sense the most
fundamental: it is called an \emph{anonymous function}, or
\emph{lambda expression} (since the concept is drawn directly from
Church's lambda calculus \cite{church41}).  The idea is that functions
are values, just like numbers and characters and strings, and
therefore there should be a way to create them without having to give
them a name.  As a simple example, an anonymous function that
increments its numeric argument by one can be written |\x -> x+1|.
Anonymous functions are most useful in situations where you do not
wish to name them, which is why they are called ``anonymous.''
Anonymity is a property also shared by sections, but sections can only
be derived from an existing infix operator.

\syn{The typesetting used in this textbook prints an actual Greek
  lambda character, but in writing |\x -> x+1| in your programs you
  will have to type ``\verb!\x -> x+1!'' instead.}

As another example, to raise the pitch of every element in a list of
pitches |ps| by an octave, we could write:
\begin{spec}
map (\p-> pitch (absPitch p + 12)) ps
\end{spec}
An even better example is an anonymous function that pattern-matches
its argument, as in the following, which doubles the duration of every
note in a list of notes |ns|:
\begin{spec}
map (\(Note d p) -> Note (2*d) p) ns
\end{spec}

\syn{Anonymous functions can only perform one match against
an argument.  That is, you cannot stack together several anonymous
functions to define one function, as you can with equations.}

Anonymous functions are considered most fundamental because
definitions such as that for |simple| given in Chapter
\ref{ch:intro}:
\begin{spec}
simple x y z = x*(y+z)
\end{spec}
can be written instead as:
\begin{spec}
simple = \x y z -> x*(y+z)
\end{spec}
\syn{|\x y z -> exp| is shorthand for |\x -> \y-> \z -> exp|.}

We can also use anonymous functions to explain precisely the behavior
of sections.  In particular, note that:
\begin{spec}
(x+)  ==>  \y -> x+y
(+y)  ==>  \x -> x+y
(+)   ==>  \x y -> x+y
\end{spec}

\vspace{.1in}\hrule

\begin{exercise}{\em
Suppose we define a function \indexwdhs{fix} as:
\begin{spec}
fix f = f (fix f)
\end{spec}
What is the principal type of |fix|?  (This is tricky!)  Suppose
further that we have a recursive function:
\begin{spec}
remainder      :: Integer -> Integer -> Integer
remainder a b  =  if a<b then a
                  else remainder (a-b) b
\end{spec}
Rewrite this function using |fix| so that it is not recursive.
(Also tricky!)  Do you think that this process can be applied to {\em
any} recursive function?}
\end{exercise}

\vspace{.1in}\hrule

\section{List Comprehensions}
\label{sec:comprehensions}

Haskell has a convenient and intuitive way to define a list in such a
way that it resembles the definition of a \emph{set} in mathematics.
For example, recall in the last chapter the definition of the function
|addDur|:
\begin{spec}
addDur       :: Dur -> [Dur -> Music a] -> Music a
addDur d ns  =  let f n = n d
                in line (map f ns)
\end{spec}
Here |ns| is a list of notes, each of which does not have a duration
yet assigned to it.  If we think of this as a set, we might be led
to write the following solution in mathematical notation:
\[ \{ n\ d\ ||\ n \in ns \} \]
which can be read, ``the set of all notes |n d| such that |n| is an
element of |ns|.''  Indeed, using a Haskell \emph{list comprehension}
we can write almost exactly the same thing:
\begin{spec}
[ n d | n <- ns ]
\end{spec}
The difference, of course, is that the above expression generates an
(ordered) list in Haskell, not an (unordered) set in mathematics.

List comprehensions allow us to rewrite the definition of |addDur|
much more succinctly and elegantly:
\begin{spec}
addDur       :: Dur -> [Dur -> Music a] -> Music a
addDur d ns  =  line [ n d | n <- ns ]
\end{spec}

\syn{Liberty is again taken in type-setting by using the symbol |<-|
  to mean ``is an element of.''  When writing your programs, you will
  have to type ``{\tt <-}'' instead.

The expression |[ exp || x <- xs]| is actually shorthand for the
expression |map (\x -> exp) xs|.  The form |x <- xs| is called a {\em
  \indexwd{generator}}, and in general more than one is allowed, as
in:
\begin{spec}
[ (x,y) | x <- [0,1,2], y <- ['a','b'] ]
\end{spec}
which evaluates to the list:
\begin{spec}
[ (0,'a'), (0,'b'), (1,'a'), (1,'b'), (2,'a'), (2,'b') ]
\end{spec}
The order here is important; that is, note that the left-most
generator changes least quickly.

It is also possible to \emph{filter} values as they are generated; for
example, we can modify the above example to eliminate the odd
integers in the first list:
\begin{spec}
[ (x,y) | x <- [0,1,2], even x, y <- ['a','b'] ]
\end{spec}
where |even n| returns |True| if |n| is even.  This example evaluates
to:
\begin{spec}
[ (0,'a'), (0,'b'), (2,'a'), (2,'b') ]
\end{spec}
}

\syn{When reasoning about list comprehensions (e.g.\ when doing proof
  by calculation), we can use the following syntactic translation
  into pure functions:
\begin{spec}
[ e | True ]           =  [ e ]
[ e | q ]              =  [ e | q, True ]
[ e | b, qs ]          =  if b then [ e | qs ] else []
[ e | p <- xs, qs ]    =  let  ok p  = [ e | qs ]
                               ok _  = []
                          in concatMap ok xs
[ e | let decls, qs ]  =  let decls in [ e | qs ]
\end{spec}
where |q| is a single qualifier, |qs| is a sequence of qualifiers, |b|
is a Boolean, |p| is a pattern, and |decls| is a sequence of variable
bindings (a feature of list comprehensions not explained earlier).
}
%% There are several other useful features of list comprehensions, but
%% they are not used in this text.  Consult the Haskell Report for
%% details.

\subsection{Arithmetic Sequences}
\label{sec:arithmetic-sequences}

\index{arithmetic sequence}
Another convenient syntax for lists whose elements can be enumerated
is called an \emph{arithmetic sequence}.  For example, the
arithmetic sequence |[1..10]| is equivalent to the list:
\begin{spec}
[1,2,3,4,5,6,7,8,9,10]
\end{spec}
There are actually four different versions of arithmetic sequences,
some of which generate \emph{infinite} lists (whose use will be
discussed in a later chapter).  In the following, let |a=n'-n|:
\begin{spec}
[n..]      -- infinite list |n|, |n+1|, |n+2|, ...
[n,n'..]   -- infinite list |n|, |n+a|, |n+2*a|, ...
[n..m]     -- finite list |n|, |n+1|, |n+2|, ..., |m|
[n,n'..m]  -- finite list |n|, |n+a|, |n+2*a|, ..., |m|
\end{spec}

Arithmetic sequences are discussed in greater detail in Appendix
\ref{ch:class-tour}.

\vspace{.1in}\hrule

\begin{exercise}{\em
Using list comprehensions, define a function:
\begin{spec}
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch,AbsPitch)]
\end{spec}
such that |apPairs aps1 aps2| is a list of all combinations of the
absolute pitches in |aps1| and |aps2|.  Furthermore, for each pair
|(ap1,ap2)| in the result, the absolute value of |ap1-ap2| must be
greater than two and less than eight.

Finally, write a function to turn the result of |apPairs| into a
|Music Pitch| value by playing each pair of pitches in parallel, and
stringing them all together sequentially.  Try varying the rhythm by,
for example, using an eighth note when the first absolute pitch is
odd, and a sixteenth note when it is even, or some other criterion.

Test your functions by using arithemtic sequences to generate the two
lists of arguments given to |apPairs|.}
\end{exercise}

\vspace{.1in}\hrule

\section{Function Composition}
\label{sec:function-composition}

\begin{figure*}
\centerline{
\epsfysize=0.75in 
\epsfbox{Pics/glue.eps}
}
\caption{Gluing Two Functions Together}
\label{fig:glue}
\end{figure*}

\index{function!composition||(}
An example of polymorphism that has nothing to do with data structures
arises from the desire to take two functions $f$ and $g$ and ``glue
them together,'' yielding another function $h$ that first applies $g$
to its argument, and then applies $f$ to that result.  This is called
function {\em composition} (just as in mathematics), and Haskell
pre-defines a simple infix operator |(.)| to achieve it, as follows:
\indexhs{(.)}
\begin{spec}
(.)        :: (b->c) -> (a->b) -> a -> c
(f . g) x  = f (g x)
\end{spec}
\syn{The symbol for function composition is typeset in this textbook
  as |.|, which is consistent with mathematical convention.  When
  writing your programs, however, you will have to use a period, as in
  ``\verb!f . g!''.}

Note the type of the operator |(.)|; it is completely polymorphic.
Note also that the result of the first function to be applied---some
type |b|---must be the same as the type of the argument to the second
function to be applied.  Pictorially, if we think of a function as a
black box that takes input at one end and returns some output at the
other, function composition is like connecting two boxes together, end
to end, as shown in Figure \ref{fig:glue}.

The ability to compose functions using |(.)| is quite handy.  For
example, recall the last version of |hList|:
\begin{spec}
hList d ps = line (map (hNote d) ps)
\end{spec}
We can do two simplifications here.  First, rewrite the right-hand
side using function composition:
\begin{spec}
hList d ps = (line . map (hNote d)) ps
\end{spec}
Then, use currying simplification:
\begin{spec}
hList d = line . map (hNote d)
\end{spec}

\section{Higher-Order Thinking}
\label{sec:higher-order-thinking}

It is worth taking a deep breath here and contemplating what has been
done with |hList|, which has gone through quite a few transformations.
Here is the original definition given in Chapter \ref{ch:intro}:
\begin{spec}
hList d []      = rest 0
hList d (p:ps)  = hNote d p :+: hList d ps
\end{spec}
Compare this to the definition above.  You may be distressed to think
that you have to go through all of these transformations just to write
a relatively simple function!  There are two points to make about
this: First, you do not have to make \emph{any} of these
transformations if you do not want to.  All of these versions of
|hList| are correct, and they all run about equally fast.  They are
explained here for pedagogical purposes, so that you understand the
full power of Haskell.  Second, with practice, you will find that you
can write the concise higher-order versions of many functions straight
away, without going through all of the steps presented here.

As mentioned earlier, one thing that helps is to start {\em thinking}
in ``higher-order'' terms.  To facilitate this way of thinking it is
helpful to write type signatures that reflect more closely their
higher-order nature.  For example, recall these type signatures for
|map|, |filter|, and |(.)|:
\begin{spec}
map     :: (a -> b) -> [a] -> [b]
filter  :: (a -> Bool) -> [a] -> [a]
(.)     :: (b->c) -> (a->b) -> a -> c
\end{spec}
Also recall that the arrow in function types is right associative.
Therefore, another completely equivalent way to write the above type
signatures is:
\begin{spec}
map     :: (a -> b) -> ([a] -> [b])
filter  :: (a -> Bool) -> ([a] -> [a])
(.)     :: (b->c) -> (a->b) -> (a->c)
\end{spec}
Although equivalent, the latter versions emphasize the fact that each
of these functions returns a function as its result.  |map|
essentially ``lifts'' a function on elements to a function on lists of
elements.  |filter| converts a predicate into a function on lists.
And |(.)| returns a function that is the composition of its two
functional arguments.

So for example, using higher-order thinking, |map (+12)| is a function
that transposes a list of absolute pitches by one octave.  |filter
(<100)| is a function that removes all absolute pitches greater than
or equal to 100 (as discussed earlier).  And therefore |map (+12)
. filter (<100)| first does the filtering, and then does the
transposition.  All very consise and very natural using higher-order
thinking.

In the remainder of this textbook definitions such as this will be
written directly, using a small set of rich polymorphic functions such
as |foldl|, |map|, |filter|, |(.)|, and a few other functions drawn
from the Standard Prelude and other standard libraries.

%% For example, suppose we wish to define a
%% function that determines whether all of the elements in a list are
%% greater than zero, and one that determines if at least one is greater
%% than zero.  The way to ``think about'' this is to raise ourselves up
%% to the list level, and to just compose functions that transform lists:
%% \begin{spec}
%% allOverZero, oneOverZero  :: [Integer] -> Bool
%% allOverZero               = and . posInts
%% oneOverZero               = or  . posInts
%% \end{spec}
%% \syn{|and :: [Bool] -> Bool| and |or :: [Bool] -> Bool| are predefined
%%   functions that ``and'' and ``or'' together all of the elements in a
%%   list, returning a single Boolean result.  The |Bool| type is
%%   predefined in Haskell simply as:
%% \begin{spec}
%% data Bool = False | True
%% \end{spec}
%% } 
%% Indeed, note that the auxiliary function |posInts| is simple
%% enough that we could incorporate its definition directly, as in:
%% \begin{spec}
%% allOverZero, oneOverZero  :: [Integer] -> Bool
%% allOverZero               = and . map (>0)
%% oneOverZero               = or  . map (>0)
%% \end{spec}

\section{Infix Function Application}

Haskell predefines an infix operator to apply a function to a value:
\begin{spec}
f $ x = f x
\end{spec} % $
At first glance this does not seem very useful---after all, why not
simply write |f x| instead of |f $ x|?  % $

But in fact this operator has a very useful purpose: eliminating
parentheses!  In the Standard Prelude, |($)| %% $ 
is defined to be right associative, and to have the lowest precedence
level, via the fixity declaration:
\begin{spec}
infixr 0 $
\end{spec} %% $
Therefore, note that |f (g x)| is the same as |f $ g x| %% $
(remember that normal function application always has higher
precedence than infix operator application), and |f (x+1)| is the same
as |f $ x + 1|. %% $
This ``trick'' is especially useful when there is a sequence of
nested, parenthesized expresssions.  For example, recall the following
definition from the last chapter:
\begin{spec}
childSong6 =  let t = (dhn/qn)*(69/120)
              in instrument  RhodesPiano 
                             (tempo t (bassLine :=: mainVoice))
\end{spec}

\pagebreak

We can rewrite the last few lines a bit more clearly as follows:
\begin{spec}
childSong6 =  let t = (dhn/qn)*(69/120)
              in  instrument  RhodesPiano  $
                  tempo t                  $ 
                  bassLine :=: mainVoice
\end{spec} 
Or, on a single line, instead of:
\begin{spec}
instrument  RhodesPiano (tempo t (bassLine :=: mainVoice))
\end{spec}
we can write:
\begin{spec}
instrument  RhodesPiano $ tempo t $ bassLine :=: mainVoice
\end{spec}

\vspace{.1in}\hrule

\begin{exercise}{\em
The last definition of |hList| still has an argument |d| on the
left-hand side, and one occurence of |d| on the right-hand side.  Is
there some way to eliminate it using currying simplification?  (Hint:
the answer is yes, but the solution is a bit perverse, and is not
recommended as a way to write your code!)}
\end{exercise}

\out{
hList d = line . map (hNote d)
        = line . ((map . hNote) d)
        = (.) line ((map . hNote) d)
        = ((.) line . (map . hNote)) d

hList = (.) line . (map . hNote)
}

\begin{exercise}{\em
Use |line|, |map| and |($)| to give a concise definition of |addDur|.}
\end{exercise} % $

\begin{exercise}{\em
Rewrite this example:
\begin{spec}
map (\x-> (x+1)/2) xs
\end{spec}
using a composition of sections.}
\end{exercise} 

\begin{exercise}{\em
Consider the expression:
\begin{spec}
map f (map g xs)
\end{spec}
Rewrite this using function composition and a single call to |map|.
Then rewrite the earlier example:
\begin{spec}
map (\x-> (x+1)/2) xs
\end{spec}
as a ``map of a map'' (i.e.\ using two maps).}
\end{exercise}
\index{function!composition||)}

\begin{exercise}{\em
Go back to any exercises prior to this chapter, and simplify your
solutions using ideas learned here.}
\end{exercise} 

\begin{exercise}{\em
Using higher-order functions introduced in this chapter, fill in the
two missing functions, |f1| and |f2|, in the evaluation below so that
it is valid:
\begin{spec}
f1 (f2 (*) [1, 2, 3, 4]) 5 ==>  [5, 10, 15, 20]
\end{spec}
}
\end{exercise}

\vspace{.1in}\hrule

