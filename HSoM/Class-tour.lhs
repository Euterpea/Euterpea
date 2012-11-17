%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Haskell's Standard Type Classes}
\label{ch:class-tour}

This provides a ``tour'' through the predefined standard type classes
in Haskell, as was done for lists in Chapter \ref{ch:list-tour}.  We
have simplified these classes somewhat by omitting some of the less
interesting methods; the Haskell Report and Standard Library Report
contain more complete descriptions.

% Haskell's standard classes form the somewhat imposing inclusion
% structure shown in Figure \ref{tut-classes-figure}.  At the top of the
% figure, we see \hs{Eq} with its subclass \hs{Ord} below it.  These were
% defined in the previous section.

\section{The Ordered Class}
\label{sec:ord-class}

The equality class \hs{Eq} was defined precisely in Chapter
\ref{ch:qualified-types}, along with a simplified version of the class
\indexwdhs{Ord}.  Here is its full specification of class \hs{Ord};
note the many default methods.  \indexhs{max} \indexhs{min}
\indexhs{compare} \indexhs{(<=)} \indexhs{(<)} \indexhs{(>=)} \indexhs{(>)}
\begin{spec}
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

    max x y 
         | x >= y    =  x
         | otherwise =  y
    min x y
         | x <  y    =  x
         | otherwise =  y
data  Ordering  =  LT | EQ | GT
      deriving (Eq, Ord, Enum, Read, Show, Bounded)
\end{spec}

Note that the default method for \hs{compare} is defined in terms of
\hs{(<=)}, and that the default method for \hs{(<=)} is defined in
terms of \hs{compare}.  This means that an instance of \hs{Ord} should
contain a method for at least one of these for everything to be well
defined.  (Using \hs{compare} can be more efficient for complex
types.)  This is a common idea in designing a type class.

\section{The Enumeration Class}
\label{sec:enum-class}

Class \indexwdhs{Enum} has a set of operations that underlie the syntactic
sugar of {\em arithmetic sequences}; for example, the arithmetic
sequence \hs{[1,3..]} is actually shorthand for \hs{enumFromThen 1 3}.
If this is true, then we should be able to generate arithmetic
sequences for any type that is an instance of \hs{Enum}.  This
includes not only most numeric types, but also \hs{Char}, so that, for
instance, \hs{['a'..'z']} denotes the list of lower-case letters in
alphabetical order.  Furthermore, a user-defined enumerated type such
as \hs{Color}:
\begin{spec}
data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
\end{spec}
can easily be given an \hs{Enum} instance declaration, after which we
can calculate the following results:
\begin{spec}
[Red .. Violet]   ===>  [  Red, Orange, Yellow, Green, 
                           Blue, Indigo, Violet]
[Red, Yellow ..]  ===>  [  Red, Yellow, Blue, Violet]
fromEnum Green    ===>  3
toEnum 5 :: Color ===>  Indigo
\end{spec}
Indeed, the derived instance will give this result.  Note that the
sequences are still {\em arithmetic} in the sense that the increment
between values is constant, even though the values are not numbers.

The complete definition of the \hs{Enum} class is given below:

\newpage
\indexhs{toEnum}
\indexhs{fromEnum}
\indexhs{enumFrom}
\indexhs{enumFromThen}
\indexhs{enumFromTo}
\indexhs{enumFromThenTo}
\begin{spec}
class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

    -- Minimal complete definition: toEnum, fromEnum
    succ              =  toEnum . (+1) . fromEnum
    pred              =  toEnum . (subtract 1) . fromEnum
    enumFrom x        =  map toEnum [fromEnum x ..]
    enumFromThen x y  =  map toEnum [fromEnum x, fromEnum y .. ]
    enumFromTo x y    =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x y z = 
        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
\end{spec}
The six default methods are sufficient for most applications, so when
writing your own instance declaration it is usually sufficient to only
provide methods for the remaining two operations: \hs{toEnum} and
\hs{fromEnum}.

In terms of arithmetic sequences, the expressions on the left below
are equivalent to those on the right:

\begin{tabular}{rl}
\hs{enumFrom n}            & \hs{[n..]}    \\
\hs{enumFromThen n n'}     & \hs{[n,n'..]} \\
\hs{enumFromTo n m}        & \hs{[n..m]}   \\
\hs{enumFromThenTo n n' m} & \hs{[n,n'..m]}\\
\end{tabular}

\section{The Bounded Class}
\label{sec:bounded-class}

The class \hs{Bounded} captures data types that are linearly bounded
in some way; i.e.\ they have both a minimum value and a maximum value.
\indexhs{Bounded}
\indexhs{minBound}
\indexhs{maxBound}
\begin{spec}
class  Bounded a  where
    minBound         :: a
    maxBound         :: a
\end{spec}
\section{The Show Class}
\label{sec:show-class}

Instances of the class \indexwdhs{Show} are those types that can be
converted to character strings.  This is useful, for example, when
writing a representation of a value to the standard output area or to
a file.  The class \indexwdhs{Read} works in the other direction: it
provides operations for parsing character strings to obtain the values
that they represent.  In this section we will look at the \hs{Show}
class; in the next we will look at \hs{Read}.

For efficiency reasons the primitive operations in these classes are
somewhat esoteric, but they provide good lessons in both algorithm and
software design, so we will look at them in some detail.

First, let's look at one of the higher-level functions that is defined
in terms of the lower-level primitives:
\begin{spec}
show :: (Show a) => a -> String
\end{spec}
Naturally enough, \indexwdhs{show} takes a value of any type that is a
member of \hs{Show}, and returns its representation as a string.  For
example, \hs{show (2+2)} yields the string \hs{"4"}, as does 
\hs{show (6-2)} and \hs{show} applied to any other expression whose
value is \hs{4}.

Furthermore, we can construct strings such as:
\begin{spec}
"The sum of " ++ show x ++ " and " ++ show y ++ " is " 
     ++ show (x+y) ++ "."
\end{spec}
with no difficulty.  In particular, because \hs{(++)} is right
associative, the number of steps to construct this string is directly
proportional to its total length, and we can't expect to do any better
than that.  (Since \hs{(++)} needs to reconstruct its left argument,
if it were left associative the above expression would repeatedly
reconstruct the same sub-string on each application of \hs{(++)}.  If
the total string length were $n$, then in the worst case the number of
steps needed to do this would be proportional to $n^2$, instead of
proportional to $n$ in the case where \hs{(++)} is right associative.)

Unfortunately, this strategy breaks down when construction of the list
is nested.  A particularly nasty version of this problem arises for
tree-shaped data structures.  Consider a function \hs{showTree} that
converts a value of type \hs{Tree} into a string, as in:
\begin{spec}
showTree (Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 4))
===> "< <2|3>|4>"
\end{spec}
We can define this behavior straightforwardly as follows:
\begin{spec}
showTree             :: (Show a) => Tree a -> String
showTree (Leaf x)     
         =  show x
showTree (Branch l r) 
         = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"
\end{spec}
Each of the recursive calls to \hs{showTree} introduces more
applications of \hs{(++)}, but since they are nested, a large amount
of list reconstruction takes place (similar to the problem that would
arise if \hs{(++)} were left associative).  If the tree being
converted has size $n$, then in the worst case the number of steps
needed to perform this conversion is proportional to $n^2$.  This is
no good!

To restore linear complexity, suppose we had a function
\indexwdhs{shows}:
\begin{spec}
shows :: (Show a) => a -> String -> String
\end{spec}
which takes a showable value and a string and returns that string with
the value's representation concatenated at the front.  For example, we
would expect \hs{shows (2+2) "hello"} to return the string
\hs{"4hello"}.  The string argument should be thought of as an
``\indexwd{accumulator}'' for the final result.

Using \hs{shows} we can define a more efficient version of
\hs{showTree} which, like \hs{shows}, has a string accumulator
argument.  Let's call this function \hs{showsTree}:
\begin{spec}
showsTree               :: (Show a) => Tree a -> String -> String
showsTree (Leaf x) s     
     = shows x s
showsTree (Branch l r) s 
     = "<" ++ showsTree l ("|" ++ showsTree r (">" ++ s))
\end{spec}
This function requires a number of steps directly proportional to the
size of the tree, thus solving our efficiency problem.  To see why
this is so, note that the accumulator argument \hs{s} is never
reconstructed.  It is simply passed as an argument in one recursive
call to \hs{shows} or \hs{showsTree}, and is incrementally extended to
its left using \hs{(++)}.

\hs{showTree} can now be re-defined in terms of \hs{showsTree} using an
empty accumulator:
\begin{spec}
showTree t  =  showsTree t ""
\end{spec}

\begin{exercise}\em
Prove that this version of \hs{showTree} is equivalent to the old.
\end{exercise}

Although this solves our efficiency problem, the presentation of this
function (and others like it) can be improved somewhat.  First, let's
create a type synonym (part of the Standard Prelude):
\indexhs{ShowS}
\begin{spec}
type ShowS              =  String -> String
\end{spec}

% This is the type of a function that returns a string representation of
% something followed by an accumulator string.  

Second, we can avoid carrying accumulators around, and also avoid
amassing parentheses at the right end of long sequences of
concatenations, by using functional composition:
\begin{spec}
showsTree               :: (Show a) => Tree a -> ShowS
showsTree (Leaf x)      
     =  shows x
showsTree (Branch l r)  
     =  ("<"++) . showsTree l . ("|"++) . showsTree r . (">"++)
\end{spec}

\syn{This can be simplified slightly more by noting that \hs{("c"++)}
is equivalent to \hs{('c':)} for any character \hs{c}.}

Something more important than just tidying up the code has come about
by this transformation: We have raised the presentation from an {\em
object level} (in this case, strings) to a {\em function level}.  You
can read the type signature of \hs{showsTree} as saying that
\hs{showsTree} maps a tree into a {\em showing function}.  Functions
like \hs{("<"++)} and \hs{("a string" ++)} are primitive showing
functions, and we build up more complex ones by function composition.

The actual \hs{Show} class in Haskell has two additional levels of
complexity (and functionality): (1) the ability to specify the {\em
precedence} of a string being generated, which is important when
\hs{show}ing a data type that has infix constructors, since it
determines when parentheses are needed, and (2) a function for
\hs{show}ing a {\em list} of values of the type under consideration,
since lists have special syntax in Haskell and are so commonly used
that they deserve special treatment.  The full definition of the
\hs{Show} class is given by:
\indexhs{showsPrec}
\indexhs{showList}
\begin{spec}
class Show a where
   showsPrec :: Int -> a -> ShowS
   showList  :: [a] -> ShowS

   showList []      = showString "[]"
   showList (x:xs)  = showChar '[' . shows x . showl xs
      where  showl []      = showChar ']'
             showl (x:xs)  = showString ", " . shows x . showl xs
\end{spec}
Note the default method for \hs{showList}, and its ``function level''
style of definition.  

In addition to this class declaration the Standard Prelude defines the
following functions, which return us to where we started our journey
in this section:
\indexhs{shows}
\indexhs{show}
\begin{spec}
shows :: (Show a) => a -> ShowS
shows  =  showsPrec 0

show   :: (Show a) => a -> String
show x  =  shows x ""
\end{spec}

Some details about \hs{showsPrec} can be found in the Haskell Report,
but if you are not displaying constructors in infix notation, the
precedence can be ignored.  Furthermore, the default method for
\hs{showList} is perfectly good for most uses of lists that you will
encounter.  Thus, for example, we can finish our \hs{Tree} example by
declaring it to be an instance of the class \hs{Show} very simply as:
\begin{spec}
instance (Show a) => Show (Tree a) where
    showsPrec n = showsTree
\end{spec}

\section{The Read Class}
\label{sec:read-class}
\indexhs{Read}

Now that we can convert trees into strings, let's turn to the inverse
problem: converting strings into trees.  The basic idea is to define a
{\em \indexwd{parser}} for a type \hs{a}, which at first glance seems as if it
should be a function of type \hs{String -> a}.  This simple approach
has two problems, however: (1) it's possible that the string is
ambiguous, leading to more than one way to interpret it as a value of
type \hs{a}, and (2) it's possible that only a prefix of the string
will parse correctly.  Thus we choose instead to return a list of
\hs{(a, String)} pairs as the result of a parse.  If all goes well we
will always get a singleton list such as \hs{[(v,"")]} as the result
of a parse, but we cannot count on it (in fact, when recursively
parsing sub-strings, we will expect a singleton list with a {\em
non-empty} trailing string).

The Standard Prelude provides a type synonym for parsers of the kind
just described:
\indexhs{ReadS}
\begin{spec}
type ReadS a            =  String -> [(a,String)]
\end{spec}
and also defines a function \indexwdhs{reads} that by analogy is
similar to \hs{shows}:
\begin{spec}
reads :: (Read a) => ReadS a
\end{spec}
We will return later to the precise definition of this function, but
for now let's use it to define a parser for the \hs{Tree} data type,
whose string representation is as described in the previous section.
List comprehensions give us a convenient idiom for constructing such
parsers:\footnote{An even more elegant approach to parsing uses monads
and parser combinators.  These are part of a standard parsing library
distributed with most Haskell systems.}
\begin{spec}
readsTree        :: (Read a) => ReadS (Tree a)
readsTree ('<':s) =  [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                        (r, '>':u) <- readsTree t ]
readsTree s       =  [(Leaf x, t)     | (x,t)      <- reads s]
\end{spec}
Let's take a moment to examine this function definition in detail.
There are two main cases to consider: If the string has the form
\hs{'<':s} we should have the representation of a branch, in which
case parsing \hs{s} as a tree should yield a left branch \hs{l}
followed by a string of the form \hs{'|':t}; parsing \hs{t} as a tree
should then yield the right branch \hs{r} followed by a string of the
form \hs{'>':u}.  The resulting tree \hs{Branch l r} is then returned,
along with the trailing string \hs{u}.  Note the expressive power we
get from the combination of pattern matching and list comprehension.

If the initial string is not of the form \hs{'<':s}, then we must
have a leaf, in which case the string is parsed using the generic
\hs{reads} function, and the result is directly returned.

If we accept on faith for the moment that there is a \hs{Read}
instance for \hs{Int} that behaves as one would expect, e.g.:
\begin{spec}
(reads "5 golden rings") :: [(Int,String)]
===> [(5, " golden rings")]
\end{spec}
then you should be able to verify the following calculations:
\begin{spec}
readsTree "< <1|2>|3>"
===>  [(Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 1)), "")]

readsTree "<1|2"  ===>  []
\end{spec}
There are a couple of shortcomings, however, in our definition of
\hs{readsTree}.  One is that the parser is quite rigid in that it
allows no ``white space'' (such as extra spaces, tabs, or line feeds)
before or between the elements of the tree representation.  The other
is that the way we parse our punctuation symbols (\hs{'<'}, \hs{'|'},
and \hs{'>'}) is quite different from the way we parse leaf values and
sub-trees.  This lack of uniformity makes the function definition
harder to read.

We can address both of these problems by using a {\em lexical
analyzer}, which parses a string into primitive ``lexemes" defined by
some rules about the string construction.  The Standard Prelude
defines a lexical analyzer:
\indexhs{lex}
\begin{spec}
lex :: ReadS String
\end{spec}
whose lexical rules are those of the Haskell language, which can be
found in the Haskell Report.  For our purposes, an informal
explanation is sufficient:

\hs{lex} normally returns a singleton list containing a pair of
strings: the first string is the first lexeme in the input string, and
the second string is the remainder of the input.  White space --
including Haskell comments -- is completely ignored.  If the input
string is empty or contains only white-space and comments, \hs{lex}
returns \hs{[("","")]}; if the input is not empty in this sense, but
also does not begin with a valid lexeme after any leading white-space,
\hs{lex} returns \hs{[]}.

Using this lexical analyzer, our tree parser can be rewritten as:
\begin{spec}
readsTree   :: (Read a) => ReadS (Tree a)
readsTree s  =  [(Branch l r, x) | ("<", t) <- lex s,
                                   (l,   u) <- readsTree t,
                                   ("|", v) <- lex u,
                                   (r,   w) <- readsTree v,
                                   (">", x) <- lex w        ]
                ++
                [(Leaf x, t)     | (x,   t) <- reads s      ]
\end{spec}
This definition solves both problems mentioned earlier: white-space is
suitably ignored, and parsing of sub-strings has a more uniform
structure.

To tie all of this together, let's first look at the definition of the
class \hs{Read} in the Standard Prelude:
\indexhs{readsPrec}
\indexhs{readList}
\indexhs{readParen}
\begin{spec}
class  Read a  where
   readsPrec  :: Int -> ReadS a
   readList   :: ReadS [a]

   readList   = readParen False (\r -> [pr |  ("[",s)  <- lex r,
                                              pr       <- readl s])
                   where  readl  s =  [([],t)   | ("]",t)  <- lex s] ++
                                      [(x:xs,u) |  (x,t)    <- reads s,
                                                   (xs,u)   <- readl' t]
                          readl' s =  [([],t)   | ("]",t)  <- lex s] ++
                                      [(x:xs,v) |  (",",t)  <- lex s,
                                                   (x,u)    <- reads t,
                                                   (xs,v)   <- readl' u]

readParen      :: Bool -> ReadS a -> ReadS a
readParen b g  =  if b then mandatory else optional
                  where  optional r  = g r ++ mandatory r
                         mandatory r = [(x,u) |  ("(",s) <- lex r,
                         sc                        (x,t)   <- optional s,
                                                 (")",u) <- lex t ]
\end{spec}
The default method for \hs{readList} is rather tedious, but otherwise
straightforward.
\indexhs{reads}
\indexhs{read}

\hs{reads} can now be defined, along with an even higher-level
function, \hs{read}:
\begin{spec}
reads :: (Read a) => ReadS a
reads  =  readsPrec 0

read   :: (Read a) => String -> a
read s  =  case [x | (x,t) <- reads s, ("","") <- lex t] of
             [x] -> x
             []  -> error "PreludeText.read: no parse"
             _   -> error "PreludeText.read: ambiguous parse"
\end{spec}
The definition of \hs{reads} (like \hs{shows}) should not be
surprising.  The definition of \hs{read} assumes that exactly one
parse is expected, and thus causes a run-time error if there is no
unique parse or if the input contains anything more than a
representation of exactly one value of type \hs{a} (and possibly
comments and white-space).

You can test that the \hs{Read} and \hs{Show} instances for a
particular type are working correctly by applying \hs{(read . show)}
to a value in that type, which in most situations should be the
identity function.

\section{The Index Class}
\label{sec:index-class}
\indexhs{Ix}
\indexhs{range}
\indexhs{index}
\indexhs{inRange}

The Standard Prelude defines a type class of array indices:
\begin{spec}
class  (Ord a) => Ix a  where
    range       :: (a,a) -> [a]
    index       :: (a,a) -> a -> Int
    inRange     :: (a,a) -> a -> Bool
\end{spec}
Arrays are defined elsewhere, but the index class is useful for other
things besides arrays, so I will describe it here.

Instance declarations are provided for \hs{Int}, \hs{Integer},
\hs{Char}, \hs{Bool}, and tuples of \hs{Ix} types; in addition,
instances may be automatically derived for enumerated and tuple types.
You should think of the primitive types as vector indices, and tuple
types as indices of multidimensional rectangular arrays.  Note that
the first argument of each of the operations of class \hs{Ix} is a
pair of indices; these are typically the {\em bounds} (first and last
indices) of an array.  For example, the bounds of a 10-element,
zero-origin vector with \hs{Int} indices would be \hs{(0,9)}, while a
100 by 100 1-origin matrix might have the bounds
\hs{((1,1),(100,100))}.  (In many other languages, such bounds would
be written in a form like \hs{1:100, 1:100}, but the present form fits
the type system better, since each bound is of the same type as a
general index.)

The \hs{range} operation takes a bounds pair and produces the list of
indices lying between those bounds, in index order.  For example,
\begin{spec}
range (0,4) ===> [0,1,2,3,4]
range ((0,0),(1,2)) ===> [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]
\end{spec}
The \hs{inRange} predicate determines whether an index lies between a
given pair of bounds.  (For a tuple type, this test is performed
componentwise, and then combined with \hs{(&&)}.)  Finally, the
\hs{index} operation determines the (zero-based) position of an index
within a bounded range; for example:
\begin{spec}
index (1,9) 2  ===>  1
index ((0,0),(1,2)) (1,1)  ===>  4
\end{spec}
\section{The Numeric Classes}
\label{sec:numeric-classes}

The \indexwdhs{Num} class and the numeric class hierarchy were briefly
described in Section \ref{sec:standard-type-classes}.  Figure
\ref{fig:numeric-class-decls} gives the full class declarations.

\begin{figure}
\begin{spec}
class  (Eq a, Show a) => Num a  where
    (+), (-), (*) :: a -> a -> a
    negate :: a -> a
    abs, signum :: a -> a
    fromInteger :: Integer -> a

class  (Num a, Ord a) => Real a  where
    toRational ::  a -> Rational

class  (Real a, Enum a) => Integral a  where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod :: a -> a -> (a,a)
    toInteger :: a -> Integer

class  (Num a) => Fractional a  where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a

class  (Fractional a) => Floating a  where
    pi :: a
    exp, log, sqrt :: a -> a
    (**), logBase :: a -> a -> a
    sin, cos, tan :: a -> a
    asin, acos, atan :: a -> a
    sinh, cosh, tanh :: a -> a
    asinh, acosh, atanh :: a -> a

class  (Real a, Fractional a) => RealFrac a  where
    properFraction :: (Integral b) => a -> (b,a)
    truncate, round :: (Integral b) => a -> b
    ceiling, floor :: (Integral b) => a -> b

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix :: a -> Integer
    floatDigits :: a -> Int
    floatRange :: a -> (Int,Int)
    decodeFloat :: a -> (Integer,Int)
    encodeFloat :: Integer -> Int -> a
    exponent :: a -> Int
    significand :: a -> a
    scaleFloat :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE 
                        :: a -> Bool
\end{spec}
\caption{Standard Numeric Classes}
\label{fig:numeric-class-decls}
\end{figure}
