%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{The PreludeList Module}
\label{ch:list-tour}

The use of lists is particularly common when programming in Haskell,
and thus, not surprisingly, there are many pre-defined polymorphic
functions for lists.  The list data type itself, plus some of the most
useful functions on it, are contained in the Standard Prelude's
\hs{PreludeList} module, which we will look at in detail in this
chapter.  There is also a Standard Library module called \hs{List}
that has additional useful functions.  It is a good idea to become
familiar with both modules.  \indexamb{List}{library}
\indexhs{PreludeList}

Although this chapter may feel like a long list of ``Haskell
features,'' the functions described here capture many common patterns
of list usage that have been discovered by functional programmers over
many years of trials and tribulations.  In many ways higher-order
declarative programming with lists takes the place of lower-level
imperative control structures in more conventional languages.  By
becoming familiar with these list functions you will be able to more
quickly and confidently develop your own applications using lists.
Furthermore, if all of us do this, we will have a common vocabulary
with which to understand each others' programs.  Finally, by reading
through the code in this module you will develop a good feel for how
to write proper function definitions in Haskell.

It is not necessary for you to understand the details of every
function, but you should try to get a sense for what is available so
that you can return later when your programming needs demand it.  In
the long run you are well-advised to read the rest of the Standard
Prelude as well as the various Standard Libraries, to discover a host
of other functions and data types that you might someday find useful
in your own work.

\section{The PreludeList Module}

To get a feel for the \hs{PreludeList} module, let's first look at its
module declaration:
\begin{spec}
module PreludeList (
    map, (++), filter, concat,
    head, last, tail, init, null, length, (!!), 
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3)
  where

import qualified Char(isSpace)

infixl 9  !!
infixr 5  ++
infix  4  `elem`, `notElem`
\end{spec}
We will not discuss all of the functions listed above, but will cover
most of them (and some were discussed in previous chapters).

\section{Simple List Selector Functions}
\label{sec:list-selectors}

\indexwdhs{head} and \indexwdhs{tail} extract the first element and remaining
elements, respectively, from a list, which must be non-empty.
\indexwdhs{last} and \indexwdhs{init} are the dual functions that work from the end
of a list, rather than from the beginning.
\begin{spec}
head             :: [a] -> a
head (x:_)       =  x
head []          =  error "PreludeList.head: empty list"

last             :: [a] -> a
last [x]         =  x
last (_:xs)      =  last xs
last []          =  error "PreludeList.last: empty list"

tail             :: [a] -> [a]
tail (_:xs)      =  xs
tail []          =  error "PreludeList.tail: empty list"

init             :: [a] -> [a]
init [x]         =  []
init (x:xs)      =  x : init xs
init []          =  error "PreludeList.init: empty list"
\end{spec}
Although \hs{head} and \hs{tail} were previously discussed in Section
\ref{sec:poly-types}, the definitions here include an equation
describing their behaviors under erroneous situations---such as
selecting the head of an empty list---in which case the \hs{error}
function is called.  It is a good idea to include such an equation for
any definition in which you have not covered every possible case in
pattern-matching; i.e.\ if it is possible that the pattern-matching
could ``run off the end'' of the set of equations.  The string
argument that you supply to the \hs{error} function should be detailed
enough that you can easily track down the precise location of the
error in your program.

\syn{If such an error equation is omitted, and then during
pattern-matching all equations fail, most Haskell systems will invoke
the \indexwdhs{error} function anyway, but most likely with a string that
will be less informative than one you can supply on your own.}

The \indexwdhs{null} function tests to see if a list is empty.
\begin{spec}
null             :: [a] -> Bool
null []          =  True
null (_:_)       =  False
\end{spec}

\section{Index-Based Selector Functions}
\label{sec:list-index-fns}

To select the $n$th element from a list, with the first element being
the $0$th element, we can use the indexing function \hs{(!!)}:
\index{list!indexing}
\begin{spec}
(!!)                :: [a] -> Int -> a
(x:_)  !! 0         =  x
(_:xs) !! n | n > 0 =  xs !! (n-1)
(_:_)  !! _         =  error "PreludeList.!!: negative index"
[]     !! _         =  error "PreludeList.!!: index too large"
\end{spec}
\syn{Note the definition of two error conditions; be sure that you
understand under what conditions these two equations would succeed.
In particular, recall that equations are matched in top-down order:
the first to match is the one that is chosen.}

\hs{take n xs} returns the prefix of \hs{xs} of length \hs{n}, or
\hs{xs} itself if \hs{n > length xs}.  Similarly, \hs{drop n xs} returns
the suffix of \hs{xs} after the first \hs{n} elements, or \hs{[]} if
\hs{n > length xs}.  Finally, \hs{splitAt n xs} is equivalent to
\hs{(take n xs, drop n xs)}.
\indexhs{take}
\indexhs{drop}
\indexhs{splitAt}
\indexhs{length}
\begin{spec}
take                   :: Int -> [a] -> [a]
take 0 _               =  []
take _ []              =  []
take n (x:xs) | n > 0  =  x : take (n-1) xs
take _ _               =  
     error "PreludeList.take: negative argument"

drop                   :: Int -> [a] -> [a]
drop 0 xs              =  xs
drop _ []              =  []
drop n (_:xs) | n > 0  =  drop (n-1) xs
drop _     _           =  
     error "PreludeList.drop: negative argument"

splitAt                   :: Int -> [a] -> ([a],[a])
splitAt 0 xs              =  ([],xs)
splitAt _ []              =  ([],[])
splitAt n (x:xs) | n > 0  =  (x:xs',xs'') 
                             where (xs',xs'') = splitAt (n-1) xs
splitAt _     _          =  
     error "PreludeList.splitAt: negative argument"

length           :: [a] -> Int
length []        =  0
length (_:l)     =  1 + length l
\end{spec}
For example:
\begin{spec}
take    3 [0, 1 .. 5] ==> [0,1,2]
drop    3 [0, 1 .. 5] ==> [3,4,5]
splitAt 3 [0, 1 .. 5] ==> ([0,1,2],[3,4,5])
\end{spec}

\section{Predicate-Based Selector Functions}
\label{sec:list-pred-fns}
\indexhs{takeWhile}
\indexhs{dropWhile}
\indexhs{span}
\indexhs{break}

\hs{takeWhile p xs} returns the longest (possibly empty) prefix of
\hs{xs}, all of whose elements satisfy the predicate \hs{p}.
\hs{dropWhile p xs} returns the remaining suffix.  Finally,
\hs{span p xs} is equivalent to \hs{(takeWhile p xs, dropWhile p xs)},
while \hs{break p} uses the negation of \hs{p}.
\begin{spec}
takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile p []          =  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile p []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span p []               =  ([],[])
span p xs@(x:xs') 
            | p x       =  (x:xs',xs'') where (xs',xs'') = span p xs
            | otherwise =  (xs,[])

break p                 =  span (not . p)
\end{spec}
\indexwdhs{filter} removes all elements not satisfying a predicate:
\begin{spec}
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs
\end{spec}

\section{Fold-like Functions}

\indexwdhs{foldl1} and \indexwdhs{foldr1} are variants of
\indexwdhs{foldl} and \indexwdhs{foldr} that have no starting value
argument, and thus must be applied to non-empty lists.
\begin{spec}
foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "PreludeList.foldl1: empty list"

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 _ []      =  error "PreludeList.foldr1: empty list"
\end{spec} 
\hs{foldl1} and \hs{foldr1} are best used in cases where an empty list
makes no sense for the application.  For example, computing the
maximum or mimimum element of a list does not make sense if the list
is empty.  Thus \hs{foldl1 max} is a proper function to compute the
maximum element of a list.

\hs{scanl} is similar to \hs{foldl}, but returns a list of successive
reduced values from the left:
\begin{spec}
scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
\end{spec}
For example:
\begin{spec}
scanl (+) 0 [1,2,3]  ==>  [0,1,3,6]
\end{spec}
Note that \hs{last (scanl f z xs) = foldl f z xs}.  \hs{scanl1} is
similar, but without the starting element:
\begin{spec}
scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
\end{spec}
Here are the full definitions:
\indexhs{scanl}
\indexhs{scanl1}
\indexhs{scanr}
\indexhs{scanr1}
\begin{spec}
scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs     =  q : (case xs of
                            []   -> []
                            x:xs -> scanl f (f q x) xs)
scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)  =  scanl f x xs
scanl1 _ []      =  error "PreludeList.scanl1: empty list"

scanr             :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     =  [q0]
scanr f q0 (x:xs) =  f x q : qs
                     where qs@(q:_) = scanr f q0 xs 

scanr1           :: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]    =  [x]
scanr1 f  (x:xs) =  f x q : qs
                    where qs@(q:_) = scanr1 f xs 
scanr1 _ []      =  error "PreludeList.scanr1: empty list"
\end{spec}

\section{List Generators}
\label{sec:list-generators}

There are some functions which are very useful for generating lists
from scratch in interesting ways.  To start, \hs{iterate f x} returns
an {\em infinite list} of repeated applications of \hs{f} to \hs{x}.
That is: \indexhs{iterate}
\begin{spec}
iterate f x  ==>  [x, f x, f (f x), ...]
\end{spec}
The ``infinite'' nature of this list may at first seem alarming, but
in fact is one of the more powerful and useful features of Haskell.

[say more]
\begin{spec}
iterate          :: (a -> a) -> a -> [a]
iterate f x      =  x : iterate f (f x)
\end{spec}
\hs{repeat x} is an infinite list, with {x} the value of every
element.  \hs{replicate n x} is a list of length \hs{n} with \hs{x}
the value of every element.  And \hs{cycle} ties a finite list into a
circular one, or equivalently, the infinite repetition of the original
list.
\indexhs{repeat}
\indexhs{replicate}
\indexhs{cycle}
\begin{spec}
repeat           :: a -> [a]
repeat x         =  xs where xs = x:xs

replicate        :: Int -> a -> [a]
replicate n x    =  take n (repeat x)

cycle            :: [a] -> [a]
cycle []         = error "Prelude.cycle: empty list" 
cycle xs         =  xs' where xs' = xs ++ xs'
\end{spec}     

\section{String-Based Functions}
\label{sec:list-string-fns}

Recall that strings in Haskell are just lists of characters.
Manipulating strings (i.e.\ text) is a very common practice, so it
makes sense that Haskell would have a few pre-defined functions to
make this easier for you.

\indexwdhs{lines} breaks a string at every newline character (written
as \hs{'\n'} in Haskell), thus yielding a {\em list} of strings, each
of which contains no newline characters.  Similary, \indexwdhs{words}
breaks a string up into a list of words, which were delimited by white
space.  Finally, \indexwdhs{unlines} and \indexwdhs{unwords} are the
inverse operations: \hs{unlines} joins lines with terminating newline
characters, and \hs{unwords} joins words with separating spaces.
(Because of the potential presence of multiple spaces and newline
characters, however, these pairs of functions are not true inverses of
each other.)
\begin{spec}
lines            :: String -> [String]
lines ""         =  []
lines s          =  let (l, s') = break (== '\n') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> lines s''

words            :: String -> [String]
words s          =  case dropWhile Char.isSpace s of
                      "" -> []
                      s' -> w : words s''
                            where (w, s'') = break Char.isSpace s'

unlines          :: [String] -> String
unlines          =  concatMap (++ "\n")

unwords          :: [String] -> String
unwords []       =  ""
unwords ws       =  foldr1 (\w s -> w ++ ' ':s) ws
\end{spec}

\indexwdhs{reverse} reverses the elements in a finite list.
\begin{spec}
reverse          :: [a] -[a]
reverse          =  foldl (flip (:)) []
\end{spec}

\section{Boolean List Functions}
\label{sec:list-boolean-fns}

\indexwdhs{and} and \indexwdhs{or} compute the logical ``and'' and
``or,'' respectively, of all of the elements in a list of Boolean
values.
\begin{spec}
and, or          :: [Bool] -> Bool
and              =  foldr (&&) True
or               =  foldr (||) False
\end{spec}
Applied to a predicate and a list, \indexwdhs{any} determines if any
element of the list satisfies the predicate.  An analogous behavior
holds for \indexwdhs{all}.
\begin{spec}
any, all         :: (a -> Bool) -> [a] -> Bool
any p            =  or . map p
all p            =  and . map p
\end{spec}

\section{List Membership Functions}

\indexwdhs{elem} is the list membership predicate, usually written in
infix form, e.g., \hs{x `elem` xs} (which is why it was given a fixity
declaration at the beginning of the module).  \indexwdhs{notElem} is
the negation of this function.
\begin{spec}
elem, notElem    :: (Eq a) => a -> [a] -> Bool
elem x           =  any (== x)
notElem x        =  all (/= x)
\end{spec}
It is common to store ``key/value'' pairs in a list, and to access the
list by finding the value associated with a given key (for this reason
the list is often called an {\em association list}).  The function
\indexwdhs{lookup} looks up a key in an association list, returning
\hs{Nothing} if it is not found, or \hs{Just y} if \hs{y} is the
value associated with the key.
\begin{spec}
lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []    =  Nothing
lookup key ((x,y):xys)
    | key == x   =  Just y
    | otherwise  =  lookup key xys
\end{spec}

\section{Arithmetic on Lists}

\indexwdhs{sum} and \indexwdhs{product} compute the sum and product,
respectively, of a finite list of numbers.
\begin{spec}
sum, product     :: (Num a) => [a] -> a
sum              =  foldl (+) 0  
product          =  foldl (*) 1
\end{spec}
\indexwdhs{maximum} and \indexwdhs{minimum} return the maximum and
minimum value, respectively from a non-empty, finite list whose
element type is ordered.
\begin{spec}
maximum, minimum :: (Ord a) => [a] -> a
maximum []       =  error "Prelude.maximum: empty list"
maximum xs       =  foldl1 max xs

minimum []       =  error "Prelude.minimum: empty list"
minimum xs       =  foldl1 min xs
\end{spec}
Note that even though \hs{foldl1} is used in the definition, a test is
made for the empty list to give an error message that more accurately
reflects the source of the problem.

\section{List Combining Functions}
\label{sec:list-combining-fns}

\hs{map} and \hs{(++)} were defined in previous chapters, but
are repeated here for completeness: \indexhs{map} \indexhs{(++)}
\begin{spec}
map :: (a -> b) -> [a] -> [a]
map f []     = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
\end{spec}
\indexwdhs{concat} appends together a list of lists:
\begin{spec}
concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
\end{spec}
\indexwdhs{concatMap} does what it says: it concatenates the result of
mapping a function down a list.
\begin{spec}
concatMap        :: (a -> [b]) -> [a] -> [b]
concatMap f      =  concat . map f
\end{spec}
\indexwdhs{zip} takes two lists and returns a list of corresponding
pairs.  If one input list is short, excess elements of the longer list
are discarded.  \indexwdhs{zip3} takes three lists and returns a list
of triples.  (``Zips'' for larger tuples are contained in the List
Library.)
\begin{spec}
zip              :: [a] -> [b] -> [(a,b)]
zip              =  zipWith (,)

zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3             =  zipWith3 (,,)
\end{spec}
\syn{The functions \indexwdhs{(,)} and \indexwdhs{(,,)} are the pairing
and tripling functions, respectively:
\begin{spec}
(,)   ==>  \x y ->   (x,y)
(,,)  ==>  \x y z -> (x,y,z)
\end{spec}
}

The \indexwdhs{zipWith} family generalises the \hs{zip} and \hs{map}
families (or, in a sense, combines them) by applying a function (given
as the first argument) to each pair (or triple, etc.) of values.  For
example, \hs{zipWith (+)} is applied to two lists to produce the list
of corresponding sums.  \indexhs{zipWith3}
\begin{spec}
zipWith          :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)
                 =  z a b : zipWith z as bs
zipWith _ _ _    =  []

zipWith3         :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                 =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ =  []
\end{spec}
The following two functions perform the inverse operations of \hs{zip}
and \hs{zip3}, respectively.
\indexhs{unzip}
\indexhs{unzip3}
\begin{spec}
unzip            :: [(a,b)] -> ([a],[b])
unzip            =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

unzip3           :: [(a,b,c)] -> ([a],[b],[c])
unzip3           =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                          ([],[],[])
\end{spec}

% \begin{exercise}\em
% Write at least two (stylistically different) definitions for a
% factorial function in Haskell.  Include the fact that, pragmatically
% speaking, we would expect an {\em error} to result if the function is
% applied to a negative argument.
% \end{exercise}


