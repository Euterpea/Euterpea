%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Qualified Types and Type Classes}
\label{ch:qualified-types}

This chapter introduces the notions of \emph{qualified types} and
\emph{type classes}.  These concepts can be viewed as a refinement of
the notion of polymorphism, and increase the ability to write modular
programs.

\section{Motivation}
\label{sec:qualified-types}

A polymorphic type such as |(a->a)| can be viewed as shorthand for
$\forall($|a|$)|a->a|$, which can be read ``\emph{for all} types |a|,
functions mapping elements of type |a| to elements of type |a|.''
Note the emphasis on ``\emph{for all}.''

In practice, however, there are times when we would prefer to limit a
polymorphic type to a smaller number of possibilities.  A good example
is a function such as |(+)|.  It is probably not a good idea to limit
|(+)| to a \emph{single} (that is, \emph{monomorphic}) type such as
|Integer->Integer->Integer|, since there are other kinds of
numbers---such as rational and floating-point numbers---that we would
like to perform addition on as well.  Nor is it a good idea to have a
different addition function for each number type, since that would
require giving each a different name, such as |addInteger|,
|addRational|, |addFloat|, etc.  And, unfortunately, giving |(+)| a
type such as |a->a->a| will not work, since this would imply that we
could add things other than numbers, such as characters, pitch
classes, lists, tuples, functions, and any type that we might define
on our own!

\index{type!qualified} \index{class} 

Haskell provides a solution to this problem through the use of {\em
  qualified types}.  Conceptually, it is helpful to think of a
qualified type just as a polymorphic type, except that in place of
``\emph{for all} types |a|'' it will be possible to say ``for all
types |a| \emph{that are members of the type class} |C|,'' where the
type class |C| can be thought of as a set of types.  For example,
suppose there is a type class \indexwdhs{Num} with members |Integer|,
|Rational|, and |Float|.  Then an accurate type for |(+)| would be
$\forall($|a|$\in$|Num|$)$|a -> a -> a|.  But in Haskell, instead of
writing $\forall(|a|\in|Num|)\cdots$, the notation |Num a =>|$\cdots$
is used.  So the proper type signature for |(+)| is:
\begin{spec}
(+) :: Num a => a -> a -> a
\end{spec}
which should be read: ``for all types |a| that are members of the type
class |Num|, |(+)| has type |a -> a -> a|.''  Members of a type class
are also called \emph{instances} of the class, and these two terms
will be used interchangeably in the remainder of the text.  The |Num a
=>|$\cdots$ part of the type signature is often called a
\emph{context}, or \emph{constraint}.

\index{class}
\syn{It is important not to confuse |Num| with a data type or a
constructor within a data type, even though the same syntax 
(``|Num a|'') is used.  |Num| is a \emph{type class}, and the
context of its use (namely, to the left of a |=>|) is always
sufficient to determine this fact.}

Recall now the type signature given for the function |simple| in
Chapter~\ref{ch:intro}:
\begin{spec}
simple         :: Integer -> Integer -> Integer -> Integer
simple x y z   = x*(y+z)
\end{spec}
Note that |simple| uses the operator |(+)| discussed above.  It also
uses |(*)|, whose type is the same as that for |(+)|:
\begin{spec}
(*) :: Num a => a -> a -> a
\end{spec}
This suggests that a more general type for |simple| is:
\begin{spec}
simple         :: Num a => a -> a -> a -> a
simple x y z   = x*(y+z)
\end{spec}
Indeed, this is the preferred, most general type that can be given for
|simple|.  It can now be used with any type that is a member of the
|Num| class, which includes |Integer|, |Int|, |Rational|, |Float| and
|Double|, among others.

The ability to qualify polymorphic types is a unique feature of
Haskell, and, as we will soon see, provides great expressiveness.  In
the following sections the idea is explored much more thoroughly, and
in particular it is shown how a programmer can define his or her own
type classes and their instances.  To begin, let's take a closer look
at one of the pre-defined type classes in Haskell, having to do with
equality.

\section{Equality}
\label{sec:equality}

\index{equality} \emph{Equality} between two expressions |e1| and |e2|
in Haskell means that the value of |e1| is the same as the value of
|e2|.  Another way to view equality is that we should be able to
substitute |e1| for |e2|, or vice versa, wherever they appear in a
program, without affecting the result of that program.

In general, however, it is not possible for a program to determine the
equality of two expressions---consider, for example, determining the
equality of two infinite lists, two infinite |Music| values, or two
functions of type |Integer -> Integer|.\footnote{This is the same as
  determining \emph{program equivalence}, a well-known example of an
  \emph{undecideable problem} in the theory of computation.}  The
ability to compute the equality of two values is called
\emph{computational equality}.  Even though by the above simple
examples it is clear that computational equality is strictly weaker
than full equality, it is still an operation that we would like to
use in many ordinary programs.

\indexhs{(==)} 

Haskell's operator for computational equality is |(==)|.  Partly
because of the problem mentioned above, there are many types for which
we would like equality defined, but some for which it might not make
sense.  For example, it is common to compare two characters, two
integers, two floating-point numbers, etc.\ On the other hand,
comparing the equality of infinite data structures, or functions, is
difficult, and in general not possible.  Thus Haskell has a type class
called |Eq|, so that the equality operator |(==)| can be given the
qualified type: \indexhs{Eq}
\begin{spec}
(==) :: Eq a => a -> a -> Bool
\end{spec}

In other words, |(==)| is a function that, for any type |a| in
the class |Eq|, tests two values of type |a| for equality,
returning a Boolean (|Bool|) value as a result.  Amongst |Eq|'s
instances are the types \indexwdhs{Char} and \indexwdhs{Integer}, so
that the following calculations hold:
\begin{spec}
42   == 42   ==>  True
42   == 43   ==>  False
'a'  == 'a'  ==>  True
'a'  == 'b'  ==>  False
\end{spec}
Furthermore, the expression |42 ==| |'a'| is {\em
\indexwd{ill-typed}}; Haskell is clever enough to know when qualified
types are ill-formed.

One of the nice things about qualified types is that they work in the
presence of ordinary polymorphism.  In particular, the type
constraints can be made to propagate through polymorphic data types.
For example, because |Integer| and |Float| are members of
|Eq|, so are the types |(Integer,Char)|, |[Integer]|,
|[Float]|, etc.  Thus:
\begin{spec}
[42,43]    == [42,43]       ==>  True
[4.2,4.3]  == [4.3,4.2]     ==>  False
(42,'a')   == (42,'a')      ==>  True
\end{spec}
This will be elaborated upon in a later section.

Type constraints also propagate through function definitions.  For
example, consider this definition of the function |`elem`| that
tests for membership in a list:
\begin{spec}
x `elem`  []            = False
x `elem` (y:ys)         = x==y || x `elem` ys
\end{spec}

\syn{|(`elem`)| is actually written |elem| in Haskell; i.e.\ it is a
  normal function, not an infix operator.  Of course it can be used
  in an infix manner (and it often is) by enclosing it in backquotes.}

Note the use of |(==)| on the right-hand side of the second
equation.  The principal type for |(`elem`)| is thus:
\begin{spec}
`elem` :: Eq a => a -> [a] -> Bool
\end{spec}
This should be read, ``For every type |a| that is an instance of the
class |Eq|, |(`elem`)| has type |a->[a]->Bool|.''  This is exactly what
we would hope for---it expresses the fact that |(`elem`)| is not defined
on all types, just those for which computational equality is defined.

The above type for |(`elem`)| is also its principal type, and Haskell
will infer this type if no signature is given.  Indeed, if we were to
write the type signature:
\begin{spec}
(`elem`) :: a -> [a] -> Bool
\end{spec}
a type error would result, because this type is fundamentally
\emph{too general}, and the Haskell type system will complain.

\syn{On the other hand, we could write:
\begin{spec}
(`elem`) :: Integer -> [Integer] -> Bool
\end{spec}
if we expect to use |(`elem`)| only on lists of integers.  In other
words, using a type signature to constrain a value to be less general
than its principal type is Ok.}

As another example of this idea, a function that squares its argument:
\begin{spec}
square x = x*x
\end{spec}
has principal type |Num a => a -> a|, since |(*)|, like
|(+)|, has type \newline
|Num a => a -> a -> a|.  Thus:
\begin{spec}
square 42   ==>  1764
square 4.2  ==>  17.64
\end{spec}
The |Num| class will be discusssed in greater detail shortly.

\section{Defining Our Own Type Classes}
\label{sec:type-class-decls}

Haskell provides a mechanism whereby we can create our own qualified
types, by defining a new type class and specifying which types are
members, or ``instances'' of it.  Indeed, the type classes |Num|
and |Eq| are not built-in as primitives in Haskell, but rather are
simply predefined in the Standard Prelude.

To see how this is done, consider the |Eq| class.  It is created by
the following \emph{type class declaration}: 
\index{class!\hkw{class}}
\index{class!declaration}
\begin{spec}
class Eq a where 
  (==)  :: a -> a -> Bool
\end{spec}
\index{class!operation} 
The connection between |(==)| and |Eq| is important: the above
declaration should be read, ``a type |a| is an instance of the class
|Eq| only if there is an operation |(==)::a->a->Bool| defined on it.''
|(==)| is called an \emph{operation} in the class |Eq|, and in general
more than one operation is allowed in a class.  More examples of this
will be introduced shortly.

\index{class!instance}
So far so good.  But how do we specify which types are instances of
the class |Eq|, and the actual behavior of |(==)| on each of those
types?  This is done with an \emph{instance declaration}.  For example:
\begin{spec}
instance Eq Integer where 
  x == y   =  integerEq x y
\end{spec}
\index{class!method}

The definition of |(==)| is called a \emph{method}.  The function
|integerEq| happens to be the primitive function that compares
integers for equality, but in general any valid expression is allowed
on the right-hand side, just as for any other function definition.
The overall instance declaration is essentially saying: ``The type
|Integer| is an instance of the class |Eq|, and here is the method
corresponding to the operation |(==)|.''  Given this declaration, we
can now compare fixed-precision integers for equality using |(==)|.
Similarly:
\begin{spec}
instance Eq Float where
  x == y  =  floatEq x y
\end{spec}
allows us to compare floating-point numbers using |(==)|.

More importantly, datatypes that we have defined on our own can also
be made instances of the class |Eq|.  Consider, for example, the
|PitchClass| data type defined in Chapter \ref{ch:music}:
\begin{spec}
data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
                 |  Bf | Ass | B | Bs | Bss
\end{spec} 
We can declare |PitchClass| to be an instance of |Eq| as follows:
\begin{spec}
instance Eq PitchClass where
  Cff  == Cff  = True
  Cf   == Cf   = True
  C    == C    = True
  ...
  Bs   == Bs   = True
  Bss  == Bss  = True
  _    == _    = False
\end{spec}
where |...| refers to the other thirty equations needed to make this
definition complete.  Indeed, this is rather tedious!  It is not only
tedious, it is also dead obvious how |(==)| should be defined.

\subsection{Derived Instances}
\label{sec:derived-instances}

To alleviate the burden of defining instances such as above, Haskell
provides a convenient way to \emph{automatically derive} such instance
declarations from data type declarations, for certain predefined type
classes.  This is done using a |deriving| clause.  For example, in the
case of |PitchClass| we can simply write:
\begin{spec}

data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
                 |  Bf | Ass | B | Bs | Bss
     deriving Eq
\end{spec}
With this declaration, Haskell will automatically derive the instance
declaration given above, so that |(==)| behaves in the way we would
expect it to.

Consider now a polymorphic type, such as the |Primitive| type from
Chapter \ref{ch:music}:
\begin{spec}
data Primitive a  =  Note Dur a
                  |  Rest Dur
\end{spec}
What should an instance for this type in the class |Eq| look like?
Here is a first attempt:
\begin{spec}
instance Eq (Primitive a) where
  Note d1 x1  == Note d2 x2  = (d1==d2) && (x1==x2)
  Rest d1     == Rest d2     = d1==d2
  _           == _           = False
\end{spec}
Note the use of |(==)| on the right-hand side, in several places.  Two
of those places involve |Dur|, which a type synonym for |Rational|.
The |Rational| type is in fact a predefined instance of |Eq|, so all
is well there.  (If it were not an instance of |Eq|, a type error
would result.)

But what about the term |x1==x2|?  |x1| and |x2| are values of the
polymorphic type |a|, but how do we know that equality is defined
on |a|, i.e.\ that the type |a| is an instance of |Eq|?  In fact this
is not known in general.  The simple fix is to add a constraint to the
instance declaration, as follows:
\begin{spec}
instance Eq a ==> Eq (Primitive a) where
  Note d1 x1  == Note d2 x2  = (d1==d2) && (x1==x2)
  Rest d1     == Rest d2     = d1==d2
  _           == _           = False
\end{spec}
This can be read, ``For any type |a| in the class |Eq|, the type
|Primitive a| is also in the class |Eq|, and here is the definition of
|(==)| for that type.''  Indeed, if we had written the original type
declaration like this:
\begin{spec}
data Primitive a  =  Note Dur a        
                  |  Rest Dur          
     deriving Eq
\end{spec}
then Haskell would have derived the above correct instance declaration
automatically.  

So, for example, |(==)| is defined on the type |Primitive Pitch|,
because |Pitch| is a type synonym for |(PitchClass, Octave)|, and (a)
|PitchClass| is an instance of |Eq| by the effort above, (b) |Octave|
is a synonym for |Int|, which is a predefined instance of |Eq|, and
(c) as mentioned earlier the pair type is a predefined instance of
|Eq|.  Indeed, now that an instance for a polymorphic type has been
seen, we can understand what the predefined instance for polymorphic
pairs must look like, namely:
\begin{spec}
instance (Eq a, Eq b) => Eq (a,b) where
  (x1,y1) == (x2,y2) = (x1==x2) && (y1==y2)
\end{spec}

About the only thing not considered is a \emph{recursive} data type.
For example, recall the |Music| data type, also from Chapter
\ref{ch:music}:
\begin{spec}
data Music a  =  Prim (Primitive a)
              |  Music a :+: Music a
              |  Music a :=: Music a
              |  Modify Control (Music a)
\end{spec}
Its instance declaration for |Eq| seems obvious:
\begin{spec}
instance Eq a => Eq (Music a) where
  Prim p1       == Prim p2       = p1==p2
  (m1 :+: m2)   == (m3 :+: m4)   = (m1 == m3)  && (m2 == m4)
  (m1 :=: m2)   == (m3 :=: m4)   = (m1 == m3)  && (m2 == m4)
  Modify c1 m1  == Modify c2 m2  = (c1 == c2)  && (m1 == m2)
\end{spec}
Indeed, assuming that |Control| is an instance of |Eq|, this is just
what is expected, and can be automatically derived by adding a
|deriving| clause to the data type declaration for |Music|.

\subsection{Default Methods}
\label{sec:default-methods}

In reality, the class \indexwdhs{Eq} as defined in Haskell's Standard
Prelude is slightly richer than what is defined above.  Here it is
in its exact form: \indexhs{(/=)}
\begin{spec}
class Eq a  where
  (==), (/=)       :: a -> a -> Bool
  x /= y           =  not (x == y)
  x == y           =  not (x /= y)
\end{spec}
\index{class!default method}

This is an example of a class with two operations, one for equality,
the other for inequality.  It also demonstrates the use of a
\emph{default method}, one for each operator.  If a method for a
particular operation is omitted in an instance declaration, then the
default one defined in the class declaration, if it exists, is used
instead.  For example, all of the instances of |Eq| defined earlier
will work perfectly well with the above class declaration, yielding
just the right definition of inequality that we would expect: the
logical negation of equality.

\syn{Both the inequality and the logical negation operators are shown
  here using the mathematical notation, |/=| and |not|, respectively.
  When writing your Haskell programs, you instead will have to use the
  operator {\tt /=} and the function name {\tt not}, respectively.}

A useful slogan that helps to distinguish type classes from ordinary
polymorphism is this: ``polymorphism captures similar structure over
different values, while type classes capture similar operations over
different structures.''  For example, a sequences of integers,
sequence of characters, etc.\ can be captured as a polymorphic |List|,
whereas equality of integers, equality of trees, etc.\ can be captured
by a type class such as |Eq|.

\subsection{Inheritance}
\label{sec:inheritance}

\index{class!inheritance}

Haskell also supports a notion called \emph{inheritance}.  For
example, we may wish to define a class \indexwdhs{Ord} that
``inherits'' all of the operations in |Eq|, but in addition has a set
of comparison operations and minimum and maximum functions (a fuller
definition of |Ord|, as taken from the Standard Prelude, is given in
Appendix \ref{ch:class-tour}):
\begin{spec}
class Eq a => Ord a  where
  (<), (<=), (>=), (>)  :: a -> a -> Bool
  max, min              :: a -> a -> a
\end{spec}
\index{class!superclass}
\index{class!subclass} 

Note the constraint |Eq a =>| in the |class| declaration.  |Eq| is a
\emph{superclass} of |Ord| (conversely, |Ord| is a \emph{subclass} of
|Eq|), and any type that is an instance of |Ord| must also be an
instance of |Eq|.  The reason that this extra constraint makes sense
is that to perform comparisons such as |a<=b| and |a>=b| implies that
we know how to compute |a==b|.

For example, following the strategy used for |Eq|, we could declare
|Music| an instance of |Ord| as follows (note the constraint |Ord a =>
...|):
\begin{spec}
instance Ord a => Ord (Music a)  where
  Prim p1       < Prim p2       = p1 < p2
  (m1 :+: m2)   < (m3 :+: m4)   = (m1<m3)  && (m2<m4)
  (m1 :=: m2)   < (m3 :=: m4)   = (m1<m3)  && (m2<m4)
  Modify c1 m1  < Modify c2 m2  = (c1<c2)  && (m1<m2)
  ...
\end{spec}
Although this is a perfectly well-defined definition for |<|, it is
not clear that it exhibits the desired behavior, an issue that will be
returned to in Section \ref{sec:tc-laws}.

Another benefit of inheritance is shorter constraints.  For example,
the type of a function that uses operations from both the |Eq| and
|Ord| classes can use just the constraint |(Ord a)| rather than
|(Eq a, Ord a)|, since |Ord| ``implies'' |Eq|.

\indexhs{sort}
\index{class!multiple inheritance}

As an example of the use of |Ord|, a generic \emph{sort} function
should be able to sort lists of any type that is an instance of
|Ord|, and thus its most general type should be:
\begin{spec}
sort  ::  Ord a => [a] -> [a]
\end{spec}
This typing for |sort| would naturally arise through the use of
comparison operators such as |<| and |>=| in its definition.

\syn{Haskell also permits \emph{multiple inheritance}, since classes
may have more than one superclass.  Name conflicts are avoided by the
constraint that a particular operation can be a member of at most one
class in any given scope.  For example, the declaration
\begin{spec}
class (Eq a, Show a) => C a where ...
\end{spec}
creates a class |C| that inherits operations from both |Eq| and
|Show|.

% Contexts are also allowed in |data| declarations; see
% Section \ref{datatype-decls}.

Finally, class methods may have additional class constraints on any
type variable except the one defining the current class.  For example,
in this class:
\begin{spec}
class C a where
  m :: Eq b => a -> b
\end{spec}
the method |m| requires that type |b| is in class |Eq|.
However, additional class constraints on type |a| are not allowed
in the method |m|; these would instead have to be part of the
overall constraint in the class declaration.}

\section{Haskell's Standard Type Classes}
\label{sec:standard-type-classes}

The Standard Prelude defines many useful type classes, including |Eq|
and |Ord|.  They are described in detail in Appendix
\ref{ch:class-tour}.  In addition, the Haskell Report and the Library
Report contain useful examples and discussions of type classes; you
are encouraged to read through them.

Most of the standard type classes in Haskell are shown in
Figure~\ref{fig:common-type-classes}, along with their key instances.
Since each of these has various default mthods defined, also shown is
the minimal set of methods that must defined---the rest are taken care
of by the default methods.  For example, for |Ord|, all we have to
provide is a definition for |(<=)|.

The \indexwdhs{Num} class, which has been used implicitly throughout
much of the text, is described in more detail below.  With this
explanation a few more of Haskell's secrets will be revealed.

\begin{figure}
\begin{tabular}{||||l||l||l||||} \hline
{\bf Type}  & {\bf Key} & {\bf Key} \\
{\bf Class} & {\bf functions} & {\bf instances} \\
\hline
|Num|  & |(+),(-),(*) :: Num a => a->a->a| & |Integer, Int, Float, Double,| \\ 
       & |negate :: Num a => a->a|         & |Rational| \\
       & minimal set: all but |(-)| or |negate| & \\
\hline
|Eq|   & |(==),(/=) :: Eq a => a->a->Bool| & |Integer, Int, Float, Double,| \\
       &                                   & |Rational, Char, Bool, ... | \\
       & minimal set: either |(==)| or |(/=)|  & \\
\hline
|Ord|  & |(>),(<),(>=),(<=) ::|            & |Integer, Int, Float, Double,| \\
       & \ \ \ \ |Ord a => a->a->Bool|     & |Rational, Char, Bool, ... | \\
       & |max,min :: Ord a => a->a->a   |  & \\
       & minimal set: |(<=)|               & \\
\hline
|Enum| & |succ,pred :: Enum a => a->a|     & |Integer, Int, Float, Double,| \\
       & |fromEnum :: Enum a => a -> Int|  & |Rational, Char, Bool, ... | \\
       & |toEnum :: Enum a => Int -> a  |  & \\
       & also enables arithmetic sequences & \\
       & minimal set: |toEnum| \& |fromEnum| & \\
\hline
|Bounded| & |minBound,maxBound :: a|       & |Int, Char, Bool| \\
\hline
|Show| & |show :: Show a => a -> String|   & Almost every type except \\
       &                                   & for functions \\
\hline
|Read| & |read :: Read a => String -> a|   & Almost every type except \\
       &                                   & for functions \\
\hline
\end{tabular}
\caption{Common Type Classes and Their Instances}
\label{fig:common-type-classes}
\end{figure}

\subsection{The |Num| Class}
\label{sec:num-class}

As we already know, Haskell provides several kinds of numbers, some of
which have already been introduced: |Int|, |Integer|, |Rational|, and
|Float|.  These numbers are instances of various type classes arranged
in a rather complicated hierarchy.  The reason for this is that there
are many operations, such as |(+)|, |abs|, and |sin|, that are common
amongst some of these number types.  For example, we would expect
|(+)| to be defined on every kind of number, whereas |sin| might only
be applicable to either single precision (|Float|) or double-precision
(|Double|) floating-point numbers.

Control over which numerical operations are allowed and which are not
is the purpose of the numeric type class hierarchy.  At the top of the
hierarchy, and therefore containing operations that are valid for all
numbers, is the class |Num|.  It is defined as:
\begin{spec}
class  (Eq a, Show a) => Num a  where
  (+), (-), (*)  :: a -> a -> a
  negate         :: a -> a
  abs, signum    :: a -> a
  fromInteger    :: Integer -> a
\end{spec}
Note that |(/)| is \emph{not} an operation in this class.
|negate| is the negation function; \indexwdhs{abs} is the absolute
value function; and \indexwdhs{signum} is the sign function, which
returns |-1| if its argument is negative, |0| if it is |0|,
and |1| if it is positive.  |fromInteger| converts an
|Integer| into a value of type |Num a => a|, which is useful for
certain coercion tasks.

\indexamb{(-)}{negation}
\indexhs{negate}
\syn{Haskell also has a negation operator, which is Haskell's only
prefix operator.  However, it is just shorthand for |negate|.  That
is, |-e| in Haskell is shorthand for |negate e|.

The operation \indexwdhs{fromInteger} also has a special purpose.  How
is it that we can write the constant |42|, say, both in a context
requiring an |Int| and in one requiring a |Float| (say).  Somehow
Haskell ``knows'' which version of |42| is required in a given
context.  But, what is the type of |42| itself?  The answer is that it
has type |Num a ==> a|, for some |a| to be determined by its context.
(If this seems strange, remember that |[]| by itself is also somewhat
ambiguous; it is a list, but a list of what?  The most we can say
about its type is that it is |[a]| for some |a| yet to be determined.)

The way this is achieved in Haskell is that literal numbers such as
|42| are actually considered to be shorthand for |fromInteger 42|.
Since |fromInteger| has type |Num a => Integer -> a|, then
|fromInteger 42| has type |Num a => a|.}

The complete hierarchy of numeric classes is shown in Figure
\ref{fig:tc-hierarchy}; note that some of the classes are subclasses
of certain non-numeric classes, such as |Eq| and |Show|.  The comments
below each class name refer to the Standard Prelude types that are
instances of that class.  See Appendix \ref{ch:class-tour} for more
detail.

\begin{figure}
\centerline{
\epsfysize=7in 
\epsfbox{Pics/classes.eps}
}
\caption{Numeric Class Hierarchy}
\label{fig:tc-hierarchy}
\end{figure}

The Standard Prelude actually defines only the most basic numeric
types: |Int|, |Integer|, |Float| and |Double|.  Other
numeric types such as rational numbers (|Ratio a|) and complex
numbers (|Complex a|) are defined in libraries.  The connection
between these types and the numeric classes is given in Figure
\ref{fig:standard-numeric-types}.  The instance declarations implied
by this table can be found in the Haskell Report.

\begin{figure}
\begin{tabular}{lll}
{\bf Numeric Type} & {\bf Type Class} & {\bf Description} \\
\hline \\
|Int|             & |Integral| & Fixed-precision integers \\
|Integer|         & |Integral| & Arbitrary-precision integers \\
|Integral a =>|   \\
\ \ \ |Ratio a|   & |RealFrac| & Rational numbers \\
|Float|           & |RealFloat|& Real floating-point, single precision\\
|Double|          & |RealFloat|& Real floating-point, double precision\\
|RealFloat a =>|  \\
\ \ \ |Complex a| & |Floating| & Complex floating-point 
\end{tabular}
\caption{Standard Numeric Types}
\label{fig:standard-numeric-types}
\end{figure}

\subsection{The |Show| Class}

It is very common to want to convert a data type value into a string.
In fact, it happens all the time when we interact with GHCi at the
command prompt, and GHCi will complain if it does not ``know'' how to
``show'' a value.  The type of anything that GHCi prints must be an
instance of the |Show| class.

\pagebreak

Not all of the operations in the |Show| class will be discussed here,
in fact the only one of interest is |show|:
\begin{spec}
class Show a where
  show :: a -> String
  ...
\end{spec}
Instances of |Show| can be derived, so normally we do not have to worry
about the details of the definition of |show|.  

%% For example, the actual definition of the |Primitive| type that we
%% gave in Chapter \ref{ch:music} is:
%% \begin{spec}
%% data Primitive  =  Note Dur Pitch           
%%                 |  Rest Dur                 
%%      deriving (Show, Eq, Ord)
%% \end{spec}

Lists also have a |Show| instance, but it is not derived, since,
after all, lists have special syntax.  Also, when |show| is applied to
a string such as |"Hello"|, it should generate a string that, when
printed, will look like |"Hello"|.  This means that it must include
characters for the quotation marks themselves, which in Haskell is
achieved by prefixing the quotation mark with the ``escape'' character
$\backslash$.  Given the following data declaration:
\begin{spec}
data Hello = Hello
     deriving Show
\end{spec}
it is then instructive to ponder over the following calculations:
\begin{spec}
show Hello                ===> "Hello"
show (show Hello)         ===> show "Hello"  ===>  "\"Hello\""
show (show (show Hello))  ===> "\"\\\"Hello\\\"\""
\end{spec}
\syn{To refer to the escape character itself, it must also be escaped;
thus |"\\"| prints as $\backslash$.}

For further pondering, consider the following program.  See if you can
figure out what it does, and why!\footnote{The essence of this idea is
due to Willard Van Orman Quine \cite{Quine}, and its use in a computer
program is discussed by Hofstadter \cite{Hofstadter}.  It was adapted
to Haskell by J\'{o}n Fairbairn.}
\begin{spec}
main     = putStr (quine q)
quine s  = s ++ show s
q        = "main = putStr (quine q)\nquine s = s ++ show s\nq = "
\end{spec}

\syn{The |"\n"| that appears twice in the string |q| is a ``newline''
  character; that is, when |q| is printed (or displayed on a console)
  the string starting to the right of |"\n"| will appear on a new line.}

Derived |Show| instances are possible for all types whose component
types also have |Show| instances.  |Show| instances for most of
the standard types are provided in the Standard Prelude.

%% Some types, such as the function type |(->)|, have a |Show|
%% instance that simply generates the string |"<<function>>"|, but not
%% a corresponding |Read| instance.

\subsection{The Functor Class}
\label{sec:functor-class}

[Define |Functor| class, and show instances for lists, |Maybe|,
  |Primitive|, |Music|, ...]

TBD

\section{Other Derived Instances}
\label{sec:other-derived-instances}

\index{class!derived instance}
\index{class!\hkw{deriving}}
\indexkw{deriving}

In addition to |Eq| and |Ord|, instances of \indexwdhs{Enum},
\indexwdhs{Bounded}, \indexwdhs{Ix}, \indexwdhs{Read}, and
\indexwdhs{Show} (see Appendix \ref{ch:class-tour}) can also be
generated by the |deriving| clause.  These type classes are widely
used in Haskell programming, making the deriving mechanism very
useful.

The textual representation defined by a derived |Show| instance is
consistent with the appearance of constant Haskell expressions
(i.e.\ values) of the type involved.  For example, from:
\begin{spec}
data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
  deriving (Show, Eq, Ord, Enum, Bounded)
\end{spec}
we can expect that:
\begin{spec}
show [Red ..]  
===>  "[Black,Blue,Green,Cyan,Red,Magenta,Yellow,White]"
\end{spec}

We can also expect that:
\begin{spec}
minBound :: Color ===> Black
maxBound :: Color ===> White
\end{spec}
Note that the type signature ``|:: Color|'' is given explicitly in
this case, because, out of any context, at least, Haskell does not
know the type for which you are trying to determine the minimum and
maximum bounds.

Further details about derived instances can be found in the Haskell
Report.

Many of the predefined data types in Haskell have |deriving| clauses,
even ones with special syntax.  For example, if we could write a data
type declaration for lists (the reason we cannot do this is that
lists have special syntax, both at the value and type level) it would
look something like this:
\begin{spec}
data [a]  =  [] 
          |  a : [a] 
     deriving (Eq, Ord)
\end{spec}
The derived |Eq| and |Ord| instances for lists are the usual
ones; in particular, character strings, as lists of characters, are
ordered as determined by the underlying |Char| type, with an
initial sub-string being less than a longer string; for example,
|"cat" < "catalog"| is |True|.

In practice, |Eq| and |Ord| instances are almost always derived,
rather than user-defined.  In fact, you should provide your own
definitions of equality and ordering predicates only with some
trepidation, being careful to maintain the expected algebraic
properties of equivalence relations and total orders, respectively
(more on this later).  An intransitive |(==)| predicate, for example,
would be problematic, confusing readers of the program who expect
|(==)| to be transitive.  Nevertheless, it is sometimes necessary to
provide |Eq| or |Ord| instances different from those that would be
derived.

The data type declarations for |PitchClass|, |Primitive|, |Music| and
|Control| given in Chapter~\ref{ch:intro} are not the ones actually
used in Eutperpea.  The actual definitions each use a |deriving|
clause, and are shown in Figure~\ref{fig:actual-datatypes}.  The
|InstrumentName| data type from Chapter~\ref{ch:intro} also has a
deriving clause for |Show|, |Eq|, and |Ord| (but is ommitted here to
save space).

\syn{When instances of more than one type class are derived for the
  same data type, they appear grouped in parentheses as in
  Figure~\ref{fig:actual-datatypes}.  Also, in this case |Eq|
  \emph{must} appear if |Ord| does (unless an explicit instance for
  |Eq| is given), since |Eq| is a superclass of |Ord|.}

\begin{figure}
\cbox{\small
\begin{spec}
data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
                 |  Bf | Ass | B | Bs | Bss
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Primitive a  =  Note Dur a        
                  |  Rest Dur          
  deriving (Show, Eq, Ord)

data Music a  = 
       Prim (Primitive a)               -- primitive value 
    |  Music a :+: Music a              -- sequential composition
    |  Music a :=: Music a              -- parallel composition
    |  Modify Control (Music a)         -- modifier
  deriving (Show, Eq, Ord)

data Control =
       Tempo       Rational           -- scale the tempo
    |  Transpose   AbsPitch           -- transposition
    |  Instrument  InstrumentName     -- instrument label
    |  Phrase      [PhraseAttribute]  -- phrase attributes
    |  Player      PlayerName         -- player label
  deriving (Show, Eq, Ord)
\end{spec}}
\caption{Euterpea's Data Types with Deriving Clauses}
\label{fig:actual-datatypes}
\end{figure}

Note that with single and double sharps and flats, there are many
enharmonic equivalences.  Thus in the data declaration for
|PitchClass|, the constructors are ordered such that, if |pc1 < pc2|,
then |pcToInt pc1 <= pcToInt pc2|.

For some examples, the |Show| class allows us to convert values to
strings:
\begin{spec}
show Cs              ===> "Cs"
show concertA        ===> "(A,4)"
\end{spec}
The |Read| class allows us to go the other way around:
\begin{spec}
read "Cs"            ===> Cs
read "(A,4)"         ===> (A,4)
\end{spec}
The |Eq| class allows testing values for equality:
\begin{spec}
concertA == a440     ===> True
concertA == (A,5)    ===> False
\end{spec}
And the |Ord| class has relational operators for types whose values
can be ordered:
\begin{spec}
C < G                ===> True
max C G              ===> G
\end{spec}
The |Enum| class is for ``enummerable types.''  For example:
\begin{spec}
succ C               ===> Dff
pred 1               ===> 0
fromEnum C           ===> 2
toEnum 3             ===> Dff
\end{spec}
The |Enum| class is also the basis on which \emph{arithmetic
  sequences} (defined earlier in Section
\ref{sec:arithmetic-sequences}) are defined.

\section{The type of |play|}
\label{sec:play-type}

Ever since the |play| function was introduced in
Chapter~\ref{ch:music}, we have been using it to ``play'' the results
of our |Music| values, i.e.\ to listen to their rendering through
MIDI.  However, it is just a function like any other function in
Haskell, but we never discussed what its type is.  In fact, here it
is:
\begin{spec}
play :: Performable a => Music a -> IO ()
\end{spec}
The type of the result, |IO ()|, is the type of a \emph{command} in
Haskell, i.e.\ something that ``does I/O.''  We will have more to say
about this in a later chapter.

But of more relevance to this chapter, note the constraint
|Performable a|.  You might guess that |Performable| is a type class,
indeed it is the type class of ``performable values.''  If a type is a
member of (i.e.\ instance of) |Performable|, then it can be
``performed,'' i.e.\ rendered as sound.  The point is, some things we
would not expect to be performable, for example a list or a character
or a function.  So the type signature for |play| can be read, ``For
any type |T| that is a member of the class |Performable|, |play| has
type |Music T -> IO ()|.''

Currently the types |Pitch|, |(Pitch,Volume)|, and
|(Pitch,[NoteAttribute])| are members of the class |Performable|.
(The |NoteAttribute| data type will be introduced in
Chapter~\ref{ch:performance}.)  Indeed, we have used |play| on the
first two of these types, i.e.\ on values of type |Music Pitch| and
|Music (Pitch,Volume)| in previous examples, and you might have
wondered how both could possibly be properly typed---hopefully now it
is clear.

\section{Reasoning With Type Classes}
\label{sec:tc-laws}

\index{class!laws}

Type classes often imply a set of \emph{laws} that govern the use of
the operators in the class.  For example, for the |Eq| class, we can
expect the following laws to hold for every instance of the class:
\begin{spec}
x == x
x == y            =:> y == x
(x==y) && (y==z)  =:> x == z
(x /= y)          =:> not (x == y)
\end{spec}
where $\supseteq$ should be read ``implies
that.''\footnote{Mathematically, the first three of these laws are the
  same as those for an \emph{equivalence relation}.}

However, there is no way to guarantee these laws.  A user may create
an instance of |Eq| that violates them, and in general Haskell has no
way to enforce them.  Nevertheless, it is useful to state the laws of
interest for a particular class, and to state the expectation that all
instances of the class be ``law-abiding.''  Then as diligent
functional programmers, we should ensure that every instance that is
defined, whether for our own type class or someone else's, is in fact
law-abiding.  \index{type!qualified}

As another example, consider the |Ord| class, whose instances are
intended to be \emph{totally ordered}, which means that the following
laws should hold, for all |a|, |b|, and |c|:
\begin{spec}
a <= a
(a <= b) && (b <= c)  =:> (a <= c)
(a <= b) && (b <= a)  =:> (a == b)
(a /= b)              =:> (a < b) || (b < a)
\end{spec}
Similar laws should hold for |(>)|.

But alas, the instance of |Music| in the class |Ord| given in Section
\ref{sec:inheritance} does not satisfy all of these laws!  To see why,
consider two |Primitive| values |p1| and |p2| such that |p1 < p2|.
Now consider these two |Music| values:
\begin{spec}
m1  = Prim p1 :+: Prim p2
m2  = Prim p2 :+: Prim p1
\end{spec}
Clearly |m1 == m2| is false, but the problem is, so are |m1 < m2|
and |m2 < m1|, thus violating the last law above.  

\index{lexicographic ordering}

To fix the problem, a \emph{lexicographic ordering} should be used on
the |Music| type, such as used in a dictionary.  For example,
``polygon'' comes before ``polymorphic,'' using a left-to-right
comparison of the letters.  The new instance declaration looks like
this:
\begin{spec}
instance Ord a => Ord (Music a) where
  Prim p1       < Prim p2        = p1 < p2
  Prim p1       < _              = True
  (m1 :+: m2)   < Prim _         = False
  (m1 :+: m2)   < (m3 :+: m4)    =  (m1<m3) || 
                                    (m1==m3) && (m2<m4)
  (m1 :+: m2)   < _              = True
  (m1 :=: m2)   < Prim _         = False
  (m1 :=: m2)   < (m3 :+: m4)    = False
  (m1 :=: m2)   < (m3 :=: m4)    =  (m1<m3) || 
                                    (m1==m3) && (m2<m4)
  (m1 :=: m2)   < _              = True
  Modify c1 m1  < Modify c2 m2   =  (c1<c2) || 
                                    (c1==c2) && (m1<m2)
  Modify c1 m1  < _              = False
\end{spec}
This example shows the value of checking to be sure that each instance
obeys the laws of its class.  Of course, that check should come in the
way of a proof.  This example also highlights the utility of derived
instances, since the derived instance of |Music| for the class |Ord|
is equivalent to that above, yet is done automatically.

\vspace{.1in}\hrule

\begin{exercise}{\em
Prove that the instance of |Music| in the class |Eq| satisfies
the laws of its class.  Also prove that the modified instance of
|Music| in the class |Ord| satisfies the laws of its class.}
\end{exercise}

\begin{exercise}{\em
Write out appropriate instance declarations for the |Color| type in
the classes |Eq|, |Ord|, and |Enum|.  (For simplicity you may define
|Color| to have fewer constructors, say just |Red|, |Green| and
|Blue|.)}
\end{exercise}

\begin{exercise}{\em
Define a type class called |Temporal| whose members are types that can be
interpreted as having a temporal duration.  |Temporal| should have
three operations, namely:
\begin{spec}
durT   :: Temporal a => a -> Dur
takeT  :: Temporal a => Dur -> a -> a
dropT  :: Temporal a => Dur -> a -> a
\end{spec}
Then define instances of |Temporal| for the types |Music| and
|Primitive|.  (Hint: this is not as hard as it sounds, because you can
\emph{reuse} some function names previously defined to do these sorts
of operations.)

Can you think of other types that are temporal?}
\end{exercise}

\begin{exercise}{\em
Functions are not members of the |Eq| class, because, in general,
determining whether two functions are equal is undecideable.  But
functions whose domains are finite, and can be completely enumerated,
\emph{can} be tested for equality.  We just need to test that each
function, when applied to each element in the domain, returns the same
result.

Define an instance of |Eq| for functions.  For this to be possible,
note that, if the function type is |a->b|, then:
\begin{itemize}
\item
the type |a| must be \emph{enumerable} (i.e.\ a member of the |Enum| class), 
\item
the type |a| must be \emph{bounded} (i.e.\ a member of |Bounded| class), and
\item
the type |b| must admit \emph{equality} (i.e.\ be a member of the |Eq|
class).
\end{itemize}
These constraints must therefore be part of the instance declaration.

Hint: using the minimum and maximum bounds of a type, enumerate all
the elements of that type using an arithmetic sequence (recall Section
\ref{sec:arithmetic-sequences}), which, despite its name, works for
any enumerable type.

Test your implementation by defining some functions on existing
Euterpea types that are finite and bounded (such as |PitchClass|
and |Color|), or by defining some functions on your own data type(s).}
\end{exercise}

\vspace{.1in}\hrule

