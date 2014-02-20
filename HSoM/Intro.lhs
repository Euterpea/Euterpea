%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter[Computer Music, Euterpea, and Haskell]{Overview of 
  Computer Music, Euterpea, and Haskell}
\label{ch:intro}

Computers are everywhere.  And so is music!  Although some might think
of the two as being at best distant relatives, in fact they share many
deep properties.  Music comes from the soul, and is inspired by the
heart, yet it has the mathematical rigor of computers.  Computers have
mathematical rigor of course, yet the most creative ideas in
mathematics and computer science come from the soul, just like music.
Both disciplines demand both left-brain and right-brain skills.  It
always surprises me how many computer scientists and mathematicians
have a serious interest in music.  It seems that those with a strong
affinity or acuity in one of these disciplines is often strong in the
other as well.

It is quite natural then to consider how the two might interact.  In
fact there is a long history of interactions between music and
mathematics, dating back to the Greeks' construction of musical scales
based on arithmetic relationships, and including many classical
composers use of mathematical structures, the formal harmonic analysis
of music, and many modern music composition techniques.  Advanced
music theory uses ideas from diverse branches of mathematics such as
number theory, abstract algebra, topology, category theory, calculus,
and so on.

There is also a long history of efforts to combine computers and
music.  Most consumer electronics today are digital, as are most forms
of audio processing and recording.  But in addition, digital musical
instruments provide new modes of expression, notation software and
sequencers have become standard tools for the working musician, and
those with the most computer science savvy use computers to explore
new modes of composition, transformation, performance, and analysis.

This textbook explores the fundamentals of computer music using a
language-centric approach.  In particular, the functional programming
language \emph{Haskell} is used to express all of the computer music
concepts.  Thus a by-product of learning computer music concepts will
be learning how to program in Haskell.  The core musical ideas are
collected into a Haskell library called \emph{Euterpea}.  The name
``Euterpea'' is derived from \emph{Euterpe}, who was one of the nine
Greek muses, or goddesses of the arts, specifically the muse of music.
A hypothetical picture of Euterpe graces the cover of this textbook.

\section{The Note vs. Signal Dichotomy}

The field of computer music has grown astronomically over the past
several decades, and the material can be structured and organized
along several dimensions.  A dimension that proves particulary useful
with respect to a programming language is one that separates
\emph{high-level} musical concerns from \emph{low-level} musical
concerns.  Since a ``high-level'' programming language---namely
Haskell---is used to program at both of these musical levels, to avoid
confusion the terms \emph{note level} and \emph{signal level} will be
used in the musical dimension.

At the \emph{note level}, a \emph{note} (i.e.\ pitch and duration) is
the lowest musical entity that is considered, and everything else is
built up from there.  At this level, in addition to conventional
representations of music, we can study interesting aspects of
so-called \emph{algorithmic composition}, including the use of
fractals, grammar-based systems, stochastic processes, and so on.
From this basis we can also study the harmonic and rhythmic
\emph{analysis} of music, although that is not currently an emphasis
in this textbook.  Haskell facilitates programming at this level
through its powerful data abstraction facilities, higher-order
functions, and declarative semantics.

In contrast, at the \emph{signal level} the focus is on the actual
sound generated in a computer music application, and thus a
\emph{signal} is the lowest entity that is considered.  Sound is
concretely represented in a digital computer by a discrete sampling of
the continuous audio signal, at a high enough rate that human ears
cannot distinguish the discrete from the continuous, usually 44,100
samples per second (the standard sampling rate used for CDs, mp3
files, and so on).  But in Euterpea, these details are hidden: signals
are treated abstractly as continuous quantities.  This greatly eases
the burden of programming with sequences of discrete values.  At the
signal level, we can study sound synthesis techniques (to simulate
the sound of a conventional instrument, say, or something completely
artificial), audio processing (e.g.\ determining the frequency
spectrum of a signal), and special effects (reverb, panning,
distortion, and so on).

Suppose for a moment that a musician is playing music using a
metro\-nome set at 96, which corresponds to 96 beats per minute.  That
means that one beat takes $\nicefrac{60}{96}$ = 0.625 seconds.  At a
stereo sampling rate of 44,100 samples per second, that in turn
translates into $2\times 0.625\times 44,100$ = 55,125 samples, and
each sample typically occupies several bytes of computer memory.  This
is typical of the minimum memory requirements of a computation at the
signal level.  In contrast, at the note level, we only need some kind
of operator or data structure that says ``play this note,'' which
requires a total of only a small handful of bytes.  This dramatic
difference highlights one of the key computational differences between
programming at the note level versus the signal level.

Of course, many computer music applications involve both the note
level \emph{and} the signal level, and indeed there needs to be a
mechanism to mediate between the two.  Although such mediation can
take many forms, it is for the most part straightforward.  Which is
another reason why the distinction between the note level and the
signal level is so natural.

This textbook begins with a treatment of the note level
(Chapters~\ref{ch:intro}-\ref{ch:MUI}) and follows with a treatment of
the signal level (Chapters~\ref{ch:signals}-\ref{ch:additive}).  If
you are interested only in the signal level, you could skip
Chapters~\ref{ch:performance}-\ref{ch:MUI}.

\section{Basic Principles of Programming}

Programming, in its broadest sense, is \emph{problem solving}.  It
begins by recognizing problems that can and should be solved using a
digital computer.  Thus the first step in programming is answering the
question, ``What problem am I trying to solve?''

% ``Solving the wrong problem'' is a phrase often heard in many
% contexts, and we certainly do not want to be victims of that crime.

% \footnote{Of course, not all problems fall into this category, and
% some problems are solved (or are attempted to be solved) using
% computers that probably should not be.  But I will avoid this
% digression.}

Once the problem is understood, a solution must be found.  This may
not be easy, of course, and in fact you may discover several
solutions, so a way to measure success is needed.  There are various
dimensions in which to do this, including correctness (``Will I get
the right answer?'') and efficiency (``Will it run fast enough, or use
too much memory?'').  But the distinction of which solution is better
is not always clear, since the number of dimensions can be large, and
programs will often excel in one dimension and do poorly in others.
For example, there may be one solution that is fastest, one that uses
the least amount of memory, and one that is easiest to understand.
Deciding which to choose can be difficult, and is one of the more
interesting challenges in programming.

The last measure of success mentioned above---clarity of a
program---is somewhat elusive: difficult to quantify and measure.
Nevertheless, in large software systems clarity is an especially
important goal, since such systems are worked on by many people over
long periods of time, and evolve considerably as they mature.  Having
easy-to-understand code makes it much easier to modify.

In the area of computer music, there is another reason why clarity is
important: namely, that the code often represents the author's thought
process, musical intent, and artistic choices.  A conventional
musical score does not say much about what the composer thought as she
wrote the music, but a program often does.  So when you write your
programs, write them for others to see, and aim for elegance and beauty,
just like the musical result that you desire.

Programming is itself a creative process.  Sometimes programming
solutions (or artistic creations) come to mind all at once, with
little effort.  More often, however, they are discovered only after
lots of hard work!  We may write a program, modify it, throw it away
and start over, give up, start again, and so on.  It is important to
realize that such hard work and reworking of programs is the norm, and
in fact you are encouraged to get into the habit of doing so.  Do not
always be satisfied with your first solution, and always be prepared
to go back and change or even throw away those parts of your program
that you are not happy with.

\section{Computation by Calculation}
\index{computation by calculation}

It is helpful when learning a new programming language to have a good
grasp of how programs in that language are executed---in other words,
an understanding of what a program \emph{means}.  The execution of
Haskell programs is perhaps best understood as \emph{computation by
  calculation}.  Programs in Haskell can be viewed as \emph{functions}
whose input is that of the problem being solved, and whose output is
the desired result---and the behavior of functions can be effectively
understood as computation by calculation.

An example involving numbers might help to demonstrate these ideas.
Numbers are used in many applications, and computer music is no
exception.  For example, integers might be used to represent pitch,
and floating-point numbers might be used to perform calculations
involving frequency or amplitude.

Suppose we wish to perform an arithmetic calculation such as
$3\times(9+5)$.  In Haskell this would be written as |3*(9+5)|, since
most standard computer keyboards and text editors do not recognize the
special symbol $\times$.  The result can be calculated as follows:
\begin{spec}
3*(9+5) 
==> 3*14 
==> 42
\end{spec}
It turns out that this is not the only way to compute the result, as
evidenced by this alternative calculation:\footnote{This assumes that
  multiplication distributes over addition in the number system being
  used, a point that will be returned to later in the text.}
\begin{spec}
3*(9+5) 
==> 3*9 + 3*5 
==> 27 + 3*5 
==> 27+15 
==> 42
\end{spec}
\index{efficiency}% 

Even though this calculation takes two extra steps, it at least gives
the same, correct answer.  Indeed, an important property of each and
every program written in Haskell is that it will always yield the same
answer when given the same inputs, regardless of the order chosen to
perform the calculations.\footnote{This is true as long as a
  non-terminating sequence of calculations is not chosen, another
  issue that will be addressed later.} This is precisely the
mathematical definition of a \emph{function}: for the same inputs, it
always yields the same output.

On the other hand, the first calculation above required fewer steps
than the second, and thus it is said to be more \emph{efficient}.
Efficiency in both space (amount of memory used) and time (number of
steps executed) is important when searching for solutions to problems.
Of course, if the computation returns the wrong answer, efficiency is
a moot point.  In general it is best to search first for an elegant
(and correct!)  solution to a problem, and later refine it for better
performance.  This strategy is sometimes summarized as, ``Get it right
first!''

The above calculations are fairly trivial, but much more sophisticated
computations will be introduced soon enough.  For starters---and to
introduce the idea of a Haskell function---the arithmetic operations
performed in the previous example can be \emph{generalized} by
defining a function to perform them for any numbers |x|, |y|, and |z|:
\indexhs{simple}
\begin{spec}
simple x y z = x*(y+z)
\end{spec}
This equation defines |simple| as a function of three
\emph{arguments}, |x|, |y|, and |z|.  In mathematical notation this
definition might be written differently, such as one of the following:
\[\begin{array}{l}
{\it simple}(x,y,z) = x\times(y+z)\\
{\it simple}(x,y,z) = x\cdot(y+z)\\
{\it simple}(x,y,z) = x(y+z)
\end{array}\]
In any case, it should be clear that ``|simple 3 9 5|'' is the same
as ``|3*(9+5)|.''  In fact the proper way to calculate the result is:
\begin{spec}
simple 3 9 5 
==> 3*(9+5)
==> 3*14
==> 42
\end{spec}
\index{unfold (in a calculation)}
The first step in this calculation is an example of \emph{unfolding} a
function definition: 3 is substituted for |x|, 9 for |y|, and 5
for |z| on the right-hand side of the definition of |simple|.
This is an entirely mechanical process, not unlike what the computer
actually does to execute the program.

|simple 3 9 5| is said to \emph{evaluate} to 42.  To express the fact
that an expression $e$ evaluates (via zero, one, or possibly many more
steps) to the value $v$, we will write $e \Longrightarrow v$ (this arrow
is longer than that used earlier).  So we can say directly, for
example, that |simple 3 9 5 ===> 42|, which should be read ``|simple 3
9 5| evaluates to 42.''

With |simple| now suitably defined, we can repeat the sequence of
arithmetic calculations as often as we like, using different values
for the arguments to |simple|.  For example, 
|simple 4 3 2 ===> 20|.

We can also use calculation to \emph{prove properties} about
programs.  For example, it should be clear that for any |a|, |b|, and
|c|, |simple a b c| should yield the same result as |simple a c b|.
For a proof of this, we calculate \emph{symbolically}; that is,
using the symbols |a|, |b|, and |c| rather than concrete numbers such
as 3, 5, and 9:
\begin{spec}
simple a b c 
==> a*(b+c) 
==> a*(c+b)
==> simple a c b
\end{spec}
Note that the same notation is used for these symbolic steps as for
concrete ones.  In particular, the arrow in the notation reflects the
direction of formal reasoning, and nothing more.  In general, if |e1
==> e2|, then it is also true that |e2 ==> e1|.

These symbolic steps are also referred to as as ``calculations,'' even
though the computer will not typically perform them when executing a
program (although it might perform them \emph{before} a program is run
if it thinks that it might make the program run faster).  The second
step in the calculation above relies on the commutativity of addition
(namely that, for any numbers $x$ and $y$, $x+y=y+x$).  The third step
is the reverse of an unfold step, and is appropriately called a
\emph{fold} calculation.  \index{fold (in a calculation)} It would be
particularly strange if a computer performed this step while executing
a program, since it does not seem to be headed toward a final answer.
But for proving properties about programs, such ``backward reasoning''
is quite important.

When we wish to spell out the justification for each step, whether
symbolic or concrete, a calculation can be annotated with more detail,
as in:
\begin{spec}
simple a b c
==> { unfold }
a*(b+c)
==> { commutativity }
a*(c+b)
==> { fold }
simple a c b
\end{spec}
In most cases, however, this will not be necessary.

Proving properties of programs is another theme that will be repeated
often in this text.  Computer music applications often have some kind
of a mathematical basis, and that mathematics must be reflected
somewhere in our programs.  But how do we know if we got it right?
Proof by calculation is one way to connect the problem specification
with the program solution.

More broadly speaking, as the world begins to rely more and more on
computers to accomplish not just ordinary tasks such as writing term
papers, sending email, and social networking, but also life-critical
tasks such as controlling medical procedures and guiding spacecraft,
then the correctness of programs gains in importance.  Proving complex
properties of large, complex programs is not easy---and rarely if ever
done in practice---but that should not deter us from proving simpler
properties of the whole system, or complex properties of parts of the
system, since such proofs may uncover errors, and if not, will at
least give us confidence in our effort.

If you are someone who is already an experienced programmer, the idea
of computing \emph{everything} by calculation may seem odd at best,
and na\"{i}ve at worst.  How do we write to a file, play a sound,
draw a picture, or respond to mouse-clicks?  If you are wondering
about these things, it is hoped that you have patience reading the
early chapters, and that you find delight in reading the later
chapters where the full power of this approach begins to shine.

In many ways this first chapter is the most difficult, since it
contains the highest density of new concepts.  If the reader has
trouble with some of the concepts in this overview chapter, keep in
mind that most of them will be revisited in later chapters.  And do not
hesitate to return to this chapter later to re-read difficult
sections; they will likely be much easier to grasp at that time.

\syn{In the remainder of this textbook the need will often arise to
  explain some aspect of Haskell in more detail, without distracting
  too much from the primary line of discourse.  In those circumstances
  the explanations will be offset in a shaded box such as this one,
  proceeded with the word ``Details.''}

\vspace{.1in}\hrule

\begin{exercise}{\em
Write out all of the steps in the calculation of the value of
\begin{spec}
simple (simple 2 3 4) 5 6
\end{spec}
}
\end{exercise}

\begin{exercise}{\em
Prove by calculation that |simple (a-b) a b ===>| $a^2 - b^2$.}
\end{exercise}

\vspace{.1in}\hrule

\section{Expressions and Values}
\label{sec:expressions}

In Haskell, the entities on which calculations are performed are
called \emph{\indexwd{expressions}}, and the entities that result from
a calculation---i.e.\ ``the answers''---are called
\emph{\indexwd{values}}.  It is helpful to think of a value just as an
expression on which no more calculation can be carried out---every
value is an expression, but not the other way around.

Examples of expressions include \emph{atomic} (meaning, indivisible)
values such as the integer |42| and the character |'a'|, which are
examples of two \emph{primitive} atomic values in Haskell.  The next
chapter introduces examples of \emph{constructor} atomic values, such
as the musical notes |C|, |D|, |Ef|, |Fs|, etc., which in standard
music notation are written C, D, E$\flat$, F$\sharp$, etc., and are
pronounced C, D, E-flat, F-sharp, etc.  (In music theory, note names
are called \emph{pitch classes}.).

In addition, there are \emph{structured} expressions (i.e., made from
smaller pieces) such as the \emph{\indexwd{list}} of pitches
|[C,D,Ef]|, the character/number \emph{\indexwd{pair}} |('b',4)|
(lists and pairs are different in a subtle way, to be described
later), and the string |"Euterpea"|.  Each of these structured
expressions is also a value, since by themselves there is no further
calculation that can be carried out.  As another example, |1+2| is an
expression, and one step of calculation yields the expression |3|,
which is a value, since no more calculations can be performed.  As a
final example, as was expained earlier, the expression |simple 3 9 5|
evaluates to the value 42.

Sometimes, however, an expression has only a never-ending
sequence of calculations.  For example, if |x| is defined as:
\begin{spec}
x = x + 1
\end{spec}
then here is what happens when trying to calculate the value of |x|:
\begin{spec}
x 
==> x + 1
==> (x + 1) + 1
==> ((x + 1) + 1) + 1
==> (((x + 1) + 1) + 1) + 1
...
\end{spec}
Similarly, if a function |f| is defined as:
\begin{spec}
f x = f (x-1)
\end{spec}
then an expression such as |f 42| runs into a similar problem:
\begin{spec}
f 42
==> f 41
==> f 40
==> f 39
...
\end{spec}
Both of these clearly result in a never-ending sequence of
calculations.  Such expressions are said to not terminate, or
\emph{diverge}.  In such cases the symbol |bottom|\index{bottom},
pronounced ``bottom,'' is used to denote the value of the expression.
This means that every diverging computation in Haskell denotes the
same |bottom| value,\footnote{Technically, each type has its own
  version of |bottom|.} reflecting the fact that, from an observer's
point of view, there is nothing to distinguish one diverging
computation from another.

\section{Types}

Every expression (and therefore every value) also has an associated
\emph{\indexwd{type}}.  It is helpful to think of types as sets of
expressions (or values), in which members of the same set have much in
common.  Examples include the primitive atomic types
\indexwdhs{Integer} (the set of all integers) and \indexwdhs{Char}
(the set of all characters), the user-defined atomic type |PitchClass|
(the set of all pitch classes, i.e.\ note names), as well as the
structured types |[Integer]| and |[PitchClass]| (the sets of all lists
of integers and lists of pitch classes, respectively), and |String|
(the set of all Haskell strings).

The association of an expression or value with its type is very
useful, and there is a special way of expressing it in Haskell.
Using the examples of values and types above:
\begin{spec}
D           :: PitchClass
42          :: Integer
'a'         :: Char
"Euterpea"  :: String
[C,D,Ef]    :: [PitchClass]
('b',4)     :: (Char,Integer)
\end{spec}
\indexhs{::}
Each association of an expression with its type is called a \emph{type
  signature}.

\index{case sensitivity}

\syn{Note that the names of specific types are capitalized, such as
  |Integer| and |Char|, as are the names of some atomic values such as
  |D| and |Fs|.  These will never be confused in context, since things
  to the right of ``|::|'' are types, and things to the left are
  values.  Note also that user-defined names of values are \emph{not}
  capitalized, such as |simple| and |x|.  This is not just a
  convention: it is required when programming in Haskell.  In
  addition, the case of the other characters matters, too.  For
  example, |test|, |teSt|, and |tEST| are all distinct names for
  values, as are |Test|, |TeST|, and |TEST| for types.}

\syn{Literal characters are written enclosed in single forward quotes
  (apostrophes), as in |'a'|, |'A'|, |'b'|, |','|, |'!'|, |' '| (a
  space), and so on.  (There are some exceptions, however; see the
  Haskell Report for details.)  Strings are written enclosed in double
  quote characters, as in |"Euterpea"| above.  The connection between
  characters and strings will be explained in a later chapter.

The ``|::|'' should be read ``has type,'' as in ``42 has type
|Integer|.''  Note that square braces are used both to construct a
list value (the left-hand side of |(::)| above), and to describe its
type (the right-hand side above).  Analogously, the round braces used
for pairs are used in the same way.  But also note that all of the
elements in a list, however long, must have the same type, whereas the
elements of a pair can have different types.}

Haskell's \emph{type system} ensures that Haskell programs are 
\emph{\indexwd{well-typed}}; that is, that the programmer has not
mismatched types in some way.  For example, it does not make much
sense to add together two characters, so the expression |'a' + 'b'| is
\emph{\indexwd{ill-typed}}.  The best news is that Haskell's type
system will tell you if your program is well-typed \emph{before you run
  it}.  This is a big advantage, since most programming errors are
manifested as type errors.

%% The idea of dividing the world of values into types should be a
%% familiar idea to most people.  We do it all of the time for just about
%% every kind of object you can think of.  Take boxes, for example.  Just
%% as we have integers and reals, lists and tuples, etc., we also have
%% large boxes and small boxes, cardboard boxes and wooden boxes, and so
%% on.  And just as we have lists of integers and lists of characters, we
%% also have boxes of nails and boxes of shoes.  And just as we would not
%% expect to be able to take the square of a list, or add two characters,
%% we would not expect to be able to use a box to pay for our groceries.

%% Types help us to make sense of the world by organizing it into groups
%% of common shape, size, functionality, etc.  The same is true for
%% programming, where types help us to organize values into groups of
%% common shape, size, and functionality, amongst other things.  Of
%% course, the kinds of commonality between values will not be the same
%% as those between objects in the real world, and in general we will be
%% more restricted---and more formal---about just what we can say about
%% types and how we say it.

% Nevertheless, the analogy holds.

\section{Function Types and Type Signatures}

\index{function!type||(} What should the \indexwd{type} of a function
be?  It seems that it should at least convey the fact that a function
takes values of one type---|T1|, say---as input, and returns values of
(possibly) some other type---|T2|, say---as output.  In Haskell this
is written |T1 -> T2|, and such a function is said to ``map values of
type |T1| to values of type |T2|.''\footnote{In mathematics |T1| is
  called the \emph{domain} of the function, and |T2| the
  \emph{range}.}  If there is more than one argument, the notation is
extended with more arrows.  For example, if the intent is that the
function |simple| defined in the previous section has type
|Integer->Integer->Integer->Integer|, we can include a type signature
with the definition of |simple|: \index{type!signature}
\begin{spec}
simple         :: Integer -> Integer -> Integer -> Integer
simple x y z   = x*(y+z)
\end{spec}
\syn{When writing Haskell programs using a typical text editor, there
  typically will not be nice fonts and arrows as in |Integer ->
  Integer|.  Rather, you will have to type {\tt Integer -> Integer}.}

\index{type!inference} Haskell's type system also ensures that
user-supplied type signatures such as this one are correct.  Actually,
Haskell's type system is powerful enough to allow us to avoid writing
any type signatures at all, in which case the type system is said to
\emph{infer} the correct types.\footnote{There are a few exceptions to
  this rule, and in the case of |simple| the inferred type is actually
  a bit more general than that written above.  Both of these points
  will be returned to later.}  Nevertheless, judicious placement of
type signatures, as was done for |simple|, is a good habit, since type
signatures are an effective form of documentation and help bring
programming errors to light.  In fact, it is a good habit to first
write down the type of each function you are planning to define, as a
first approximation to its full specification---a way to grasp its
overall functionality before delving into its details.

\index{function!application}
The normal use of a function is referred to as \emph{function
  application}.  For example, |simple 3 9 5| is the application of
the function |simple| to the arguments 3, 9, and 5.  Some
functions, such as |(+)|, are applied using what is known as
\emph{\indexwd{infix} syntax}; that is, the function is written
between the two arguments rather than in front of them (compare
|x+y| to |f x y|).

\syn{Infix functions are often called \emph{\indexwd{operators}}, and
are distinguished by the fact that they do not contain any numbers or
letters of the alphabet.  Thus |^!| and |*#:| are infix
operators, whereas |thisIsAFunction| and |f9g| are not (but are
still valid names for functions or other values).  The only exception
to this is that the symbol \emph{'} is considered to be alphanumeric;
thus |f'| and |one's| are valid names, but not operators.

In Haskell, when referring to an infix operator as a value, it is enclosed
in parentheses, such as when declaring its type, as in:
\begin{spec}
(+) :: Integer -> Integer -> Integer
\end{spec}
Also, when trying to understand an expression such as |f x + g y|,
there is a simple rule to remember: function application \emph{always}
has ``higher \indexwd{precedence}'' than operator application, so that
|f x + g y| is the same as |(f x) + (g y)|.

Despite all of these syntactic differences, however, operators are
still just functions.}
\index{function!type||)}

\vspace{.1in}\hrule

\begin{exercise}{\em
Identify the well-typed expressions in the following, and, for each,
give its proper type:
\begin{spec}
[ A, B, C ]
[ D, 42 ]
( -42, Ef )
[ ('a',3), ('b',5) ]
simple 'a' 'b' 'c'
( simple 1 2 3, simple )
["I","love","Euterpea"]
\end{spec}
For those expressions that are ill-typed, explain why.
}
\end{exercise} 
\out{
\begin{spec}
[ (2,3), (4,5) ]
[ D, 42 ]
( Ef, -42 )
simple 'a' 'b' 'c'
( simple 1 2 3, simple )
["hello","world"]
\end{spec}
}

\vspace{.1in}\hrule

\section{Abstraction, Abstraction, Abstraction}
\label{sec:abstraction}
\index{abstraction||(}

The title of this section is the answer to the question: ``What are
the three most important ideas in programming?''  Webster defines the
verb ``abstract'' as follows:
\begin{quote}
{\bf abstract}, \emph{vt} (1) remove, separate (2) to consider apart
from application to a particular instance.
\end{quote}
In programming this happens when a pattern repeats itself and we wish
to ``separate'' that pattern from the ``particular instances'' in
which it appears.  In this textbook this process is called the
\emph{abstraction principle}.\index{abstraction!principle} The
following sections introduce several different kinds of abstraction,
using examples involving both simple numbers and arithmetic (things
everyone should be familiar with) as well as musical examples (that
are specific to Euterpea).

\subsection{Naming}
\index{abstraction!naming||(}

One of the most basic ideas in programming---for that matter, in every
day life---is to \emph{name} things.  For example, we may wish to give
a name to the value of $\pi$, since it is inconvenient to retype (or
remember) the value of $\pi$ beyond a small number of digits.  In
mathematics the greek letter $\pi$ in fact \emph{is} the name for this
value, but unfortunately we do not have the luxury of using greek
letters on standard computer keyboards and/or text editors.  So in
Haskell we write:
\begin{spec}
pi  :: Double
pi  = 3.141592653589793
\end{spec}
to associate the name |pi| with the number 3.141592653589793.  The
type signature in the first line declares |pi| to be a
\emph{double-precision floating-point number}, which mathematically
and in Haskell is distinct from an integer.\footnote{We will have more
  to say about floating-point numbers later.} Now the name |pi| can be
used in expressions whenever it is in scope; it is an abstract
representation, if you will, of the number 3.141592653589793.
Furthermore, if there is ever a need to change a named value (which
hopefully will not ever happen for |pi|, but could certainly happen
for other values), we would only have to change it in one place,
instead of in the possibly large number of places where it is used.

For a simple musical example, note first that in music theory, a
\emph{pitch} consists of a \emph{pitch class} and an \emph{octave}.
For example, in Euterpea we simply write |(A,4)| to represent the
pitch class |A| in the fourth octave.  This particular note is called
``concert A'' (because it is often used as the note to which an
orchestra tunes its instruments) or ``A440'' (because its frequency is
440 cycles per second).  Because this particular pitch is so common,
it may be desirable to give it a name, which is easily done in
Haskell, as was done above for $\pi$:
\begin{spec}
concertA, a440 :: (PitchClass, Octave)
concertA  = (A,4)  -- concert A
a440      = (A,4)  -- A440
\end{spec}

\syn{This example demonstrates the use of program {\em
    \indexwd{comments}}.  Any text to the right of ``{\tt --}'' till
  the end of the line is considered to be a programmer comment, and is
  effectively ignored.  Haskell also permits {\em nested} comments
  that have the form |{- this is a comment -}| and can appear anywhere
  in a program, including across multiple lines.}

This example demonstrates the (perhaps obvious) fact that several
different names can be given to the same value---just as your brother
John might have the nickname ``Moose.''  Also note that the name
|concertA| requires more typing than |(A,4)|; nevertheless, it has
more mnemonic value, and, if mistyped, will more likely result in a
syntax error.  For example, if you type ``|concrtA|'' by mistake, you
will likely get an error saying, ``Undefined variable,'' whereas if
you type ``|(A,5)|'' you will not.

\syn{This example also demonstrates that two names having the same
  type can be combined into the same type signature, separated by a
  comma.  Note finally, as a reminder, that these are names of values,
  and thus they both begin with a lowercase letter.}

Consider now a problem whose solution requires writing some larger
expression more than once.  For example:
\begin{spec}
x  :: Float
x  = f (pi*r**2) + g (pi*r**2)
\end{spec}

\syn{|(**)| is Haskell's floating-point exponentiation operator.  Thus
  |pi*r**2| is analogous to $\pi r^2$ in mathematics.  |(**)| has
  higher precedence than |(*)| and the other binary arithmetic
  operators in Haskell.}

Note in the definition of |x| that the expression |pi*r**2|
(presumably representing the area of a circle whose radius is |r|) is
repeated---it has two instances---and thus, applying the abstraction
principle, it can be separated from these instances.  From the
previous examples, doing this is straightforward---it is called
\emph{naming}---so we might choose to rewrite the single equation
above as two:
\begin{spec}
area  = pi*r**2
x     = f area + g area
\end{spec}
If, however, the definition of |area| is not intended for use
elsewhere in the program, then it is advantageous to ``hide'' it
within the definition of |x|.  This will avoid cluttering up the
namespace, and prevents |area| from clashing with some other value
named |area|.  To achieve this, we could simply use a \indexwdkw{let}
expression:
\begin{spec}
x =  let area = pi*r**2
     in f area + g area
\end{spec}
A {\bf let} expression restricts the \emph{visibility} of the names
that it creates to the internal workings of the {\bf let} expression
itself.  For example, if we were to write:
\begin{spec}
area  = 42
x     =  let area = pi*r**2
         in f area + g area
\end{spec}
then there is no conflict of names---the ``outer'' |area| is
completely different from the ``inner'' one enclosed in the {\bf let}
expression.  Think of the inner |area| as analogous to the first name
of someone in your household.  If your brother's name is ``John'' he
will not be confused with John Thompson who lives down the street when
you say, ``John spilled the milk.''

So you can see that naming---using either top-level equations or
equations within a {\bf let} expression---is an example of the
abstraction principle in action.  
%% It is often the case, of course, that we \emph{anticipate} the need for
%% abstraction; for example, directly writing down the final solution
%% above, because we knew that we would need to use the expression
%% |a-b+2| more than once.  
\index{abstraction!naming||)}

\syn{An equation such as |c = 42| is called a
\emph{\indexwd{binding}}.  A simple rule to remember when programming in
Haskell is never to give more than one binding for the same name in a
context where the names can be confused, whether at the top level of
your program or nestled within a |let| expression.  For example,
this is not allowed:
\begin{spec}
a  = 42
a  = 43
\end{spec}
nor is this:
\begin{spec}
a  = 42
b  = 43
a  = 44
\end{spec}
}

% (On the other hand, as you will soon see, functions can be defined
% with multiple equations, each defining the function's behavior on a
% different kind of argument.  But in such cases all of the equations
% for the same function must appear together (one after the other).)

\subsection{Functional Abstraction}
\label{sec:fun-abstract}
\index{abstraction!functional||(}

The design of functions such as |simple| can be viewed as the
abstraction principle in action.  To see this using the example above
involving the area of a circle, suppose the original program looked
like this:
\begin{spec}
x  :: Float
x  = f (pi*r1**2) + g (pi*r2**2)
\end{spec}
Note that there are now two areas involved---one of a circle whose
radius is |r1|, the other |r2|.  Now the expressions in parentheses
have a \emph{repeating pattern of operations}.  In discerning the
nature of a repeating pattern it is sometimes helpful to first identify
those things that are \emph{not} repeating, i.e.\ those things that
are \emph{changing}.  In the case above, it is the radius that is
changing.  A repeating pattern of operations can be abstracted as a
\emph{function} that takes the changing values as arguments.  Using
the function name |areaF| (for ``area function'') we can write:
\begin{spec}
x  =  let areaF r = pi*r**2
      in f (areaF r1) + g (areaF r2)
\end{spec}
This is a simple generalization of the previous example, where the
function now takes the ``variable quantity''---in this case the
radius---as an argument.  A very simple proof by calculation, in which
|areaF| is unfolded where it is used, can be given to demonstrate that
this program is equivalent to the old.

This application of the abstraction principle is called
\emph{functional abstraction}, since a sequence of operations is
abstracted as a \emph{function} such as |areaF|.  

For a musical example, a few more concepts from Euterpea are first
introduced, concepts that are addressed more formally in the next
chapter:
\begin{enumerate}
\item
In music theory a \emph{note} is a \emph{pitch} combined with a
\emph{duration}.  Duration is measured in beats, and in Euterpea has
type |Dur|.  A note whose duration is one beat is called a whole note,
one with duration $\nicefrac{1}{2}$ is called a half note, and so on.
A note in Euterpea is the smallest entity, besides a rest, that is
actually a performable piece of music, and its type is |Music Pitch|
(other variations of this type will be introduced in later chapters).

\item
In Euterpea there are functions:
\begin{spec}
note  :: Dur -> Pitch -> Music Pitch
rest  :: Dur -> Music Pitch
\end{spec}
such that |note d p| is a note whose duration is |d| and pitch is |p|,
and |rest d| is a rest with duration |d|.  For example, |note (1/4)
(A,4)| is a quarter note concert A.

\item
In Euterpea the following infix operators combine smaller |Music|
values into larger ones:
\begin{spec}
(:+:)  :: Music Pitch -> Music Pitch -> Music Pitch
(:=:)  :: Music Pitch -> Music Pitch -> Music Pitch
\end{spec}
Intuitively:
\begin{itemize}
\item |m1 :+: m2| is the music value that represents the playing of
  |m1| followed by |m2|.
\item |m1 :=: m2| is the music value that represents the playing of
  |m1| and |m2| simultaneously.
\end{itemize}

\item
Eutperepa also has a function |trans :: Int -> Pitch -> Pitch|
such that |trans i p| is a pitch that is |i| semitones (half steps, or
steps on a piano) higher than |p|.
\end{enumerate}

Now for the example.  Consider the simple melody:
\begin{spec}
note qn p1 :+: note qn p2 :+: note qn p3
\end{spec}
where |qn| is a quarter note:
\begin{spec}
qn = 1/4
\end{spec}
Suppose we wish to harmonize each note with a note played a minor
third lower.  In music theory, a minor third corresponds to three
semitones, and thus the harmonized melody can be written as:
\begin{spec}
mel =  (note qn p1 :=: note qn (trans (-3) p1)) !:+: 
       (note qn p2 :=: note qn (trans (-3) p2)) !:+: 
       (note qn p3 :=: note qn (trans (-3) p3))
\end{spec}

Note as in the previous example a repeating pattern of
operations---namely, the operations that harmonize a single note with
a note three semitones below it.  As before, to abstract a sequence of
operations such as this, a function can be defined that takes the
``variable quantities''---in this case the pitch---as arguments.  We
can take this one step further, however, by noting that in some other
context we might wish to vary the duration.  Recognizing this is to
\emph{anticipate} the need for abstraction.  Calling this function
|hNote| (for ``harmonize note'') we can then write:
\begin{spec}
hNote      :: Dur -> Pitch -> Music Pitch
hNote d p  = note d p :=: note d (trans (-3) p)
\end{spec}
There are three instances of the pattern in |mel|, each of which can
be replaced with an application of |hNote|.  This leads to:
\begin{spec}
mel  :: Music Pitch
mel  = hNote qn p1 :+: hNote qn p2 :+: hNote qn p3
\end{spec}
Again using the idea of unfolding described earlier in this chapter,
it is easy to prove that this definition is equivalent to the previous
one.

As with |areaF|, this use of |hNote| is an example of functional
abstraction.  In a sense, functional abstraction can be seen as a
generalization of naming.  That is, |area r1| is just a name for
|pi*r1**2|, |hNote d p| is just a name for |note d p :=: note d
(trans (-3) p)|, and so on.  Stated another way, named quantities
such as |area|, |pi|, |concertA|, and |a440| defined earlier can be
thought of as functions with no arguments.

Of course, the definition of |hNote| could also be hidden within |mel|
using a |let| expression as was done in the previous example:
\begin{spec}
mel  :: Music Pitch
mel  =  let hNote d p = note d p :=: note d (trans (-3) p)
        in hNote qn p1 :+: hNote qn p2 :+: hNote qn p3
\end{spec}

\subsection{Data Abstraction}
\label{sec:basic-list-abstraction}
\index{abstraction!data||(}

The value of |mel| is the sequential composition of three harmonized
notes.  But what if in another situation we must compose together
five harmonized notes, or in other situations even more?  In
situations where the number of values is uncertain, it is useful to
represent them in a \emph{data structure}.  For the example at hand, a
good choice of data structure is a \emph{\indexwd{list}}, briefly
introduced earlier, that can have any length.  The use of a data
structure motivated by the abstraction principle is one form of
\emph{data abstraction}.

Imagine now an entire list of pitches, whose length is not known at the
time the program is written.  What now?  It seems that a function is
needed to convert a list of pitches into a sequential composition of
harmonized notes.  Before defining such a function, however, there is
a bit more to say about lists.

Earlier the example |[C,D,Ef]| was given, a list of pitch classes
whose type is thus |[PitchClass]|.  A list with \emph{no} elements
is---not surprisingly---written |[]|, and is called the \emph{empty
  list}.

To add a single element |x| to the front of a list |xs|, we write
|x:xs| in Haskell.  (Note the naming convention used here; |xs| is the
plural of |x|, and should be read that way.)  For example, |C :
[D,Ef]| is the same as |[C,D,Ef]|.  In fact, this list is equivalent
to |C:(D:(Ef:[]))|, which can also be written |C:D:Ef:[]| since the
infix operator |(:)| is right associative.  \index{associativity}

\syn{In mathematics we rarely worry about whether the notation
  $a+b+c$ stands for $(a+b)+c$ (in which case $+$ would be ``left
  associative'') or $a+(b+c)$ (in which case $+$ would ``right
  associative'').  This is because in situations where the parentheses
  are left out it is usually the case that the operator is
  \emph{mathematically} associative, meaning that it does not matter
  which interpretation is chosen.  If the interpretation \emph{does}
  matter, mathematicians will include parentheses to make it clear.
  Furthermore, in mathematics there is an implicit assumption that
  some operators have higher \emph{precedence} than others; for
  example, $2\times a + b$ is interpreted as $(2\times a) + b$, not $2
  \times (a+b)$.

In many programming languages, including Haskell, each operator is
defined to have a particular precedence level and to be left
associative, right associative, or to have no associativity at all.
For arithmetic operators, mathematical convention is usually followed;
for example, |2*a+b| is interpreted as |(2*a)+b| in Haskell.  The
predefined list-forming operator |(:)| is defined to be right
associative.  Just as in mathematics, this associativity can be
overridden by using parentheses: thus |(a:b):c| is a valid Haskell
expression (assuming that it is well-typed; it must be a list of
lists), and is very different from |a:b:c|.  A way to specify the
precedence and associativity of user-defined operators will be
discussed in a later chapter.}

%% [consider eliminating the next paragraph]

%% Examples of predefined functions defined on lists in Haskell include
%% \indexwdhs{head} and \indexwdhs{tail}, which return the ``head'' and
%% ``tail'' of a list, respectively.  That is, |head (x:xs) ==> x| and
%% |tail (x:xs) ==> xs| (we will define these two functions formally
%% in Section \ref{sec:poly-types}).  Another example is the function
%% |(++)| which \emph{concatenates}, or \emph{appends}, together its two
%% list arguments.  For example, 
%% |[1,2,3] ++ [4,5,6] ==> [1,2,3,4,5,6]| (|(++)| will be defined
%% in Section \ref{sec:append}).  \indexhs{(++)}

Returning now to the problem of defining a function (call it |hList|)
to turn a list of pitches into a sequential composition of harmonized
notes, we should first express what its type should be:
\begin{spec}
hList :: Dur -> [Pitch] -> Music Pitch
\end{spec}
To define its proper behavior, it is helpful to consider, one by one,
all possible cases that could arise on the input.  First off, the list
could be empty, in which case the sequential composition should be a
|Music Pitch| value that has zero duration.  So:
\begin{spec}
hList d []     = rest 0
\end{spec}

The other possibility is that the list \emph{is not} empty---i.e.\ it
contains at least one element, say |p|, followed by the rest of the
elements, say |ps|.  In this case the result should be the
harmonization of |p| followed by the sequential composition of the
harmonization of |ps|.  Thus:
\begin{spec}
hList d (p:ps) = hNote d p :+: hList d ps
\end{spec}
Note that this part of the definition of |hList| is
\emph{recursive}---it refers to itself!  But the original
problem---the harmonization of |p:ps|---has been reduced to the
harmonization of |p| (previously captured in the function |hNote|) and
the harmonization of |ps| (a slightly smaller problem than the
original one).

Combining these two equations with the type signature yields the
complete definition of the function |hList|:
\begin{spec}
hList           :: Dur -> [Pitch] -> Music Pitch
hList d []      = rest 0
hList d (p:ps)  = hNote d p :+: hList d ps
\end{spec}
\index{pattern!matching}

Recursion is a powerful technique that will be used many times in this
textbook.  It is also an example of a general problem-solving
technique where a large problem is broken down into several smaller
but similar problems; solving these smaller problems one-by-one leads
to a solution to the larger problem.  \index{recursion}

\syn{Although intuitive, this example highlights an important aspect
of Haskell: \emph{pattern matching}.  The left-hand sides of
the equations contain \emph{patterns} such as |[]| and |x:xs|.
When a function is applied, these patterns are \emph{matched} against
the argument values in a fairly intuitive way (|[]| only matches
the empty list, and |p:ps| will successfully match any list with at
least one element, while naming the first element |p| and the rest
of the list |ps|).  If the match succeeds, the right-hand side is
evaluated and returned as the result of the application.  If it fails,
the next equation is tried, and if all equations fail, an error
results.  All of the equations that define a particular function must
appear together, one after the other.

Defining functions by pattern matching is quite common in Haskell, and
you should eventually become familiar with the various kinds of
patterns that are allowed; see Appendix \ref{ch:patterns} for a
concise summary.}  \index{pattern}

%% This is called a \emph{recursive} function definition since |hList|
%% ``refers to itself'' on the right-hand side of the second equation.

Given this definition of |hList| the definition of |mel| can be
rewritten as:
\begin{spec}
mel = hList qn [p1, p2, p3]
\end{spec}

We can prove that this definition is equivalent to the old via
calculation:
\begin{spec}
mel = hList qn [p1, p2, p3]
==> hList qn (p1:p2:p3:[])
==> hNote qn p1 :+: hList qn (p2:p3:[])
==> hNote qn p1 :+: hNote qn p2 :+: hList qn (p3:[])
==> hNote qn p1 :+: hNote qn p2 :+: hNote qn p3 :+: hList qn []
==> hNote qn p1 :+: hNote qn p2 :+: hNote qn p3 :+: rest 0
\end{spec}
The first step above is not really a calculation, but rather a
rewriting of the list syntax.  The remaining calculations each
represent an unfolding of |hList|. 
\index{abstraction!data||)}
\index{abstraction||)}

Lists are perhaps the most commonly used data structure in Haskell,
and there is a rich library of functions that operate on them.  In
subsequent chapters lists will be used in a variety of interesting
computer music applications.

\vspace{.1in}\hrule

\begin{exercise}{\em
Modify the definitions of |hNote| and |hList| so that they each take
an extra argument that specifies the interval of harmonization (rather
than being fixed at -3).  Rewrite the definition of |mel| to take these
changes into account.}
\end{exercise}

\vspace{.1in}\hrule

\section{Haskell Equality vs.\ Euterpean Equality}

The astute reader will have objected to the proof just completed,
arguing that the original version of |mel|:
\begin{spec}
hNote qn p1 :+: hNote qn p2 :+: hNote qn p3
\end{spec}
is not the same as the terminus of the above proof:
\begin{spec}
hNote qn p1 :+: hNote qn p2 :+: hNote qn p3 :+: rest 0
\end{spec}
Indeed, that reader would be right!  As Haskell values, these
expressions are \emph{not} equal, and if you printed each of them you
would get different results.  So what happened?  Did proof by
calculation fail?

No, proof by calculation did not fail, since, as just pointed out,
as Haskell values these two expressions are not the same, and proof by
calculation is based on the equality of Haskell values.  The problem
is that a ``deeper'' notion of equivalence is needed, one based on the
notion of \emph{musical} equality.  Adding a rest of zero duration to
the beginning or end of any piece of music should not change what we
\emph{hear}, and therefore it seems that the above two expressions are
\emph{musically} equivalent.  But it is unreasonable to expect Haskell
to figure this out for the programmer!

As an analogy, consider the use of an ordered list to represent a set
(which is unordered).  The Haskell values |[x1,x2]| and |[x2,x1]| are
not equal, yet in a program that ``interprets'' them as sets, they
\emph{are} equal.

The way this problem is approached in Euterpea is to formally define a
notion of \emph{musical interpretation}, from which the notion
of \emph{musical equivalence} is defined.  This leads to a kind of
``algebra of music'' that includes, among others, the following axiom:
\begin{spec}
m :+: rest 0 === m
\end{spec}
The operator |(===)| should be read, ``is musically equivalent to.''
With this axiom it is easy to see that the original two expressions
above \emph{are} in fact musically equivalent.

For a more extreme example of this idea, and to entice the reader to
learn more about musical equivalence in later chapters, note that
|mel|, given pitches |p1 = Ef|, |p2 = F|, |p3 = G|, and duration |d =
1/4|, generates the harmonized melody shown in Figure \ref{fig:mel};
we can write this concretely in Euterpea as:
\begin{spec}
mel1 =  (note (1/4) (Ef,  4) :=: note (1/4) (C,4)) !:+:
        (note (1/4) (F,   4) :=: note (1/4) (D,4)) !:+:
        (note (1/4) (G,   4) :=: note (1/4) (E,4))
\end{spec}
The definition of |mel1| can then be seen as a \emph{polyphonic}
interpretation of the musical phrase in Figure \ref{fig:mel}, where
each pair of notes is seen as a harmonic unit.  In contrast, a
\emph{contrapuntal} interpretation sees two independent \emph{lines},
or \emph{voices}, in this case the line $\langle$E$\flat$,F,G$\rangle$
and the line $\langle$C,D,E$\rangle$.  In Euterpea we can write this
as:
\begin{spec}
mel2 =  (note (1/4) (Ef,  4) :+: note (1/4) (F,4)   :+: note (1/4) (G,4))
        :=:
        (note (1/4) (C,   4) :+: note (1/4) (D,4)   :+: note (1/4) (E,4))
\end{spec}
|mel1| and |mel2| are clearly not equal as Haskell values.  Yet if
they are played, they will \emph{sound} the same---they are, in the
sense described earlier, \emph{musically equivalent}.  But proving
these two phrases musically equivalent will require far more than a
simple axiom involving |rest 0|.  In fact this can be done in an
elegant way, using the algebra of music developed in Chapter
\ref{ch:algebra}.

\begin{figure*}
%% \includegraphics{pics/threenoteharm.pdf}
\centerline{
\epsfysize=0.6in 
\epsfbox{pics/threenoteharm.eps}
}
\caption{Polyphonic vs.\ Contrapuntal Interpretation}
\label{fig:mel}
\end{figure*}

\section{Code Reuse and Modularity}
\label{sec:code-reuse}

There does not seem to be much repetition in the last definition of
|hList|, so perhaps the end of the abstraction process has been
reached.  In fact, it is worth considering how much progress has been
made.  The original definition:
\begin{spec}
mel =  (note qn p1 :=: note qn (trans (-3) p1)) !:+: 
       (note qn p2 :=: note qn (trans (-3) p2)) !:+: 
       (note qn p3 :=: note qn (trans (-3) p3))
\end{spec}
was replaced with:
\begin{spec}
mel = hList qn [p1, p2, p3]
\end{spec}
But additionally, definitions for the auxiliary functions |hNote| and
|hList| were introduced:
\begin{spec}
hNote      :: Dur -> Pitch -> Music Pitch
hNote d p  = note d p :=: note d (trans (-3) p)

hList           :: Dur -> [Pitch] -> Music Pitch
hList d []      = rest 0
hList d (p:ps)  = hNote d p :+: hList d ps
\end{spec}
In terms of code size, the final program is actually larger than the
original!  So has the program improved in any way?

Things have certainly gotten better from the standpoint of ``removing
repeating patterns,'' and we could argue that the resulting program
therefore is easier to understand.  But there is more.  Now that
auxiliary functions such as |hNote| and |hList| have been defined, we
can \emph{reuse} them in other contexts.  Being able to reuse code is
also called \emph{\indexwd{modularity}}, since the reused components
are like little modules, or building blocks, that can form the
foundation of many applications.\footnote{``Code reuse'' and
  ``modularity'' are important software engineering principles.}  In a
later chapter, techniques will be introduced---most notably,
\emph{higher-order functions} and \emph{polymorphism}---for improving
the modularity of this example even more, and substantially increasing
the ability to reuse code.

\section{[Advanced] Programming with Numbers}
\label{sec:numbers-caveat}
\index{number systems||(}

In computer music programming, it is often necessary to program with
numbers.  For example, it is often convenient to represent pitch on a
simple absolute scale using integer values.  And when computing with
analog signals that represent a particular sound wave, it is necessary
to use floating point numbers as an approximation to the reals.  So it
is a good idea to understand precisely how numbers are represented
inside a computer, and within a particular language such as Haskell.

In mathematics there are many different kinds of number systems.  For
example, there are integers, natural numbers (i.e.\ non-negative
integers), real numbers, rational numbers, and complex numbers.  These
number systems possess many useful properties, such as the fact that
multiplication and addition are commutative, and that multiplication
distributes over addition.  You have undoubtedly learned many of these
properties in your studies, and have used them often in algebra,
geometry, trigonometry, physics, and so on.

Unfortunately, each of these number systems places great demands on
computer systems.  In particular, a number can in general require an
\emph{arbitrary amount of memory} to represent it.  Clearly, for
example, an irrational number such as $\pi$ cannot be represented
exactly; the best we can do is approximate it, or possibly write a
program that computes it to whatever (finite) precision is needed in a
given application.  But even integers (and therefore rational numbers)
present problems, since any given integer can be arbitrarily large.

Most programming languages do not deal with these problems very well.
In fact, most programming languages do not have exact forms of many of
these number systems.  Haskell does slightly better than most, in that
it has exact forms of integers (the type \indexwdhs{Integer}) as well
as rational numbers (the type \indexwdhs{Rational}, defined in the
Ratio Library).  But in Haskell and most other languages there is no
exact form of real numbers, for example, which are instead
approximated by \emph{floating-point numbers} with either single-word
precision (\indexwdhs{Float} in Haskell) or double-word precision
(\indexwdhs{Double}).  Even worse, the behavior of arithmetic
operations on floating-point numbers can vary somewhat depending on
what kind of computer is being used, although hardware standardization
in recent years has reduced the degree of this problem.

The bottom line is that, as simple as they may seem, great care must
be taken when programming with numbers.  Many computer errors, some
quite serious and renowned, were rooted in numerical incongruities.
The field of mathematics known as \emph{ \indexwd{numerical analysis}} is
concerned precisely with these problems, and programming with
floating-point numbers in sophisticated applications often requires a
good understanding of numerical analysis to devise proper algorithms
and write correct programs.

As a simple example of this problem, consider the distributive law,
expressed here as a calculation in Haskell, and used earlier in
this chapter in calculations involving the function |simple|:
\begin{spec}
a*(b+c) ==> a*b + a*c
\end{spec}
For most floating-point numbers, this law is perfectly valid.  For
example, in the GHC implementation of Haskell, the expressions
|pi*(3+4) :: Float| and |pi*3+pi*4 :: Float| both yield the same
result: 21.99115.  But funny things can happen when the magnitude of
|b+c| differs significantly from the magnitude of either |b| or
|c|.  For example, the following two calculations are from GHC:
\begin{spec}
5*(-0.123456  +    0.123457)  :: Float ==> 4.991889e-6
5*(-0.123456) + 5*(0.123457)  :: Float ==> 5.00679e-6
\end{spec}
Although the discrepancy here is small, its very existence is
worrisome, and in certain situations it could be disastrous.  The
precise behavior of floating-point numbers will not be discussed
further in this textbook.  Just remember that they are
\emph{approximations} to the real numbers.  If real-number accuracy is
important to your application, further study of the nature of
floating-point numbers is probably warranted.

On the other hand, the distributive law (and many others) is valid in
Haskell for the exact data types |Integer| and |Ratio Integer|
(i.e.\ rationals).  Although the representation of an |Integer| in
Haskell is not normally something to be concerned about, it should be
clear that the representation must be allowed to grow to an arbitrary
size.  For example, Haskell has no problem with the following number:
\begin{spec}
veryBigNumber  :: Integer
veryBigNumber  = 43208345720348593219876512372134059
\end{spec}
and such numbers can be added, multiplied, etc.\ without any loss of
accuracy.  However, such numbers cannot fit into a single word of
computer memory, most of which are limited to 32 or 64 bits.  Worse,
since the computer system does not know ahead of time exactly how many
words will be required, it must devise a dynamic scheme to allow just
the right number of words to be used in each case.  The overhead of
implementing this idea unfortunately causes programs to run slower.

For this reason, Haskell (and most other languages) provides another
integer data type called \indexwdhs{Int} that has maximum and minimum
values that depend on the word-size of the particular computer being
used.  In other words, every value of type |Int| fits into one word of
memory, and the primitive machine instructions for binary numbers can
be used to manipulate them efficiently.\footnote{The Haskell Report
  requires that every implementation support |Int|s at least in the
  range $-2^{29}$ to $2^{29}-1$, inclusive.  The GHC implementation
  running on a 32-bit processor, for example, supports the range
  $-2^{31}$ to $2^{31}-1$.}  Unfortunately, this means that
\emph{overflow} or \emph{underflow} errors could occur when an |Int|
value exceeds either the maximum or minimum values.  Sadly, most
implementations of Haskell (as well as most other languages) do not
tell you when this happens.  For example, in GHC running on a 32-bit
processor, the following |Int| value:
\begin{spec}
i  :: Int
i  = 1234567890
\end{spec}
works just fine, but if you multiply it by two, GHC returns the value
|-1825831516|!  This is because twice |i| exceeds the maximum allowed
value, so the resulting bits become nonsensical,\footnote{Actually,
  these bits are perfectly sensible in the following way: the 32-bit
  binary representation of |i| is 01001001100101100000001011010010,
  and twice that is 10010011001011000000010110100100.  But the latter
  number is seen as negative because the 32nd bit (the highest-order
  bit on the CPU on which this was run) is a one, which means it is a
  negative number in ``twos-complement'' representation.  The
  twos-complement of this number is in turn
  01101100110100111111101001011100, whose decimal representation is
  1825831516.} and are interpreted in this case as a negative number
of the given magnitude.

This is alarming!  Indeed, why should anyone ever use |Int| when
|Integer| is available?  The answer, as implied earlier, is
efficiency, but clearly care should be taken when making this choice.
If you are indexing into a list, for example, and you are confident
that you are not performing index calculations that might result in
the above kind of error, then |Int| should work just fine, since a
list longer than $2^{31}$ will not fit into memory anyway!  But if you
are calculating the number of microseconds in some large time
interval, or counting the number of people living on earth, then
|Integer| would most likely be a better choice.  Choose your number
data types wisely!

In this textbook the numeric data types |Integer|, |Int|, |Float|,
|Double|, |Rational|, and |Complex| will be  used for a variety of
different applications; for a discussion of the other number types,
consult the Haskell Report.  As these data types are used, there will
be little discussion about their properties---this is not, after all,
a book on numerical analysis---but a warning will be cast whenever
reasoning about, for example, floating-point numbers, in a way that
might not be technically sound.  \index{number systems||)}

%% ---

\out{
\section{Qualified Types}
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
different addition function for each type of number that we wish to
add, since that would require giving each a different name, such as
|addInteger|, |addRational|, |addFloat|, etc.  And, unfortunately,
giving |(+)| a type such as |a->a->a| will not work, since this would
imply that we could add things other than numbers, such as
characters, pitch classes, lists, tuples, functions, and any type that
you might define on your own!

\index{type!qualified} \index{class} 

Haskell provides a solution to this problem through the use of {\em
  qualified types}.  Conceptually, it is helpful to think of a
qualified type just as a polymorphic type, except that in place of
``\emph{for all} types |a|'' it will be possible to say ``for all
types |a| \emph{that are members of the type class} |C|,'' where the
type class |C| can be thought of as a set of types.  For example,
suppose there is a type class \indexwdhs{Num} with members |Integer|,
|Rational|, and |Float|.  Then an accurate type for |(+)| is
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

\begin{figure}
\begin{tabular}{||||l||l||l||||} \hline
{\bf Type}  & {\bf Key} & {\bf Key} \\
{\bf Class} & {\bf functions} & {\bf instances} \\
\hline
|Num|  & |(+),(-),(*) :: Num a => a->a->a| & |Integer, Int, Float, Double,| \\ 
       & |negate :: Num a => a->a|         & |Rational| \\

\hline
|Eq|   & |(==),(/=) :: Eq a => a->a->Bool| & |Integer, Int, Float, Double,| \\
&&                                           |Rational, Char, Bool, ... | \\
\hline
|Ord|  & |(>),(<),(>=),(<=) ::|            & |Integer, Int, Float, Double,| \\
       & \ \ \ \ |Ord a => a->a->Bool|     & |Rational, Char, Bool, ... | \\
       & |max,min :: Ord a => a->a->Bool|  & \\
\hline
|Enum| & |succ,pred :: Enum a => a->a|     & |Integer, Int, Float, Double,| \\
       & also enables arithmetic sequences & |Rational, Char, Bool, ... | \\
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

Recall now the type signature previously given for |simple|:
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
Haskell, and, as you will soon see, provides great expressiveness.  In
particular, you will see that it is possible to define your own type
class and its members.  For now, all you need to know is that some
functions and operators are predefined to be instances of certain
type classes, such as |(+)| and |(*)| above.  Table
\ref{fig:common-type-classes} shows a number of others.  For example,
the |Show| class allows us to convert values to strings:
\begin{spec}
show D               ==> "D"
show concertA        ==> "(A,4)"
show (simple 3 9 5)  ==> "42"
\end{spec}
The |Read| class allows us to go the other way around:
\begin{spec}
read "D"             ==> D
read "(A,4)"         ==> (A,4)
read "42"            ==> 42
\end{spec}
The |Eq| class allows testing values for equality:
\begin{spec}
simple 3 9 5 == 42   ==> True
concertA == (A,5)    ==> False
\end{spec}
And the |Ord| class has relational operators for types whose values
can be ordered:
\begin{spec}
simple 3 9 5 > 41    ==> True
max 42 27            ==> 42
'a' < 'b'            ==> True
\end{spec}
The |Enum| class allows us to use \emph{arithmetic sequences}, which
will be explained in a later chapter.
}
