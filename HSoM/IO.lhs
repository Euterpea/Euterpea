%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Basic Input/Output}
\label{ch:IO}

\begin{code}

\end{code}

So far the only input/output (IO) that we have seen in Euterpea is the
use of the |play| function to generate the MIDI output corresponding
to a |Music| value.  But we have said very little about the |play|
function itself.  What is its type?  How does it work?  How does one
do IO in a purely functional language such as Haskell?  Our goal in
this chapter is to answer these questions.  Then in
Chapter~\ref{ch:MUI} we will describe an elegant way to do IO
involving a ``musical user interface,'' or \emph{MUI}.

%% In doing so we will introduce a key idea in Haskell, namely
%% \emph{monads}.

\section{IO in Haskell}
\label{sec:IO}
\index{action (IO)}

The Haskell Report defines the result of a program to be the value of
the variable \indexwdhs{main} in the module \indexwdhs{Main}.  This is
a mere technicality, however, only having relevance when you compile a
program as a stand-alone executable (see the GHC documentation for a
discussion of how to do that).

The way most people run Haskell programs, especially during program
development, is through the GHCi command prompt.  As you know, the
GHCi implementation of Haskell allows you to type whatever expression
you wish to the command prompt, and it will evaluate it for you.  

In both cases, the Haskell system ``executes a program'' by evaluating
an expression, which (for a well-behaved program) eventually yields a
value.  The system must then display that value on your computer
screen in some way that makes sense to you.  GHC does this by
insisting that the type of the value be an instance of the |Show|
class---in which case it ``shows'' the result by converting it to a
string using the |show| function (recall the discussion in Section
\ref{sec:qualified-types}).  So an integer is printed as an integer, a
string as a string, a list as a list, and so on.  We will refer to the
area of the computer screen where this result is printed as the {\em
  standard output area}, which may vary from one implementation to
another.

But what if a program is intended to write to a file?  Or print a file
on a printer?  Or, the main topic of this book, to play some music
through the computer's sound card, or an external MIDI device?  These
are examples of {\em output}, and there are related questions about
{\em input}: for example, how does a program receive input from the
computer keyboard or mouse, or receive input from a MIDI keyboard?

In general, how does Haskell's ``expression-oriented'' notion of
``computation by calculation'' accommodate these various kinds of
input and output?

The answer is fairly simple: in Haskell there is a special kind of
value called an {\em action}.  When a Haskell system evaluates an
expression that yields an action, it knows not to try to display the
result in the standard output area, but rather to ``take the
appropriate action.''  There are primitive actions---such as writing a
single character to a file or receiving a single character from a MIDI
keyboard---as well as compound actions---such as printing an entire
string to a file or playing an entire piece of music.  Haskell
expressions that evaluate to actions are commonly called {\em
  commands}.

%% since they command the Haskell system to perform some kind of action.
%% Haskell functions that yield actions when they are applied are also
%% commonly called commands.

% \footnote{The Haskell Report does not use the terms ``action'' or
% ``command'' to describe IO, but I find that using these special names
% helps clarify the presentation.}

\indexhs{IO} \indexhs{return}

Some commands return a value for subsequent use by the program: a
character from the keyboard, for instance.  A command that returns a
value of type |T| has type |IO T|.  If no useful value is returned,
the command has type |IO ()|.  The simplest example of a command is
|return x|, which for a value |x :: T| immediately returns |x| and has
type |IO T|.

\index{unit type} \index{()}
\syn{The type |()| is called the {\em unit type}, and has exactly
one value, which is also written |()|.  Thus |return ()| has
type |IO ()|, and is often called a ``noop'' because it is an
operation that does nothing and returns no useful result.  Despite the
negative connotation, it is used quite often!}

Remember that all expressions in Haskell must be well-typed before a
program is run, so a Haskell implementation knows ahead of time, by
looking at the type, that it is evaluating a command, and is thus
ready to ``take action.''

\section{|do| Syntax}

To make these ideas clearer, let's consider a few examples.  One
useful IO command is \indexwdhs{putStr}, which prints a string
argument to the standard output area, and has type 
|String -> IO ()|.  The |()| simply indicates that there is no
useful result returned from this action; its sole purpose is to print
its argument to the standard output area.  So the program:
\begin{spec}
module Main where
main = putStr "Hello World\n"
\end{spec}
is the canonical ``Hello World'' program that is often the first
program that people write in a new language.

Suppose now that we want to perform {\em two} actions, such as first
writing to a file named |"testFile.txt"|, then printing to the
standard output area.  Haskell has a special keyword, |do|, to
denote the beginning of a sequence of commands such as this, and so we
can write: \indexkw{do}
\begin{spec}
do  writeFile "testFile.txt" "Hello File System"
    putStr "Hello World\n"
\end{spec}
where the file-writing function \indexwdhs{writeFile} has type:
\begin{spec}
writeFile      :: FilePath -> String -> IO ()
type FilePath  = String
\end{spec}

\indexhs{FilePath}
\syn{A |do| expression allows one to sequence an arbitrary number of
commands, each of type |IO ()|, using layout to distinguish them
(just as in a |let| or |where| expression).  When used in this
way, the result of a |do| expression also has type |IO ()|.}

So far we have only used actions having type |IO ()|; i.e.\ output
actions.  But what about input?  As above, we will consider input from
both the user and the file system.

To receive a line of input from the user (which will be typed in the
{\em standard input area} of the computer screen, usually the same as
the standard output area) we can use the function:
\begin{spec}
getLine :: IO String
\end{spec}
\indexhs{getLine}
Suppose, for example, that we wish to read a line of input using this
function, and then write that line (a string) to a file.  To do this
we write the compound command:
\begin{spec}
do  s <- getLine
    writeFile "testFile.txt" s
\end{spec}

\syn{Note the syntax for binding |s| to the result of executing the
  |getLine| command---when doing this in your program, you will have
  to type {\tt <-}.  Since the type of |getLine| is |IO String|, the
  type of |s| is |String|.  Its value is then used in the next line as
  an argument to the |writeFile| command.}

Similarly, we can read the entire contents of a file using the command
|readFile :: FilePath -> IO String|, and then print the result to
standard output:
\begin{spec}
do  s <- readFile "testFile.txt"
    putStr s
\end{spec}

\syn{Any type that is an instance of the |Monad| type class can be
  used with the |do| syntax to sequence actions.  The |Monad| class is
  discussed in detail in Chapter \ref{ch:monads}.  It suffices to say
  for now that the |IO| type is an instance of the |Monad| class.}

\section{Actions are Just Values}
\label{sec:actions-are-value}

There are many other commands available for file, system, and user IO,
some in the Standard Prelude, and some in various libraries (such as
|IO|, |Directory|, |System|, and |Time|).  We will not discuss many of
these here, other than the MIDI IO commands described in
Section~\ref{sec:midi-io}.

Before that, however, we wish to emphasize that, despite the special
|do| syntax, Haskell's IO commands are no different in status from
any other Haskell function or value.  For example, it is possible to
create a {\em list} of actions, such as:
\begin{spec}
actionList = [  putStr "Hello World\n",
                writeFile "testFile.txt" "Hello File System",
                putStr "File successfully written." ]
\end{spec}
However, a list of actions is just a list of values: they actually do
not {\em do} anything until they are sequenced appropriately using a
|do| expression, and then returned as the value of the overall program
(either as the variable |main| in the module |Main|, or typed at the
GHCi prompt).  Still, it is often convenient to place actions into a
list as above, and the Haskell provides some useful functions for
turning them into single commands.  In particular, the function
\indexhs{sequence\_} |sequence_| in the Standard Prelude, when used
with IO, has type:
\begin{spec}
sequence_ :: [IO a] -> IO ()
\end{spec}
and can thus be applied to the |actionList| above to yield the
single command:
\begin{spec}
main :: IO ()
main = sequence_ actionList
\end{spec}

For a more interesting example of this idea, we first note that
Haskell's strings are really just {\em lists of characters}.  Indeed,
|String| is a type synonym for a list of characters:
\begin{spec}
type String = [Char]
\end{spec}
Because strings are used so often, Haskell allows you to write
|"Hello"| instead of |['H', 'e', 'l', 'l', 'o']|.  But keep in
mind that this is just syntax---strings really are just lists of
characters, and these two ways of writing them are identical from
Haskell's perspective.

(Earlier the type synonym |FilePath| was defined for |String|.  This
shows that type synonyms can be created using other type synonyms.)

Now back to the example.  From the function |putChar :: Char -> IO ()|, 
which prints a single character to the standard output area, we can
define the function |putStr| used earlier, which prints an entire
string.  To do this, let's first define a function that converts a
list of characters (i.e.\ a string) into a list of IO actions:
\indexhs{putCharList}
%% \begin{spec}
%% putCharList       :: String -> [IO ()]
%% putCharList []     = []
%% putCharList (c:cs) = putChar c : putCharList cs
%% \end{spec}
\begin{spec}
putCharList  :: String -> [IO ()]
putCharList  = map putChar
\end{spec}

With this, \indexwdhs{putStr} is easily defined:
\begin{spec}
putStr  :: String -> IO ()
putStr  = sequence_ . putCharList
\end{spec}
Or, more succinctly:
\begin{spec}
putStr  :: String -> IO ()
putStr  = sequence_ . map putStr
\end{spec}

%% Note that the expression |putCharList s| is a list of actions, and
%% |sequence_| is used to turn them into a single (compound) command,
%% just as we did earlier.

Of course, |putStr| can also be defined directly as a recursive
function, which we do here just to emphasize that actions are just
values, so we can use all of the functional programming skills that we
normally use:
\begin{spec}
putStr         :: String -> IO ()
putStr []      = return ()
putStr (c:cs)  = do  putChar c
                     putStr cs
\end{spec}

IO processing in Haskell is consistent with everything we have learned
about programming with expressions and reasoning through calculation,
although that may not be completely obvious yet.  Indeed, it turns out
that a |do| expression is just syntax for a more primitive way of
combining actions using functions, namely a \emph{monad}, to be
revealed in full in Chapter \ref{ch:monads}.

\section{Reading and Writing MIDI Files}
\label{sec:midi-io}

[TODO: Explain MIDI-file IO functions defined in |Codec.Midi|,
as well as the Euterpea functions for writing MIDI files.]
