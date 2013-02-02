%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

\chapter{Preface}

In 2000 I wrote a book called \emph{The Haskell School of
  Expression -- Learning Functional Programming through Multimedia}
\cite{soe}.  In that book I used graphics, animation, music, and
robotics as a way to motivate learning how to program, and
specifically how to learn \emph{functional programming} using Haskell,
a purely functional programming language.  Haskell \cite{haskell98} is
quite a bit different from conventional imperative or object-oriented
languages such as C, C$++$, Java, C\#, and so on.  It takes a different
mind-set to program in such a language, and appeals to the
mathematically inclined and to those who seek purity and elegance in
their programs.  Although Haskell was designed over twenty years ago,
it has only recently begun to catch on in a significant way, not just
because of its purity and elegance, but because with it you can solve
real-world problems quickly and efficiently, and with great economy of
code.

I have also had a long, informal, yet passionate interest in music,
being an amateur jazz pianist and having played in several bands over
the years.  About fifteen years ago, in an effort to combine work with
play, I and my students wrote a Haskell library called \emph{Haskore}
for expressing high-level computer music concepts in a purely
functional way \cite{haskore,haskore-tutorial,haskore-fop}.  Indeed,
three of the chapters in \emph{The Haskell School of Expression}
summarize the basic ideas of this work.  Soon after that, with the
help of another student, I designed a Haskell library called
\emph{HasSound} that was, essentially, a Haskell interface to
\emph{csound} \cite{csound} for doing sound synthesis and instrument
design.

Thus, when I recently became responsible for the Music Track in the
new \emph{Computing and the Arts} major at Yale, and became
responsible for teaching not one, but two computer music courses in
the new curriculum, it was natural to base the course material on
Haskell.  This current book is a rewrite of \emph{The Haskell School
  of Expression} with a focus on computer music, based on, and greatly
improving upon, the ideas in Haskore and HasSound.  The new Haskell
library that incorporates all of this is called \emph{Euterpea}.

\index{Curry, Haskell B.}  Haskell was named after the logician
Haskell B.\ Curry who, along with Alonzo Church, helped establish
the theoretical foundations of functional programming in the 1940's,
when digital computers were mostly just a gleam in researchers' eyes.
A curious historical fact is that Haskell Curry's father, Samuel Silas
Curry, helped found and direct a school in Boston called the {\em
  School of Expression}.  (This school eventually evolved into what is
now {\em Curry College}.)  Since pure functional programming is
centered around the notion of an {\em expression}, I thought that {\em
  The Haskell School of Expression} would be a good title for my first
book.  And it was thus quite natural to choose \emph{The Haskell
  School of Music} for my second!

\section*{How To Read This Book}

As mentioned earlier, there is a certain mind-set, a certain viewpoint
of the world, and a certain approach to problem solving that
collectively work best when programming in Haskell (this is true for
any programming paradigm).  If you teach only Haskell language details
to a C programmer, he or she is likely to write ugly, incomprehensible
functional programs.  But if you teach how to think differently, how
to see problems in a different light, functional solutions will come
easily, and elegant Haskell programs will result.  As Samuel Silas
Curry once said:
\begin{quote}
All expression comes {\em from within outward}, from the center to the
surface, from a hidden source to outward manifestation.  The study of
expression as a natural process brings you into contact with cause and
makes you feel the source of reality.
\end{quote}
What is especially beautiful about this quote is that music is also a
form of expression, although Curry was more likely talking about
writing and speaking.  In addition, as has been noted by many, music
has many ties to mathematics.  So for me, combining the elegant
mathematical nature of Haskell with that of music is as natural as
singing a nursery tune.

Using a high-level language to express musical ideas is, of course,
not new.  But Haskell is unique in its insistence on purity (no side
effects), and this alone makes it particularly suitable for expressing
musical ideas.  By focusing on \emph{what} a musical entity is rather
than on \emph{how} to create it, we allow musical ideas to take their
natural form as Haskell expressions.  Haskell's many abstraction
mechanisms allow us to write computer music programs that are elegant,
concise, yet powerful.  We will consistently attempt to let the music
express itself as naturally as possible, without encoding it in terms
of irrelevant language details.

Of course, my ultimate goal is not just to teach computer music
concepts.  Along the way you will also learn Haskell.  There is no
limit to what one might wish to do with computer music, and therefore
the better you are at programming, the more success you will have.
This is why I think that many languages designed specifically for
computer music---although fun to work with, easy to use, and cute in
concept---face the danger of being too limited in expressiveness.

You do not need to know much, if any, music theory to read this book,
and you do not need to play an instrument.  Of course, the more you
know about music, the more you will be able to apply the concepts
learned in this text in musically creative ways.

My general approach to introducing computer music concepts is to first
provide an intuitive explanation, then a mathematically rigorous
definition, and finally fully executable Haskell code.  
% It will often be the case that there is a close correspondence
% between the mathematical definition and the Haskell code.
In the process I introduce Haskell features as they are needed, rather
than all at once.  I believe that this interleaving of concepts and
applications makes the material easier to digest.

Another characteristic of my approach is that I do not hide any
details---I want Euterpea to be as transparent as possible!  There are
no magical built-in operations, no special computer music commands or
values.  This works out well for several reasons.  First, there is in
fact nothing ugly or difficult to hide---so why hide anything at all?
Second, by reading the code, you will better and more quickly
understand Haskell.  Finally, by stepping through the design process
with me, you may decide that you prefer a different approach---there
is, after all, no One True Way to express computer music ideas.  I
expect that this process will position you well to write rich,
creative musical applications on your own.

I encourage the seasoned programmer having experience only with
conventional imperative and/or object-oriented languages to read this
text with an open mind.  Many things will be different, and will
likely feel awkward.  There will be a tendency to rely on old habits
when writing new programs, and to ignore suggestions about how to
approach things differently.  If you can manage to resist those
tendencies I am confident that you will have an enjoyable learning
experience.  Those who succeed in this process often find that many
ideas about functional programming can be applied to imperative and
object-oriented languages as well, and that their imperative coding
style changes for the better.

I also ask the experienced programmer to be patient while in the
earlier chapters I explain things like ``syntax,'' ``operator
precedence,'' etc., since it is my goal that this text should be
readable by someone having only modest prior programming experience.
With patience the more advanced ideas will appear soon enough.

If you are a novice programmer, I suggest taking your time with the
book; work through the exercises, and don't rush things.  If, however,
you don't fully grasp an idea, feel free to move on, but try to
re-read difficult material at a later time when you have seen more
examples of the concepts in action.  For the most part this is a
``show by example'' textbook, and you should try to execute as many of
the programs in this text as you can, as well as every program that
you write.  Learn-by-doing is the corollary to show-by-example.

Finally, I note that some section titles are prefaced with the
parenthetical phrase, ``{\bf [Advanced]}''.  These sections may be
skipped upon first reading, especially if the focus is on learning
computer music concepts, as opposed to programming concepts.

\section*{Haskell Implementations}

There are several implementations of Haskell, all available free on
the Internet through the Haskell users' website at
\url{http://haskell.org}.  However, the one that has dominated all
others, and on which Euterpea is based, is \emph{\indexwd{GHC}}, an
easy-to-use and easy-to-install Haskell compiler and interpreter (see
\url{http://haskell.org/ghc}).  GHC runs on a variety of platforms,
including PC's, various flavors of Unix, and Macs.  The preferred way
to install GHC is through the \emph{Haskell Platform}
(\url{http://hackage.haskell.org/platform/}).  Any text editor can be
used to create source files, but I prefer to use emacs (see
\url{http://www.gnu.org/software/emacs}), along with its Haskell mode
(see \url{http://projects.haskell.org/haskellmode-emacs/}).  The
entire Euterpea library, including the source code from this textbook,
and installation instructions, can be found at
\url{http://haskell.cs.yale.edu}.

\newpage

\section*{Acknowledgements}

I wish to thank my funding agencies---the National Science Foundation,
the Defense Advanced Research Projects Agency, and Microsoft
Research---for their generous support of research that contributed to
the foundations of Euterpea.  Yale University has provided me a
stimulating and flexible environment to pursue my dreams for over
thirty years, and I am especially thankful for its recent support of
the Computing and the Arts initiative.

Tom Makucevich, a talented computer music practitioner and composer in
New Haven, was the original motivator, and first user, of Haskore,
which preceded Euterpea.  Watching him toil endlessly with low-level
csound programs was simply too much for me to bear!  Several
undergraduate students at Yale contributed to the original design and
implementation of Haskore.  I would like to thank in particular the
contributions of Syam Gadde and Bo Whong, who co-authored the original
paper on Haskore.  Additionally, Matt Zamec helped me greatly in the
creation of HasSound.

I wish to thank my more recent graduate students, in particular Hai
(Paul) Liu, Eric Cheng, Donya Quick, and Daniel Winograd-Cort for
their help in writing much of the code that constitutes the current
Euterpea library.  In addition, many students in my computer music
classes at Yale provided valuable feedback through earlier drafts of
the manuscript.

% Also thanks to Serge Lehuitouze and ... for their comments on the
% text.

Finally, I wish to thank my wife, Cathy Van Dyke, my best friend and
ardent supporter, whose love, patience, and understanding have helped
me get through some bad times, and enjoy the good.

\vspace{0.1in}
{\noindent}Happy Haskell Music Making!

\vspace{0.1in}
{\noindent}Paul Hudak\newline
New Haven\newline
January 2012
