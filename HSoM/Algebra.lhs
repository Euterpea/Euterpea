%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

%% Todo:
%% -- Add exercise to prove mel1 = mel2 from Intro

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{An Algebra of Music}
\label{ch:algebra}

In this chapter we will explore a number of properties of the |Music|
data type and functions defined on it, properties that collectively
form an \emph{algebra of music} \cite{PTM-PADL}.  With this algebra we
can reason about, transform, and optimize computer music programs in a
meaning preserving way.

\section{Musical Equivalance}

\index{algebraic properties}
Suppose we have two values |m1 :: Music Pitch| and |m2 :: Music Pitch|,
and we want to know if they are equal.  If we treat them simply as
Haskell values, we could easily write a function that compares their
structures recursively to see if they are the same at every level, all
the way down to the |Primitive| rests and notes.  This is in fact what
the Haskell function |(==)| does.  For example, if:
\begin{spec}
m1  = c 4 en :+: d 4 qn
m2 = revM (revM m1)
\end{spec}
Then |m1 == m2| is |True|.

Unfortunately, as we saw in the last chapter, if we reverse a parallel
composition, things do not work out as well.  For example:
\begin{spec}
revM (revM (c 4 en :=: d 4 qn))
==> (rest 0 :+: c 4 en :+: rest en) :=: d 4 qn
\end{spec}

In addition, as we discussed briefly in Chapter \ref{ch:intro}, there
are musical properties for which standard Haskell equivalence is
insufficient to capture.  For example, we would expect the following
two musical values to \emph{sound} the same, regardless of the actual
values of |m1|, |m2|, and |m3|:
\begin{spec}
(m1 :+: m2) :+: m3
m1 :+: (m2 :+: m3)
\end{spec}
In other words, we expect the operator |(:+:)| to be \emph{associative}.

The problem is that, as data structures, these two values are
\emph{not} equal in general, in fact there are no finite values that
can be assigned to |m1|, |m2|, and |m3| to make them
equal.\footnote{If |m1 = m1 :+: m2| and |m3 = m2 :+: m3| then the two
  expressions are equal, but these are infinite values that cannot
  be reversed or even performed.}

The obvious way out of this dilemma is to define a new notion of
equality that captures the fact that the \emph{performances} are the
same---i.e.\ if two things \emph{sound} the same, they must be
musically equivalent.  And thus we define a formal notion of musical
equivalence:

\paragraph*{Definition:}
Two musical values |m1| and |m2| are \emph{equivalent}, written \newline
|m1 === m2|, if and only if:
\[ (\forall|pm,c|)\ \ |perf pm c m1 = perf pm c m2| \]

We will study a number of properties in this chapter that capture
musical equivalences, similar in spirit to the associativity of
|(:+:)| above.  Each of them can be thought of as an \emph{axiom}, and
the set of valid axioms collectively forms an \emph{algebra of music}.
By proving the validity of each axiom we not only confirm our
intuitions about how music is interpreted, but also gain confidence
that our |perform| function actually does the right thing.
Furthermore, with these axioms in hand, we can \emph{transform}
musical values in meaning-preserving ways.

Speaking of the |perform| function, recall from Chapter
\ref{ch:performance} that we defined \emph{two} versions of |perform|,
and the definition above uses the function |perf|, which includes the
duration of a musical value in its result.  The following Lemma
captures the connection between these functions:

\begin{lemma}
\label{lem:perf}
{\em
For all |pm|, |c|, and |m|:
\begin{spec}
perf pm c m = (perform pm c m, dur m * cDur c)
\end{spec}
where |perform| is the function defined in Figure \ref{fig:perform}.
}
\end{lemma}

To see the importance of including duration in the definition of
equivalence, we first note that if two musical values are equivalent,
we should be able to substitute one for the other in any valid musical
context.  But if duration is not taken into account, then all rests
are equivalent (because their performances are just the empty list).
This means that, for example, |m1 :+: rest 1 :+: m2| is equivalent to
|m1 :+: rest 2 :+: m2|, which is surely not what we want.\footnote{A
  more striking example of this point is John Cage's composition
  \emph{4'33''}, which consists basically of four minutes and
  thirty-three seconds of silence \cite{}.}

Note that we could have defined |perf| as above, i.e.\ in terms of
|perform| and |dur|, but as mentioned in Section \ref{sec:performance}
it would have been computationally inefficient to do so.  On the other
hand, if the Lemma above is true, then our proofs might be simpler if
we first proved the property using |perform|, and then using |dur|.
That is, to prove |m1 === m2| we need to prove:
\begin{spec}
perf pm c m1 = perf pm c m2
\end{spec}
Instead of doing this directly using the definition of |perf|, we
could instead prove both of the following:
\begin{spec}
perform pm c m1 = perform pm c m2
dur m1 = dur m2
\end{spec}

\subsection{Literal Player}

The only problem with this strategy for defining musical equivalence
is that the notion of a \emph{player} can create situations where
certain properties that we would like to hold, in fact do not.  After
all, a player may interpret a note or phrase in whatever way it (or he
or she) may desire.  For example, it seems that this property should
hold:
\begin{spec}
tempo 1 m === m
\end{spec}
However, a certain (rather perverse) player might interpret anything
tagged with a |Tempo| modifier as an empty performance---in which case
the above property will fail!  To solve this problem, we assume that...

\section{Some Simple Axioms}

% \setcounter{axiom}{0}

Let's look at a few simple axioms, and see how we can prove each of
them using the proof techniques that we have developed so far.

(Note: In the remainder of this chapter we will use the functions
|tempo r| and |trans p| to represent their unfolded versions, |Modify
(Tempo r)| and |Modify (Transpose t)|, respectively.  In the proofs we
will not bother with the intermediate steps of unfolding these
functions.)

Here is the first axiom that we will consider:
\begin{axiom}{\em
For any |r1|, |r2|, and |m|:
\begin{spec}
tempo r1 (tempo r2 m) === tempo (r1*r2) m
\end{spec}
}
\end{axiom}
In other words, \emph{tempo scaling is multiplicative}.

We can prove this by calculation, starting with the definition of
musical equivalence.  For clarity we will first prove the property for
|perform|, and then for |dur|, as suggested in the last section:
\begin{spec}
let dt = cDur c

perform pm c (tempo r1 (tempo r2 m))
==> { unfold perform }
perform pm (c {cDur = dt/r1}) (tempo r2 m)
==> { unfold perform }
perform pm (c {cDur = (dt/r1)/r2}) m
==> { arithmetic }
perform pm (c {cDur = dt/(r1*r2)}) m
==> { fold perform }
perform pm c (tempo (r1*r2) m)
\end{spec}

\begin{spec}
dur (tempo r1 (tempo r2 m))
==> { unfold dur }
dur (tempo r2 m) / r1
==> { unfold dur }
(dur m / r2) / r1
==> {arithmetic }
dur m / (r1*r2)
==> { fold dur }
dur (tempo (r1*r2) m)
\end{spec}

Here is another useful axiom and its proof:
\begin{axiom}{\em
For any |r|, |m1|, and |m2|:
\begin{spec}
tempo r (m1 :+: m2) === tempo r m1 :+: tempo r m2
\end{spec}
}
\end{axiom}
In other words, {\em tempo scaling distributes over sequential
composition}.

{\noindent \bf Proof:}

\begin{spec}
let  t  = cTime c; dt = cDur c
     t1 = t + dur m1 * (dt/r)
     t2 = t + (dur m1 / r) * dt
     t3 = t + dur (tempo r m1) * dt

perform pm c (tempo r (m1 :+: m2))
==> { unfold perform }
perform pm (c {cDur = dt/r}) (m1 :+: m2)
==> { unfold perform }
perform pm (c {cDur = dt/r}) m1 
    ++ perform pm (c {cTime = t1, cDur = dt/r}) m2
==> { fold perform }
perform pm c (tempo r m1) 
    ++ perform pm (c {cTime = t1}) (tempo r m2)
==> { arithmetic }
perform pm c (tempo r m1) 
    ++ perform pm (c {cTime = t2}) (tempo r m2)
==> { fold dur }
perform pm c (tempo r m1) 
    ++ perform pm (c {cTime = t3}) (tempo r m2)
==> { fold perform }
perform pm c (tempo r m1 :+: tempo r m2)
\end{spec}

\begin{spec}
dur (tempo r (m1 :+: m2))
==> dur (m1 :+: m2) / r
==> (dur m1 + dur m2) / r
==> dur m1 / r + dur m2 / r
==> dur (tempo r m1) + dur (tempo r m2)
==> dur (tempo r m1 :+: tempo r m2)
\end{spec}
 
An even simpler axiom is given by:
\begin{axiom}{\em
For any |m|, |tempo 1 m === m|.
}
\end{axiom}
In other words, {\em unit tempo scaling is the identity function for
type} |Music|.

{\noindent\bf Proof:}

\begin{spec} 
let dt = cDur c

perform pm c (tempo 1) m)
==> { unfold perform  }
perform pm (c {cDur = dt/1}) m
==> { arithmetic }
perform pm c m
\end{spec}

\begin{spec}
dur (tempo 1) m)
==> dur m / 1
==> dur m
\end{spec}
 
Note that the above three proofs, being used to establish axioms, all
involve the definitions of |perform| and |dur|.  In contrast, we can
also establish {\em theorems} whose proofs involve only the axioms.
For example, Axioms 1, 2, and 3 are all needed to prove the following:

\begin{theorem}{\em
For any |r|, |m1|, and |m2|:
\begin{spec}
tempo r m1 :+: m2 === tempo r (m1 :+: tempo (1/r) m2)
\end{spec}
}
\end{theorem}

{\noindent\bf Proof:}

\begin{spec} 
tempo r m1 :+: m2
==> { Axiom 3 }
tempo r m1 :+: tempo 1 m2
==> { arithmetic }
tempo r m1 :+: tempo (r*(1/r)) m2
==> { Axiom 1 } 
tempo r m1 :+: tempo r (tempo (1/r) m2)
==> { Axiom 2 }
tempo r (m1 :+: tempo (1/r) m2)
\end{spec}

%% This theorem justifies the equivalence of the two phrases shown in
%% Figure \ref{equiv}.

%% \begin{figure*}
%% \vspace*{1in}
%% \centerline{
%% \epsfysize=.6in 
%% \epsfbox{Pics/equiv.eps}
%% }
%% \caption{Equivalent Phrases}
%% \label{equiv}
%% \end{figure*}

\section{The Fundamental Axiom Set}

There are many other useful axioms, but we do not have room to include
all of their proofs here.  They are listed below, which include the axioms
from the previous section as special cases, and the proofs are left as
exercises.  

\begin{axiom}{\em
|Tempo| is {\em multiplicative} and |Transpose| is {\em
additive}.  That is, for any |r1|, |r2|, |p1|, |p2|, and
|m|:
\begin{spec}
tempo r1 (tempo r2 m)  === tempo (r1*r2) m

trans p1 (trans p2 m)  === trans (p1+p2) m
\end{spec}
}
\end{axiom}

\begin{axiom}{\em
Function composition is {\em commutative} with respect to both tempo
scaling and transposition.  That is, for any |r1|, |r2|, |p1|
and |p2|:
\begin{spec}
tempo r1 . tempo r2  === tempo r2 . tempo r1

trans p1 . trans p2  === trans p2 . trans p1

tempo r1 . trans p1  === trans p1 . tempo r1
\end{spec}
}
\end{axiom}

\begin{axiom}{\em
Tempo scaling and transposition are {\em distributive} over both
sequential and parallel composition.  That is, for any |r|,
|p|, |m1|, and |m2|:
\begin{spec}
tempo r (m1 :+: m2)  === tempo r m1 :+: tempo r m2

tempo r (m1 :=: m2)  === tempo r m1 :=: tempo r m2

trans p (m1 :+: m2)  === trans p m1 :+: trans p m2

trans p (m1 :=: m2)  === trans p m1 :=: trans p m2
\end{spec}
}
\end{axiom}

\begin{axiom}{\em
Sequential and parallel composition are {\em associative}.  That is,
for any |m0|, |m1|, and |m2|:
\begin{spec}
m0 :+: (m1 :+: m2)  === (m0 :+: m1) :+: m2

m0 :=: (m1 :=: m2)  === (m0 :=: m1) :=: m2
\end{spec}
}
\end{axiom}

\begin{axiom}{\em
Parallel composition is {\em commutative}.  That is, for any |m0|
and |m1|:
\begin{spec}
m0 :=: m1 === m1 :=: m0
\end{spec}
}
\end{axiom}

\begin{axiom}{\em
|rest 0| is a {\em unit} for |tempo| and |trans|, and a {\em
zero} for sequential and parallel composition.  That is, for any
|r|, |p|, and |m|:
\begin{spec}
tempo r (rest 0)  === rest 0

trans p (rest 0)  === rest 0

m :+: rest 0      === m === rest 0 :+: m

m :=: rest 0      === m === rest 0 :=: m 
\end{spec}
}
\end{axiom}

\begin{axiom}{\em
A rest can be used to ``pad'' a parallel composition.  That is, for
any |m1|, |m2|, such that |diff = dur m1 > dur m2 >= 0|, and any |d <=
diff|:
\begin{spec}
m1 :=: m2 === m1 :=: (m2 :+: rest d)
\end{spec}
\label{ax:pad}
}
\end{axiom}

\begin{axiom}{\em
There is a duality between |(:+:)| and |(:=:)|, namely that, for any
|m0|, |m1|, |m2|, and |m3| such that |dur m0 = dur m2|:
\begin{spec}
(m0 :+: m1) :=: (m2 :+: m3) === (m0 :=: m2) :+: (m1 :=: m3)
\end{spec}
}
\end{axiom}

\vspace{.1in}\hrule

\begin{exercise}{\em 
Prove Lemma \ref{lem:perf}.}
\end{exercise}

\begin{exercise}{\em 
Establish the validity of each of the above axioms.}
\end{exercise}

\begin{exercise}{\em
Recall the polyphonic and contrapuntal melodies |mel1| and |mel2| from
Chapter~\ref{ch:intro}.  Prove that |mel1 === mel2|.}
\end{exercise}

\vspace{.1in}\hrule

\section{An Algebraic Semantics}

Discuss formal semantics.  Denotational, operational (relate to
``proof by calculation''), and algebraic.

Soundness and Completeness.

\cite{PTM-PADL}

\section{Other Musical Properties}

Aside from the axioms discussed so far, there are many other
properties of |Music| values and its various operators, just as we saw
in Chapter~\ref{ch:induction} for lists.  For example, this property
of |map| taken from Figure~\ref{fig:list-props1}:
\begin{spec}
map (f . g)       = map f . map g
\end{spec}
suggests and analogous property for |mMap|:
\begin{spec}
map (f . g)       = map f . map g
\end{spec}
Not all of the properties in Figures~\ref{fig:list-props1} and
\ref{fig:list-props2} have analogous musical renditions, and there
are also others that are special only to |Music| values.
Figure~\ref{fig:music-props} summarizes the most important of these
properties, including the one above.  Note that some of the properties
are expressed as strict equality---that is, the left-hand and
right-hand sides are equivalent as Haskell values.  But others are
expressed using musical equivalence---that is, using |(===)|.  We
leave the proofs of all these properties as an exercise.

\begin{figure}
\cbox{
\begin{minipage}{4.75in}
{\bf Properties of |mMap|:}

\vspace{0.1in}
\begin{spec}
mMap (\x->x)       = \x->x
mMap (f . g)       = mMap f . mMap g
mMap f . dropM d   = dropM d . mMap f
mMap f . takeM d   = takeM d . mMap f
\end{spec}

\vspace{0.1in}
{\bf Properties of |takeM| and |dropM|:}

\vspace{0.1in} For all non-negative |d1| and |d2|:
\begin{spec}
takeM d1 . takeM d2  = takeM (min d1 d2)
dropM d1 . dropM d2  = dropM (d1 + d2)
takeM d1 . dropM d2  = dropM d1 . takeM (d1 + d2)
\end{spec}
For all non-negative |d1| and |d2| such that |d2 >= d1|:
\begin{spec}
dropM d1 . takeM d2 = takeM (d2 - d1) . dropM d1
\end{spec}

\vspace{0.1in}
{\bf Properties of |revM|:}

\vspace{0.1in} For all finite-duration |m|:
\begin{spec}
revM (revM m)     === m
revM (takeM d m)  === dropM (dur m - d) (revM m)
revM (dropM d m)  === takeM (dur m - d) (revM m)
takeM d (revM m)  === revM (dropM (dur m - d) m)
dropM d (revM m)  === revM (takeM (dur m - d) m)
\end{spec}

\vspace{0.1in}
{\bf Properties of |dur|:}

\vspace{0.1in}
\begin{spec}
dur (revM m)     = dur m
dur (takeM d m)  = min d (dur m)
dur (dropM d m)  = max 0 (dur m - d)
\end{spec}
\end{minipage}}
\caption{Useful Properties of Other Musical Functions}
\label{fig:music-props}
\end{figure}

\vspace{.1in}\hrule

\begin{exercise}{\em
Prove that |timesM a m :+: timesM b m === timesM (a+b) m|.}
\end{exercise}

\begin{exercise}{\em
Prove as many of the axioms from Figure~\ref{fig:music-props} as you
can.}
\end{exercise}

\out{
Proof that revM (revM m) === m:

revM (revM (Prim p)) 
==> revM (Prim p)
==> Prim p

revM (revM (Modify c m))
==> revM (Modify c (revM m))
==> Modify c (revM (revM m))
==> Modify c m

revM (revM (m1 :+: m2))
==> revM (revM m2 :+: revM m1)
==> revM (revM m1) :+: revM (revM m2)
==> m1 :+: m2

revM (revM (m1 :=: m2))
==> revM (let d1 = dur m1; d2 = dur m2
          in if d1>d2
             then revM m1 :=: (rest (d1 − d2) :+: revM m2)
             else (rest (d2 − d1) :+: revM m1) :=: revM m2 )
==> let d1 = dur m1; d2 = dur m2
    in if d1>d2
       then revM (revM m1 :=: (rest (d1−d2) :+: revM m2))
       else revM ((rest (d2−d1) :+: revM m1) :=: revM m2)
==> let d1 = dur m1; d2 = dur m2
    in if d1>d2
       then revM (revM m1 :=: (rest (d1−d2) :+: revM m2))
       else revM ((rest (d2−d1) :+: revM m1) :=: revM m2)

Taking each branch of the conditional separately, first assume d1>d2:

revM (revM m1 :=: (rest (d1−d2) :+: revM m2))
==> let d1' = dur (revM m1)
        d2' = dur (rest (d1−d2) :+: revM m2)
        in if d1'>d2'
           then revM (revM m1) :=: (rest (d1'-d2') :+: 
                                    revM (rest (d1−d2) :+: revM m2))
           else (rest (d2'-d1') :+: revM (revM m1)) :=:
                                   revM (rest (d1−d2) :+: revM m2)
==> let d1' = dur m1 = d1
        d2' = (d1−d2) + d2 = d1
        in if d1'>d2'
           then revM (revM m1) :=: (rest (d1'-d2') :+: 
                                    revM (rest (d1−d2) :+: revM m2))
           else (rest (d2'-d1') :+: revM (revM m1)) :=:
                                    revM (rest (d1−d2) :+: revM m2)
==> (rest (d2'-d1') :+: revM (revM m1)) :=: revM (rest (d1−d2) :+: revM m2)
==> (rest 0 :+: revM (revM m1)) :=: revM (rest (d2−d1) :+: revM m2)
=== revM (revM m1) :=: revM (rest (d1−d2) :+: revM m2)
==> m1 :=: revM (rest (d1−d2) :+: revM m2)
==> m1 :=: (revM (revM m2) :+: revM (rest (d1−d2)))
==> m1 :=: (m2 :+: revM (rest (d1−d2)))
==> m1 :=: (m2 :+: rest (d1−d2))
=== m1 :=: m2

Note: The last step relies on Axiom \ref{ax:pad}.

The other branch of the conditional follows similarly.
}

\vspace{.1in}\hrule
