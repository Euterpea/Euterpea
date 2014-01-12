%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

\documentclass[11pt,fleqn,oneside]{book} % 

\newcommand{\HSoMVersion}{2.6 (January 2014)}

%% \usepackage[pdftex,bookmarks=true]{hyperref}

\usepackage{amssymb}
\usepackage{amsmath}
\usepackage{nicefrac}
\usepackage{hyperref}

%% pdftex]

\hypersetup{
    pdfauthor={Paul Hudak},
    pdftitle={The Haskell School of Music},
    colorlinks,
    citecolor={magenta},
    bookmarks={true},
    pdfstartview={Fit},
    pdfpagelayout={SinglePage}
}

\usepackage{graphicx}
\usepackage{epsfig}
\usepackage{subfigure}
\usepackage{shading}
\usepackage{enumerate}
\usepackage{url}
%% \usepackage{diagrams}

% If using --poly
\usepackage{polytable}
\usepackage{lazylist}

% The (working) code can be extracted from each section via:
% > lhs2TeX --code fileName.lhs > code.hs
%
% Pre-processing of each section for LaTeX should be done via:
% > lhs2TeX --poly fileName.lhs > fileName.tex

% The following should be at the top of each section for lhs2TeX:
% 
%include lhs2TeX.fmt
%include myFormat.fmt

\setlength{\parskip}{0.05in}

% theorem-like environments
\newtheorem{axiom}{Axiom}[section]
\newtheorem{theorem}{Theorem}[section]
\newtheorem{corollary}{Corollary}[section]
\newtheorem{definition}{Definition}[section]
\newtheorem{lemma}{Lemma}[section]
\newtheorem{example}{Example}[section]
\newtheorem{exercise}{Exercise}[chapter]
% \newtheorem{proof}{Proof}[section]

% \newcommand{\lb}{[\hspace*{-.4 mm}[}
% \newcommand{\rb}{]\hspace*{-.4 mm}]}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% All this indexing should change to be more %%%%
%%%% standard and to use lhs2tex for formatting %%%%

% new commands used for the index:
\newcommand{\indexwd}[1]{#1\index{#1}}
\newcommand{\indexwdhs}[1]{\ihs{#1}\index{#1@@\protect\ihs{#1}}}
\newcommand{\indexwdkw}[1]{\hkw{#1}\index{#1@@\protect\hkw{#1}}}
\newcommand{\indexhs}[1]{\index{#1@@\protect\ihs{#1}}}
\newcommand{\indexkw}[1]{\index{#1@@\protect\hkw{#1}}}
\newcommand{\indexamb}[2]{\index{#1@@\protect\ihs{#1} (#2)}}

% the following are two hacks because we're no longer using Mark
% Jones' preprocessor
% \newcommand{\ihs}[1]{\hs{#1}}
\newcommand{\ihs}[1]{{\em #1}}
\newcommand{\hkw}[1]{{\bf #1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\out}[1]{}
\newcommand{\prh}[1]{{\bf #1}}
\newcommand{\todo}[1]{}
%% \newcommand{\todo}[1]{{\bf [To do:} #1 {\bf ]}}
\newcommand{\red}{\Longrightarrow}

\newcommand{\syn}[1]{\begin{quote} 
                     \parashade[0.9]{roundcorners}{\gdef\outlineboxwidth{.5}
                        {\small\sf {\bf Details:} #1}}
                     \end{quote}}

\begin{document}

%---------------------------------------------------------------------

\frontmatter

\begin{titlepage}

\vspace*{.1in}

\begin{center}
{\huge\sl The Haskell School of Music}\\[.2in]
{\Large --- From Signals to Symphonies ---}
\end{center}
\vspace{0.5in}

\centerline{
\epsfysize=3in 
\epsfbox{pics/muse_euterpe.eps}
}

\vspace{.1in}
\begin{center}
%% {\Large\bf by}\\[.3in]
{\Large\bf Paul Hudak}\\[.2in]
{\large\bf Yale University}\\
{\large\bf Department of Computer Science}\\[.5in]
{\large\bf Version \HSoMVersion}
\end{center}

\end{titlepage}

\newpage

\vspace*{3.0in}
\fbox{
\begin{minipage}{5in}
\begin{center}
\vspace{.1in}
{\sl The Haskell School of Music}\\
{\sl --- From Signals to Symphonies ---}\\[.1in]
{\sl Paul Hudak}\\[.1in]
{\sl Yale University}\\
{\sl Department of Computer Science}\\
{\sl New Haven, CT, USA}\\
{\sl Version \HSoMVersion}\\[.1in]
Copyright \copyright\ Paul Hudak\\
January 2011, 2012, 2013\\
All rights reserved.  No part of this publication may be reproduced or
distributed in any form or by any means, or stored in a data base or
retrieval system, without the prior written permission of the author.
\vspace{.1in}\\
Cover image: \emph{Euterpe}, the Greek Muse of Music\\
(attribution unknown)\\[.1in]
%% \vspace{.1in}
\end{center}
\end{minipage}
}

\newpage

\tableofcontents

\listoffigures

\listoftables

% Preface
\include{Preface}

%---------------------------------------------------------------------

\mainmatter

% An Overview of Computer Music, Euterpea, and Haskell
\include{Intro}

% Simple Music
\include{Music}

% Polymorphic and Higher-order Functions
\include{Poly}

% A Musical Interlude
\include{Interlude}

% Syntactic Magic
\include{Syntax}

% More Music
\include{MoreMusic}

% Qualified Types and Type Classes
\include{QualifiedTypes}

% Interpretation and Performance
\include{Performance}

% Self-Similar Music
\include{SelfSimilar}

% Proof by Induction
\include{Induction}

% An Algebra of Music
\include{Algebra}

% Musical L-Systems
\include{LSystems}

% Random Numbers, Probability Distributions, and Markov Chains
\include{RandomMusic}

% From Performance to Midi
\include{ToMidi}

% Basic Input/Output
\include{IO}

% Higher-Order Types and Monads
\include{Monads}

% Musical User Interface
\include{MUI}

% Sound and Signals
\include{Signals}

% Euterpea Signal Functions
\include{SigFuns}

% Spectrum Analysis
\include{SpectrumAnalysis}

% Additive and Subtractive Synthesis
\include{Additive}

% Amplitude and Frequency Modulation
\include{AMAndFM}

% Physical Modelling
\include{PhysicalModeling}

% Effects
\include{Effects}

% Programming with Streams
% \include{streams}

% Communicating With the Outside World
% \include{ioproc}

% ---------------------------------------------------------------------
% \backmatter

\appendix

% Tour of PreludeList
\include{List-tour}

% A Tour of Haskell's Standard Type Classes
\include{Class-tour}

% Built-In Types Are Not Special
\include{Bitans}

% Pattern-Matching Details
\include{Patterns}

\newpage

% Bibliography
\bibliographystyle{alpha}
\bibliography{HSoM}

% Index
% \printindex

\end{document}
