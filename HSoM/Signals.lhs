%-*- mode: Latex; abbrev-mode: true; auto-fill-function: do-auto-fill -*-

% ToDo:
% add remaining figures

%include lhs2TeX.fmt
%include myFormat.fmt

\chapter{Sound and Signals}
\label{ch:signals}

In this chapter we study the fundamental nature of sound and its basic
mathematical representation as a signal.  We also discuss discrete
digital representations of a signal, which form the basis of modern
sound synthesis and audio processing.

% Taken from Chapter 2 of the text, and
% Blair School of Music (BSoM) http://www.computermusicresource.com/

\section{The Nature of Sound}
\label{sec:sound}

Before studying digital audio, it's important that we first know what
\emph{sound} is.  In essence, sound is the rapid compression and
relaxation of air, which travels as a \emph{wave} through the air from
the physical source of the sound to, ultimately, our ears.  The
physical source of the sound could be the vibration of our vocal
chords (resulting in speech or singing), the vibration of a speaker
cone, the vibration of a car engine, the vibration of a string in a
piano or violin, the vibration of the reed in a saxophone or of
the lips when playing a trumpet, or even the (brief and chaotic)
vibrations that result when our hands come together as we clap.
The ``compression and relaxation'' of the air (or of a coiled spring)
is called a \emph{longitudinal} wave, in which the vibrations occur
parallel to the direction of travel of the wave.  In contrast, a rope
that is fixed at one end and being shaken at the other, and a wave in
the ocean, are examples of a \emph{transverse} wave, in which the
rope's and water's movement is perpendicular to the direction the wave
is traveling.

[Note: There are some great animations of these two kinds of waves at:
\newline
  \verb|http://www.computermusicresource.com/what.is.sound.html|.]

If the rate and amplitude of the sound are within a suitable range, we
can \emph{hear} the sound---i.e.\ it is \emph{audible sound}.
``Hearing'' results when the vibrating air waves cause our ear drum to
vibrate, in turn stimulating nerves that enter our brain.  Sound above
our hearing range (i.e.\ vibration that is too quick to induce any
nerve impulses) is called \emph{ultrasonic sound}, and sound below our
hearing range is said to be \emph{infrasonic}.

Staying within the analog world, sound can also be turned into an
\emph{electrical} signal using a \emph{microphone} (or ``mic'' for
short).  Several common kinds of microphones are:
\begin{enumerate}
\item Carbon microphone.  Based on the resistance of a pocket of
  carbon particles that are compressed and relaxed by the sound waves
  hitting a diaphram.
\item Condenser microphone.  Based on the capacitance between two
  diaphrams, one being vibrated by the sound.
\item Dynamic microphone.  Based on the inductance of a coil of wire
  suspended in a magnetic field (the inverse of a speaker).
\item Piezoelectric microphone.  Based on the property of certain
  crystals to induce current when they are bent.
\end{enumerate}

\begin{figure}[hbtp]
\centering
\includegraphics[height=4in,angle=270]{pics/sinewave.eps} 
\caption{A Sine Wave}
\label{fig:sine-wave}
\end{figure}

Perhaps the most common and natural way to represent a wave
diagrammatically, whether it be a sound wave or electrical wave,
longitudinal or transverse, is as a \emph{graph} of its amplitude
vs.\ time.  For example, Figure \ref{fig:sine-wave} shows a
\emph{sinusiodal wave} of 1000 cycles per second, with an amplitude
that varies beween +1 and -1.  A sinusoidal wave follows precisely the
definition of the mathematical sine function, but also relates
strongly, as we shall soon see, to the vibration of sound produced by
most musical instruments.  In the remainder of this text, we will
refer to a sinusoidal wave simply as a sine wave.

%% \begin{figure*}
%% \centerline{
%% \epsfysize=2in 
%% \epsfbox{pics/sinewave.eps}
%% }
%% \caption{A Sine Wave}
%% \label{fig:sine-wave}
%% \end{figure*}

%% Perhaps the most natural way to draw sound is the same as for an
%% electrical signal---that is, as a \emph{graph} of its amplitude
%% vs.\ time.  For example, see Figure \ref{fig:signal-graph}.  This same
%% representation can be used to represent both logitudinal and
%% transverse waves.

\emph{Acoustics} is the study of the properties, in particular the
propagation and reflection, of sound.  \emph{Psychoacoustics} is the
study of the mind's interpretation of sound, which is not always as
tidy as the physical properties that are manifest in acoustics.
Obviously both of these are important areas of study for music in
general, and therefore play an important role in generating or
simulating music with a computer.

The speed of sound can vary considerably, depending on the material,
the temperature, the humidity, and so on.  For example, in dry air at
room temperature (68 degrees Farenheit), sound travels at a rate of
1,125 feet (343 meters) per second, or 768 miles (1,236 kilometers)
per hour.  Perhaps surprisingly, the speed of sound varies little with
respect to air pressure, although it does vary with temperature.

The reflection and absorbtion of sound is a much more difficult topic,
since it depends so much on the material, the shape and thickness of
the material, and the frequency of the sound.  Modeling well the
acoustics of a concert hall, for example, is quite challenging.  To
understand how much such reflections can affect the overall sound that
we hear, consider a concert hall that is 200 feet long and 100 feet
wide.  Based on the speed of sound given above, it will take a sound
wave $\nicefrac{2\times200}{1125} = 0.355$ seconds to travel from the
front of the room to the back of the room and back to the front again.
That $\nicefrac{1}{3}$ of a second, if loud enough, would result in a
significant distortion of the music, and corresponds to about one beat
with a metronome set at 168.

With respect to our interpretation of music, sound has (at least)
three key properties:
\begin{enumerate}
\item \emph{Frequency} (perceived as \emph{pitch}).
\item \emph{Amplitude} (perceived as \emph{loudness}).
\item \emph{Spectrum} (perceived as \emph{timbre}).
\end{enumerate}
We discuss each of these in the sections that follow.

%% \subsection{Review of Trigonometric Identities}

%% In preparation for what follows, we quickly review some basic
%% properties of trigonometric functions that are useful for audio
%% processing.  In general, all of the transcendental functions have a
%% use in audio processing and computer music applications, but our focus
%% here is on sine and cosine.

\subsection{Frequency and Period}
\label{sec:frequency}

The \emph{frequency} $f$ is simply the rate of the vibrations (or
repetitions, or cycles) of the sound, and is the inverse of the
\emph{period} (or duration, or wavelength) $p$ of each of the
vibrations:
\[ f = \frac{1}{p} \]
Frequency is measured in \emph{Hertz} (abbreviated Hz), where 1 Hz is
defined as one cycle per second.  For example, the sound wave in
Figure \ref{fig:sine-wave} has a frequency of 1000 Hz (i.e.\ 1 kHz)
and a period of $\nicefrac{1}{1000}$ second (i.e.\ 1 ms).

In trigonometry, functions like sine and cosine are typically applied
to angles that range from 0 to 360 degrees.  In audio processing (and
signal processing in general) angles are instead usually measured in
\emph{radians}, where $2\pi$ radians is equal to $360^\circ$.  Since
the sine function has a period of $2\pi$ and a frequency of
$\nicefrac{1}{2\pi}$, it repeats itself every $2\pi$ radians:
\[ \sin (2\pi k + \theta) = \sin \theta \]
for any integer $k$. 

But for our purposes it is better to parameterize these functions over
frequency as follows.  Since $\sin(2\pi t)$ covers one full cycle in
one second, i.e.\ has a frequency of 1 Hz, it makes sense that
$\sin(2\pi f t)$ covers $f$ cycles in one second, i.e.\ has a frequency
of $f$.  Indeed, in signal processing the quantity $\omega$ is defined
as:
\[ \omega = 2 \pi f \]
That is, a pure sine wave as a function of time behaves as
$\sin(\omega t)$.

Finally, it is convenient to add a \emph{phase} (or \emph{phase
  angle}) to our formula, which effectively shifts the sine wave in
time.  The phase is usually represented by $\phi$.  Adding a
multiplicative factor $A$ for amplitude (see next section), we arrive
at our final formula for a sine wave as a function of time:
\[ s(t) = A\sin(\omega t + \phi) \]
A negative value for $\phi$ has the effect of ``delaying'' the sine
wave, whereas a positive value has the effect of ``starting early.''
Note also that this equation holds for negative values of $t$.

All of the above can be related to cosine by recalling the following
identity:
\[ \sin(\omega t + \dfrac{\pi}{2}) = \cos(\omega t) \]
More generally:
\[ A \sin(\omega t + \phi) = a\cos(\omega t) + b\sin(\omega t) \]
Given $a$ and $b$ we can solve for $A$ and $\phi$:
\[\begin{array}{lcl}
A    &=& \sqrt{a^2 + b^2} \\[.05in]
\phi &=& \tan^{-1} \dfrac{b}{a}
\end{array}\]
Given $A$ and $\phi$ we can also solve for $a$ and $b$:
\[\begin{array}{lcl}
a &=& A\cos(\phi) \\
b &=& A\sin(\phi)
\end{array}\]

\subsection{Amplitude and Loudness}
\label{sec:amplitude}

Amplitude can be measured in several ways.  The \emph{peak amplitude}
of a signal is its maximum deviation from zero; for example our sine
wave in Figure \ref{fig:sine-wave} has a peak amplitude of 1.  But
different signals having the same peak amplitude have more or less
``energy,'' depending on their ``shape.''  For example, Figure
\ref{fig:rms} shows four kinds of signals: a sine wave, a square wave,
a sawtooth wave, and a triangular wave (whose names are suitably
descriptive).  Each of them has a peak amplitude of 1.  But,
intuitively, one would expect the square wave, for example, to have
more ``energy,'' or ``power,'' than a sine wave, because it is
``fatter.''  In fact, it's value is everywhere either +1 or -1.

\begin{figure}[hbtp]
\centering
\includegraphics[height=3in,angle=270]{pics/sine_rms.eps} 
\includegraphics[height=3in,angle=270]{pics/square_rms.eps} 
\includegraphics[height=3in,angle=270]{pics/sawtooth_rms.eps} 
\includegraphics[height=3in,angle=270]{pics/triangle_rms.eps} 
\caption{RMS Amplitude for Different Signals}
\label{fig:rms}
\end{figure}

To measure this characteristic of a signal, scientists and engineers
often refer to the \emph{root-mean-square} amplitude, or RMS.
Mathematically, the root-mean-square is the square root of the mean of
the squared values of a given quantity.  If $x$ is a discrete quantity
given by the values $x_1, x_2, ..., x_n$, the formula for RMS is:

\[ x_{\rm RMS} = \sqrt{\frac{x_1^2 + x_2^2 + ... + x_n^2}{n}} \]

And if $f$ is continuous function, its RMS value over the interval $T_1
\leq t \leq T_2$ is given by:

\[ \sqrt{{\frac{1}{T_2-T_1}}\int_{-T_1}^{T_2}f(t)^2dt} \]

For a sine wave, it can be shown that the RMS value is approximately
0.707 of the peak value.  For a square wave, it is 1.0.  And for both
a sawtooth wave and a triangular wave, it is approximately 0.577.
Figure \ref{fig:rms} shows these RMS values superimposed on each of
the four signals.

Another way to measure amplitude is to use a relative logarithmic
scale that more aptly reflects how we hear sound.  This is usually
done by measuring the sound level (usually in RMS) with respect to
some reference level.  The number of \emph{decibels} (dB) of sound
is given by:
\[ S_{dB} = 10 \log_{10}\frac{S}{R} \]
where $S$ is the RMS sound level, and $R$ is the RMS reference
level.  The accepted reference level for the human ear is $10^{-12}$
watts per square meter, which is roughly the threshold of hearing.

A related concept is the measure of how much useful information is in
a signal relative to the ``noise.''  The \emph{signal-to-noise ratio},
or $\mathit{SNR}$, is defined as the ratio of the \emph{power} of each
of these signals, which is the square of the RMS value:
\[ \mathit{SNR} = \left(\frac{S}{N}\right)^2 \]
where $S$ and $N$ are the RMS values of the signal and noise,
respectively.  As is often the case, it is better to express this on a
logarithmic scale, as follows:
\[\begin{array}{lcl}
\mathit{SNR}_{dB} &=& 10 \log_{10}\left(\dfrac{S}{N}\right)^2 \\[0.12in]
                 &=& 20 \log_{10}\dfrac{S}{N}
\end{array}\]

The \emph{dynamic range} of a system is the difference between the
smallest and largest values that it can process.  Because this range
is often very large, it is usually measured in decibels, which is a
logarithmic quantity.  The ear, for example, has a truly remarkable
dynamic range---about 130 dB.  To get some feel for this, silence
should be considered 0 dB, a whisper 30 dB, normal conversation about
60 dB, loud music 80 dB, a subway train 90 dB, and a jet plane taking
off or a very loud rock concert 120 dB or higher.

Note that if you double the sound level, the decibels increase by
about 3 dB, whereas a million-fold increase corresponds to 60 dB:
\[\begin{array}{lclcl}
10 \log_{10}2    &=& 10 \times 0.301029996 &\cong& 3 \\
10 \log_{10}10^6 &=& 10 \times 6          &=&     60 \\
\end{array}\]
So the ear is truly adaptive!  (The eye also has a large dynamic range
with respect to light intensity, but not quite as much as the ear, and
its response time is much slower.)

\begin{figure}[hbtp]
\centering
\includegraphics[height=4in]{pics/equal_loudness_contour.eps} 
\caption{Fletcher-Munson Equal Loudness Contour}
\label{fig:fletcher-munson}
\end{figure}

Loudness is the perceived measure of amplitude, or volume, of sound,
and is thus subjective.  It is most closely aligned with RMS
amplitude, with one important exception: loudness depends somewhat on
frequency!  Of course that's obvious for really high and really low
frequencies (since at some point we can't hear them at all), but in
between things aren't constant either.  Furthermore, no two humans are
the same.  Figure \ref{fig:fletcher-munson} shows the
\emph{Fletcher-Munson Equal-Loudness Contour}, which reflects the
perceived equality of sound intensity by the average human ear with
respect to frequency.  Note from this figure that:
\begin{itemize}
\item The human ear is less sensitive to low frequencies.
\item The maximum sensitivity is around 3-4 kHz, which roughly
  corresponds to the resonance of the auditory canal.
\end{itemize}

%% [See: \newline
%% \verb|http://hyperphysics.phy-astr.gsu.edu/Hbase/sound/earsens.html|
%% and
%% \verb|http://hyperphysics.phy-astr.gsu.edu/Hbase/hframe.html|]
%% \verb|http://www2.sfu.ca/sonic-studio/handbook/Equal_Loudness_Contours.html|.]

Another important psychoacoustical property is captured in the
\emph{Weber\-Fechner Law}, which states that the \emph{just noticeable
  difference} (jnd) in a quantity---i.e.\ the minimal change necessary
for humans to notice something in a cognitive sense---is a relative
constant, independent of the absolute level.  That is, the ratio of
the change to the absolute measure of that quantity is constant:

\[ \frac{\Delta q}{q} = k \]

The jnd for loudness happens to be about 1 db, which is another
reason why the decibel scale is so convenient.  1 db corresponds to a
sound level ratio of 1.25892541.  So, in order for a person to ``just
notice'' an increase in loudness, one has to increase the sound level
by about 25\%.  If that seems high to you, it's because your ear is so
adaptive that you are not even aware of it.

\subsection{Frequency Spectrum}
\label{sec:spectrum}

Humans can hear sound approximately in the range 20 Hz to 20,000 Hz =
20 kHz.  This is a dynamic range in frequency of a factor of 1000, or
30 dB.  Different people can hear different degrees of this range (I
can hear very low tones well, but not very high ones).  On a piano,
the fundamental frequency of the lowest note is 27.5 Hz, middle
(concert) A is 440 hz, and the top-most note is about 4 kHz.  Later we
will learn that these notes also contain \emph{overtones}---multiples
of the fundamental frequency---that contribute to the \emph{timbre},
or sound quality, that distinguishes one instrument from another.
(Overtones are also called \emph{harmonics} or \emph{partials}.)

The \emph{phase}, or time delay, of a signal is important too, and
comes into play when we start mixing signals together, which can
happen naturally, deliberately, from reverberations (room acoustics),
and so on.  Recall that a pure sine wave can be expressed as
$\sin(\omega t + \phi)$, where $\phi$ is the \emph{phase angle}.
Manipulating the phase angle is common in additive synthesis and
amplitude modulation, topics to be covered in later chapters.

\begin{figure}[hbtp]
\centering
\includegraphics[height=3in,angle=270]{pics/sine_spect1.eps}
\begin{center}
(a) Spectral plot of pure sine wave
\end{center}
\includegraphics[height=3in,angle=270]{pics/sine_spect2.eps} 
\begin{center}
(b) Spectral plot of a noisy sine wave
\end{center}
\includegraphics[height=3in,angle=270]{pics/sine_spect3.eps} 
\begin{center}
(c) Spectral plot of a musical tone
\end{center}
\caption{Spectral Plots of Different Signals}
\label{fig:frequency-spectrum}
\end{figure}

A key point is that most sounds do not consist of a single, pure sine
wave---rather, they are a combination of many frequencies, and at
varying phases relative to one another.  Thus it is helpful to talk of
a signal's \emph{frequency spectrum}, or spectral content.  If we have
a regular repetitive sound (called a \emph{periodic signal}) we can
plot its spectral content instead of its time-varying graph.  For a
pure sine wave, this looks like an impulse function, as shown in
Figure \ref{fig:frequency-spectrum}a.

But for a richer sound, it gets more complicated.  First, the
distribution of the energy is not typically a pure impulse, meaning
that the signal might vary slightly above and below a particular
frequency, and thus its frequency spectrum typically looks more like
Figure \ref{fig:frequency-spectrum}b.

In addition, a typical sound has many different frequencies associated
with it, not just one.  Even for an instrument playing a single note,
this will include not just the perceived pitch, which is called the
\emph{fundamental frequency}, but also many \emph{overtones} (or
harmonics) which are multiples of the fundamental, as shown in Figure
\ref{fig:frequency-spectrum}c.  The \emph{natural harmonic series} is
one that is approximated often in nature, and has a harmonically
decaying series of overtones.

What's more, the articulation of a note by a performer on an
instrument causes these overtones to vary in relative size over time.
There are several ways to visualize this graphically, and Figure
\ref{fig:time-varying-spectrum} shows two of them.  In
\ref{fig:time-varying-spectrum}a, shading is used to show the varying
amplitude over time.  And in \ref{fig:time-varying-spectrum}b, a 3D
projection is used.

\begin{figure}[hbtp]
\centering
\includegraphics[height=4in,angle=270]{pics/spectrum_map.eps}
\begin{center}
(a) Using shading
\end{center}
\includegraphics[height=4in,angle=270]{pics/spectrum_mesh.eps} 
\begin{center}
(b) Using 3D projection
\end{center}
\caption{Time-Varying Spectral Plots}
\label{fig:time-varying-spectrum}
\end{figure}

The precise blend of the overtones, their phases, and how they vary
over time, is primarily what distinguishes a particular note, say
concert A, on a piano from the same note on a guitar, a violin, a
saxophone, and so on.  We will have much more to say about these
issues in later chapters.

[See pictures at:
\newline
  \verb|http://www.computermusicresource.com/spectrum.html|.]

% This also relates to the envelope that “shapes” a sound. [see BSoM
% for more ideas on this]

\section{Digital Audio}
\label{sec:digital-audio}
 
The preceding discussion has assumed that sound is a continuous
quantity, which of course it is, and thus we represent it using
continuous mathematical functions.  If we were using an analog
computer, we could continue with this representation, and create
electronic music accordingly.  Indeed, the earliest electronic
synthesizers, such as the \emph{Moog synthesizer} of the 1960's, were
completely analog.

However, most computers today are \emph{digital}, which require
representing sound (or signals in general) using digital values.  The
simplest way to do this is to represent a continuous signal as a
\emph{sequence of discrete samples} of the signal of interest.  An
\emph{analog-to-digital converter}, or ADC, is a device that converts
an instantaneous sample of a continuous signal into a binary value.
The microphone input on a computer, for example, connects to an ADC.

Normally the discrete samples are taken at a fixed \emph{sampling
  rate}.  Choosing a proper sampling rate is quite important.  If it
is too low, we will not acquire sufficient samples to adequately
represent the signal of interest.  And if the rate is too high, it may
be an overkill, thus wasting precious computing resources (in both
time and memory consumption).  Intuitively, it seems that the highest
frequency signal that we could represent using a sampling rate $r$
would have a frequency of $\nicefrac{r}{2}$, in which case the result
would have the appearance of a square wave, as shown in Figure
\ref{fig:sample-rate}a.  Indeed, it is easy to see that problems could
arise if we sampled at a rate significantly lower than the frequency
of the signal, as shown in Figures \ref{fig:sample-rate}b and
\ref{fig:sample-rate}c for sampling rates equal to, and one-half, of
the frequency of the signal of interest---in both cases the result is
a sampled signal of 0 Hz!

\begin{figure}[hbtp]
\centering
\includegraphics[height=3.1in,angle=270]{pics/aliasing_2f.eps}
\begin{center}
(a)
\end{center}
\includegraphics[height=3.1in,angle=270]{pics/aliasing_f.eps} 
\begin{center}
(b)
\end{center}
\includegraphics[height=3.1in,angle=270]{pics/aliasing_half-f.eps} 
\begin{center}
(c)
\end{center}
\caption{Choice of Sampling Rate}
\label{fig:sample-rate}
\end{figure}

Indeed, this observation is captured in what is known as the
\emph{Nyquist-Shannon Sampling Theorm} that, stated informally, says
that the accurate reproduction of an analog signal (no matter how
complicated) requires a sampling rate that is at least twice the
highest frequency of the signal of interest.

For example, for audio signals, if the highest frequency humans can
hear is 20 kHz, then we need to sample at a rate of at least 40 kHz
for a faithful reproduction of sound.  In fact, CD's are recorded at
44.1 kHz.  But many people feel that this rate is too low, as some
people can hear beyond 20 kHz.  Another recording studio standard is
48 kHz.  Interestingly, a good analog tape recorder from generations
ago was able to record signals with frequency content even higher than
this---perhaps digital is not always better!

\subsection{From Continuous to Discrete}
\label{sec:discrete}

Recall the definition of a sine wave from Section \ref{sec:frequency}:
\[ s(t) = A\sin(\omega t + \phi) \]
We can easily and intuitively convert this to the discrete domain by
replacing the time $t$ with the quantity $\nicefrac{n}{r}$, where $n$
is the integer index into the sequence of discrete samples, and $r$ is
the sampling rate discussed above. If we use $s[n]$ to denote the
$(n+1)^{\rm th}$ sample of the signal, we have:
\[ s[n] = A\sin\left(\frac{\omega n}{r} + \phi\right),\ \ \ \ \ \ \ \ 
                              n = 0, 1, ..., \infty \]
Thus $s[n]$ corresponds to the signal's value at time $\nicefrac{n}{r}$.

\subsection{Fixed-Waveform Table-Lookup Synthesis}
\label{sec:wavetable}

One of the most fundamental questions in digital audio is how to
generate a sine wave as efficiently as possible, or, in general, how
to generate a fixed periodic signal of any form (sine wave, square
wave, sawtooth wave, even a sampled sound bite).  A common and
efficient way to generate a periodic signal is through
\emph{fixed-waveform table-lookup synthesis}.  The idea is very
simple: store in a table the samples of a desired periodic signal, and
then index through the table at a suitable rate to reproduce that
signal at some desired frequency.  The table is often called a
\emph{wavetable}.

In general, if we let:
\[\begin{array}{lcl}
L &=& {\rm table\ length}        \\
f &=& {\rm resulting\ frequency} \\
i &=& {\rm indexing\ increment}   \\
r &=& {\rm sample\ rate}
\end{array}\]
then we have:
\[ f = \frac{i r}{L} \]

For example, suppose the table contains 8196 samples.  If the sample
rate is 44.1 kHz, how do we generate a tone of, say, 440 Hz?  Plugging
in the numbers and solving the above equation for $i$, we get:
\[\begin{array}{lcl}
440 &=& \dfrac{i \times 44.1 {\rm kHz}}{8196} \\[.1in]
i   &=& \dfrac{440 \times 8196}{44.1 {\rm kHz}} \\[.1in]
    &=& 81.77
\end{array}\]
So, if we were to sample approximately every 81.77$^{\rm th}$ value in
the table, we would generate a signal of 440 Hz.

Now suppose the table $T$ is a vector, and $T[n]$ is the $n$th
element.  Let's call the exact index increment $i$ into a continuous
signal the \emph{phase}, and the actual index into the corresponding
table the \emph{phase index} $p$.  The computation of successive
values of the phase index and output signal $s$ is then captured by
these equations:
\[\begin{array}{lcl}
p_o    &=& \lfloor \phi_0 + 0.5 \rfloor \\
p_{n+1} &=& (p_n + i) \bmod L \\
s_n    &=& T [\ \lfloor p_n + 0.5 \rfloor\ ]
\end{array}\]
$\lfloor a+0.5 \rfloor$ denotes the floor of $a+0.5$, which effectively
rounds $a$ to the nearest integer.  $\phi_0$ is the initial phase
angle (recall earlier discussion), so $p_0$ is the initial index into
the table that specifies where the fixed waveform should begin.

Instead of rounding the index, one could do better by
\emph{interpolating} between values in the table, at the expense of
efficiency.  In practice, rounding the index is often good enough.
Another way to increase accuracy is to simply increase the size of the
table.

\subsection{Aliasing}
\label{sec:aliasing}

Earlier we saw examples of problems that can arise if the sampling
rate is not high enough.  We saw that if we sample a sine wave at
twice its frequency, we can suitably capture that frequency.  If we
sample at exactly its frequency, we get 0 Hz.  But what happens in
between?  Consider a sampling rate ever-so-slightly higher or lower
than the sine wave's fundamental frequency-–-in both cases, this will
result in a frequency much lower than the original signal, as shown in
Figures \ref{fig:aliasing1} and \ref{fig:aliasing2}.  This is
analogous to the effect of seeing spinning objects under fluorescent
or LED light, or old motion pictures of the spokes in the wheels of
horse-drawn carriages.

\begin{figure}[hbtp]
\centering
\includegraphics[height=4in,angle=270]{pics/aliasing_lowf1.eps}
\includegraphics[height=4in,angle=270]{pics/aliasing_lowf2.eps} 
\caption{Aliasing 1}
\label{fig:aliasing1}
\end{figure}

\begin{figure}[hbtp]
\centering
\includegraphics[height=4in,angle=270]{pics/aliasing_lowf5.eps} 
\includegraphics[height=4in,angle=270]{pics/aliasing_lowf6.eps} 
\caption{Aliasing 2}
\label{fig:aliasing2}
\end{figure}

These figures suggest the following.  Suppose that $m$ is one-half the
sampling rate.  Then:
\[\begin{array}{lll}
\hline \\
{\rm Original\ signal} && {\rm Reproduced\ signal} \\
\hline
0-m                   && 0-m \\
m-2m		      && m-0 \\
2m-3m                 && 0-m \\
3m-4m                 && m-0 \\
\cdots                && \cdots \\
\hline
\end{array}\]
This phenomenon is called \emph{aliasing}, or \emph{foldover} of the
signal onto itself.

This is not good!  In particular, it means that audio signals in the
ultrasonic range will get ``folded'' into the audible range.  To solve
this problem, we can add an analog \emph{low-pass filter} in front of
the ADC--–usually called an \emph{anti-aliasing} filter---to eliminate
all but the audible sound before it is digitized.  In practice,
however, this can be tricky.  For example, a steep analog filter
introduces \emph{phase distortion} (i.e.\ frequency-dependent time
delays), and early digital recordings were notorious in the ``harsh
sound'' that resulted.  This can be fixed by using a filter with less
steepness (but resulting in more aliasing), or using a time
correlation filter to compensate, or using a technique called
\emph{oversampling}, which is beyond the scope of this text.

A similar problem occurs at the other end of the digital audio
process---i.e.\ when we reconstruct an analog signal from a digital
signal using a \emph{digital-to-analog converter}, or DAC.  The
digital representation of a signal can be viewed mathematically as a
stepwise approximation to the real signal, as shown in Figure
\ref{fig:no-aliasing}, where the sampling rate is ten times the
frequency of interest.  As discussed earlier, at the highest frequency
(i.e.\ at one-half the sampling rate), we get a square wave.  As we
will see in Chapter~\ref{ch:spectrum-analysis}, a square wave can be
represented mathematically as the sum of an infinite sequence of sine
waves, consisting of the fundamental frequency and all of its odd
harmonics.  These harmonics can enter the ultrasonic region, causing
potential havoc in the analog circuitry, or in a dog's ear (dogs can
hear frequencies much higher than humans).  The solution is to add yet
another low-pass filter, called an \emph{anti-imaging} or
\emph{smoothing} filter to the output of the DAC.  In effect, this
filter ``connects the dots,'' or interpolates, between successive
values of the stepwise approximation.

\begin{figure}[hbtp]
\centering
\includegraphics[height=3.2in,angle=270]{pics/noaliasing.eps} 
\caption{A Properly Sampled Signal}
\label{fig:no-aliasing}
\end{figure}

In any case, a basic block diagram of a typical digital audio
system---from sound input to sound output---is shown in Figure
\ref{fig:DAW-block-diagram}.

\begin{figure}
\centering
\includegraphics[height=4.0in]{pics/DAWBlockDiagram.eps}
\caption{Block Diagram of Typical Digital Audio System}
\label{fig:DAW-block-diagram}
\end{figure}

\subsection{Quantization Error}
\label{sec:quantization}

In terms of amplitude, remember that we are using digital numbers to
represent an analog signal.  For conventional CD's, 16 bits of
precision are used.  If we were to compute and then ``listen to'' the
round-off errors that are induced, we would hear subtle imperfections,
called \emph{quantization error}, or more commonly, ``noise.''

One might compare this to ``hiss'' on a tape recorder (which is due to
the molecular disarray of the magnetic recording medium), but there
are important differences.  First of all, when there is no sound,
there is no quantization error in a digital signal, but there is still
hiss on a tape.  Also, when the signal is very low and regular, the
quantization error becomes somewhat regular as well, and is thus
audible as something different from hiss.  Indeed, it's only when the
signal is loud and complex that quantization error compares favorably
to tape hiss.

One solution to the problem of low signal levels mentioned above is to
purposely introduce noise into the system to make the signal less
predictable.  This fortuitous use of noise deserves a better name, and
indeed it is called \emph{dither}.

\subsection{Dynamic Range}
\label{sec:dynamic-range}

What is the dynamic range of an $n$-bit digital audio system?  If we
think of quantization error as noise, it makes sense to
use the equation for $\mathit{SNR}_{dB}$ given in Section \ref{sec:amplitude}:

\[ \mathit{SNR}_{dB} = 20 \log_{10}\frac{S}{N} \]

But what should $N$ be, i.e.\ the quantization error?  Given a signal
amplitude range of $\pm a$, with $n$ bits of resolution it is divided
into $\nicefrac{2a}{2^n}$ points.  Therefore the dynamic range is:
\[\begin{array}{lcl}
20 \log_{10}\left(\dfrac{2a}{\nicefrac{2a}{2^n}}\right) 
           &=&       20 \times\log_{10}(2^n) \\
           &=&       20 \times n \times\log_{10} (2) \\[.02in]
           &\approx& 20 \times n \times (0.3) \\[.02in]
           &=&       6n
\end{array}\]
For example, a 16-bit digital audio system results in a dynamic range
of 96 dB, which is pretty good, although a 20-bit system yields 120
dB, corresponding to the dynamic range of the human ear.

\vspace{.1in}\hrule

\begin{exercise}{\em
For each of the following, say whether it is a longitudinal wave or a
transverse wave:
\begin{itemize}
\item	A vibrating violin string.
\item	Stop-and-go traffic on a highway.
\item	``The wave'' in a crowd at a stadium.
\item	``Water hammer'' in the plumbing of your house.
\item	The wave caused by a stone falling in a pond.
\item	A radio wave.
\end{itemize} }
\end{exercise}

\begin{exercise}{\em
You see a lightning strike, and 5 seconds later you hear the thunder.
How far away is the lightning? }
\end{exercise}

\begin{exercise}{\em
You clap your hands in a canyon, and 2 seconds later you hear an echo.
How far away is the canyon wall? }
\end{exercise}

\begin{exercise}{\em
By what factor must one increase the RMS level of a signal to yield a
10 dB increase in sound level? }
\end{exercise}

\begin{exercise}{\em
A dog can hear in the range 60-45,000 Hz, and a bat 2,000-110,000 Hz.
In terms of the frequency response, what are the corresponding dynamic
ranges for these two animals, and how do they compare to that of
humans? }
\end{exercise}

\begin{exercise}{\em
What is the maximum number of audible overtones in a note whose
fundamental frequency is 100 Hz?  500 Hz? 1500 Hz?  5 kHz? }
\end{exercise}

\begin{exercise}{\em
Consider a continuous input signal whose frequency is f.  Devise a
formula for the frequency r of the reproduced signal given a sample
rate s. }
\end{exercise}

\begin{exercise}{\em
How much memory is needed to record 3 minutes of stereo sound using
16-bit samples taken at a rate of 44.1 kHz? }
\end{exercise}

\begin{exercise}{\em
If we want the best possible sound, how large should the table be using
fixed-waveform table-lookup synthesis, in order to cover the audible
frequency range? }
\end{exercise}

\begin{exercise}{\em
The Doppler effect occurs when a sound source is in motion.  For
example, as a police car moves toward you its siren sounds higher than
it really is, and as it goes past you, it gets lower.  How fast would
a police car have to go to change a siren whose frequency is the same
as concert A, to a pitch an octave higher? (i.e. twice the frequency)
At that speed, what frequency would we hear after the police car
passes us? }
\end{exercise}

\vspace{.1in}\hrule

\out{ -------------------------

Oversampling

Oversampling is a simple “trick” that improves dynamic range as well
as anti-aliasing.  The idea is to interpolate between digital samples.
This became popular in early CD players.

More recently so-called “1-bit oversampling” has become popular.  The
idea here is to represent signals using a single bit of quantization,
but sample at a much higher rate.  This trade-off in “information
content” is well-known mathematically, and in practice it greatly
simplifies the anti-aliasing problem, because the filter that is
needed can be far less steep (since the higher rate takes us way out
of the audible range).

}
