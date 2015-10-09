 _____      _                             
|  ___|    | |                            
| |__ _   _| |_ ___ _ __ _ __   ___  __ _ 
|  __| | | | __/ _ \ '__| '_ \ / _ \/ _` |
| |__| |_| | ||  __/ |  | |_) |  __/ (_| |
\____/\__,_|\__\___|_|  | .__/ \___|\__,_|
                        | |               
                        |_|               

Euterpea is a domain-specific language embedded in Haskell for 
computer music research, education, and development, providing 
both note-level and signal-level abstractions.  It is a descendant 
of Haskore and HasSound, and is intended for both educational purposes 
as well as serious computer music applications.  Euterpea is a 
wide-spectrum DSL, suitable for high-level music representation, 
algorithmic composition, and analysis; mid-level concepts such as 
MIDI; and low-level audio processing, sound synthesis, and instrument 
design.  It also includes a "musical user interface," a set of 
computer-music specific GUI widgets such as keyboards, pushbuttons, 
sliders, and so on.  The performance of Euterpea is intended to be 
as good as any other computer music language, with the goal of being 
able to develop real-time applications, using both MIDI and a 
high-performance back-end for real-time audio.  

See Liense for licensing information.

Homepage: http://haskell.cs.yale.edu/


============================
==== Getting the Source ====
============================

Currently (9/2/2015), the most up-to-date version of Euterpea is 
available through GitHub at:

    https://github.com/Euterpea/Euterpea

We recommend checking out the master version, as it should always be 
kept stable.

When we reach milestones, we will release stable versions to Hackage.


============================
======= Installation =======
============================

There is a stable version of Euterpea on Hackage, so one can easily 
install Euterpea with:
    cabal install euterpea

For the most up to date version, install from source:

  1) Clone the source from github
     git clone https://github.com/Euterpea/Euterpea

  2) cd into the Euterpea directory
     cd Euterpea

  3) install Euterpea with cabal
     cabal install

--------- Windows ----------
There are currently no further steps or known issues installing on Windows.


---------- Linux -----------
You may require additional steps to get MIDI sound output working on Linux.  
First of all, we recommend using TiMidity (http://timidity.sourceforge.net/) 
and either Freepats (http://freepats.zenvoid.org/) or PersonalCopy 
(ftp://ftp.personalcopy.net/pub/Unison.sf2.gz) for MIDI support.  
Make sure timidity is properly depending on the PersonalCopy soundfont 
if you're using it.

Make sure timidity is the default MIDI-Through port.  The easiest way to 
do this is probably to remove the default dummy port:
sudo rmmod snd_seq_dummy
Then, while Euterpea programs are running, you must have timidity running 
in the background:
timidity -iA -Os &

On gentoo:  run:
aconnect -l
This will output the Midi Through port number, say, it is port number 14,
then run:
aconnect 14:0 128:0
This will connect a timidity default port (128) to port 14
Now Euterpea should direct Midi stream to standard output.


--------- Mac OS X ---------
Euterpea is known to work fine on OS X (including Mavericks) when used with
GHC 7.8.3 & Haskell Platform 2014.2.0.0 or later.

Once Euterpea is set up, you may require additional steps to get MIDI sound
output working.  Download SimpleSynth and open it before you run ghci.  It’s
a software MIDI synthesizer that plays MIDI output through the speaker.

With GHC 7.8.3 or later, to run the GUI examples in ghci reliably, you need
to start gchi with -fno-ghci-sandbox, or set it within ghci as follows:

    ghci -fno-ghci-sandbox
    *: :set -fno-ghci-sandbox
    *: :m + Euterpea.Examples.MUI
    *: mui5

With older versions of GHC, you may need the ``EnableGUI trick''. See
Euterpea/Examples/EnableGUI.hs for details.


------ Troubleshooting -----
If you get errors about packages not being installed, make sure that cabal 
binaries are in your `$PATH`.

To add cabal binaries to your path first add 
export PATH=$HOME/.cabal/bin:$PATH
to your .bashrc and then run 
source ~/.bashrc
Now you should be able to successfully cabal install.


============================
======= Building HSoM ======
============================

This Euterpea distribution comes with the source code for the book:

    The Haskell School of Music, by Paul Hudak.

Building the source into a PDF requires LaTeX as well as the package 
lhs2TeX.  Information about LaTeX can be found at 

    http://www.latex-project.org/

and information about lhs2TeX can be found at

    http://www.andres-loeh.de/lhs2tex/

As lhs2TeX is available on Hackage, it can be installed with cabal:

    cabal install lhs2tex

Once these are ready, building the book can be achieved by running the 
batch script MakeTex.bat in the HSoM directory.  This will compile the 
lhs files into tex files, the tex files to a dvi, the dvi into a ps, and 
finally the ps to a pdf.

Note that the files of HSoM are Literate Haskell (lhs) files.  As such, 
they can be run directly with GHC.  However, the batch script MakeCode.bat 
will extract just the code.  Although they can be regenerated, these 
extracted files are already included with the Euterpea distribution, and 
many are important files for the proper functioning of the library.


============================
====== Getting Started =====
============================

A good place to begin learning about Euterpea is from the text that 
accompanies this distribution: The Haskell School of Music, by Paul Hudak.  
The source files are available in the HSoM directory, and building them 
to a PDF is described above.

Using Euterpea is generally as easy as adding

    import Euterpea

to the imports of your Haskell program.  However, for specific advanced uses, 
other specific imports can be appropriate.

Lastly, the Euterpea.Examples subdirectory contains many examples of using 
Euterpea in practice.  These examples are designed to showcase Euterpea's 
powers, but they may also be useful simply as a starting off point.


============================
======== Information =======
============================

Euterpea was created by:
    Paul Hudak <paul.hudak@cs.yale.edu>, 
    Eric Cheng <eric.cheng@aya.yale.edu>,
    Hai (Paul) Liu <hai.liu@aya.yale.edu>
and is currently maintained by
    Paul Hudak <paul.hudak@cs.yale.edu>, 
    Donya Quick <donya.quick@yale.edu>,
    Dan Winograd-Cort <daniel.winograd-cort@yale.edu>

This file was last modified on 9/2/2015
by Daniel Winograd-Cort
