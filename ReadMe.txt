
=============================
= Installation oF EuterpeaI =
=============================

The usual cabal installation steps are as follows:

1. To configure the module:

       runhaskell Setup.hs configure

   You might need to do:

       runhaskell Setup.hs configure  --user
   
   if your libraries are installed in user space, and you can do:

       runhaskell Setup.hs configure --user --prefix=DIR

   if you want to install the package to your user's directory instead
   of the system one (replace DIR with your own directory of choice).

2. To build the module:

       runhaskell Setup.hs build

3. To install the module:

       runhaskell Setup.hs install

   This will install a top-level EuterpeaI module for GHC.


