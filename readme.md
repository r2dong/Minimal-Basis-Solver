Minimal Basis Solver
===
###Overview
A minimal basis solver for relational databases is implemented using Haskell. Input are files which will be parsed using
Alex and Happy, and output (the computed minimal basis) is written to a file as well.
###Usage
####Format of Input Files
Relation:

R1 , R2 , R3 ...

FDs:

R1 , R2 -> R3 , R4 ;
R2 -> R4 ;

-- Attribute names are arbitary
####Running the Program
In a terminal or command prompt, type "cabal run inputFileName"
