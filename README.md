# Onsong-Parser

A parser for .onsong and .chopro files to create html files, made in Haskell.
Written for my static site generator (see that repository once it exists) to create an interface for a giant onsong-library I wanted to host on a raspberry pi.
It was made out of the want to share some songs with people for a gathering without everyone owning the Onsong App and not being able to project the lyrics onto a screen for everyone.

## Installation

After cloning this repository compile the source files onsong.hs or chopro.hs with ghc for parsing of .onsong and .chopro files respectively. 
```ghc onsong.hs```
Dependencies for this project include the Prelude, Data.Text and Text.Parsec Haskell Libraries.

## Usage

Currently you just run the compiled program and apply a filename without a file ending, the program will add that on its own.
Creates a html file with the song data.

## Future Developent

Metadata parsing is something I'm currently working on.

