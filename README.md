# 一右二烏

## Purpose

This is a tool to generate printable kanji flashcards, because the free existing sets I found where not satisfying to me. The goal is to have the flashcards:

 - Good looking (design was inspired by White Rabbit flashcards)
 - Automatically generated
 - Multilingual. This is not work in progress / postponed.

## Example

## Requirements

- stack
- inkscape (to generate pdf/tex from svg strokes diagrams)
- xelatex
- a Kanjidic2 (xml) copy
- a JMdict (xml) copy

## Usage

### Build

> stack build

### Create .tex flashcard file for the kanji of a text file

> stack exec ichimiginikarasu-exe -- -f resources/jpn_words_girardi_kelly -j resources/JMdict -k resources/kanjidic2.xml -n mydeck -i inputfile > mydeck.tex

### Generate .pdf from .tex

> xelatex mydeck.tex

### Advanced

Launch options are available using:
> stack exec ichimiginikarasu-exe -- -h

Grepping 'error' from the latex compilation can help identify potential typesetting problems (content too large and overlapping typically):
> xelatex mydeck.tex | grep 'error'

## Issues

- The issues file list kanji that are rendered somewhat off
- tex & pdf files are generated during the execution in the 'resources/kanji_vg directory' and are not cleaned
- Some interesting compounds readings and translations are missing, as no good strategy was found to select them. E.g.: 空's reading 'そら - sky' is missing, whereas 'から - empty' is.

## Acknowledgements

Lots of existing resources were necessary to create this tool and flashcards. In no particular order:

- The kanji stroke diagrams by Ulrich Apel
- The kanji stroke diagrams converter by Kim Ahlström of jisho.org
- The kradfile, JMdict, kanjidic2 by the Electronic Dictionary Research & Development Group at Monash university
- The Kelly & Girardi Japanese words frequency list
- The kanji similarity matrix, by Lars Jensen
See the LICENSE file for details, and these sites for similar resources: http://ftp.monash.edu.au/pub/nihongo/00INDEX.html, http://lars.yencken.org/datasets/phd/

## Future features

- Multi language
- Custom input (ie. kanji content specified in individual files, not loaded from dictionaries)

