# 一右二烏

## Purpose

This is a tool to generate printable kanji flashcards, because the free existing sets I found were not satisfying to me. The goal is to have the flashcards:

 - Good looking (design was inspired by White Rabbit flashcards)
 - Automatically generated
 
Multilingual support was an early goal, but the current state of dictionaries for language other than English makes it less promising. Hence this is currently postponed.

## Example

### Generated decks

- https://github.com/makemeunsee/ichimiginikarasu/raw/master/examples/jlpt4.pdf
- https://github.com/makemeunsee/ichimiginikarasu/raw/master/examples/jlpt3.pdf
- https://github.com/makemeunsee/ichimiginikarasu/raw/master/examples/jlpt2.pdf
- https://github.com/makemeunsee/ichimiginikarasu/raw/master/examples/jlpt1.pdf

### Samples

![Flashcard sample](https://github.com/makemeunsee/ichimiginikarasu/blob/master/example.png)

![Annotated flashcard sample](https://github.com/makemeunsee/ichimiginikarasu/blob/master/example_explained.png)

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

where 'inputfile' is the text file to get kanjis from, and 'mydeck' is the name of the deck (appears top right of the flashcards front).

Other parameters can be changed to load custom resources.

### Generate .pdf from .tex

> xelatex mydeck.tex

### Advanced

Launch options are available using:
> stack exec ichimiginikarasu-exe -- -h

Grepping 'error' from the latex compilation can help identify potential typesetting problems (content too large and overlapping typically):
> xelatex mydeck.tex | grep 'error'

## Issues

- The [issues](https://github.com/makemeunsee/ichimiginikarasu/blob/master/issues) file lists kanjis that are rendered somewhat off
- tex & pdf files are generated during the execution in the 'resources/kanji_vg directory' and are not cleaned
- Some interesting compounds readings and translations are missing, as no good strategy was found to select them. E.g.: 空's reading 'そら - sky' is missing, whereas 'から - empty' is present.

## Acknowledgements

Lots of existing resources were necessary to create this tool and flashcards. In no particular order:

- The kanji stroke diagrams by Ulrich Apel
- The kanji stroke diagrams converter by Kim Ahlström of [jisho.org](http://jisho.org)
- The kradfile, JMdict, kanjidic2 by the Electronic Dictionary Research & Development Group at Monash university
- The Kelly & Girardi Japanese words frequency list
- The kanji similarity matrix, by Lars Yencken

See the LICENSE file for details, and these sites for similar resources:

http://ftp.monash.edu.au/pub/nihongo/00INDEX.html

http://lars.yencken.org/datasets/phd/

## Future features

- Multi language
- Custom input (ie. kanji content specified in individual files, not loaded from dictionaries)

