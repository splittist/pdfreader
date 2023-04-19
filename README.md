# PDFREADER
Reading and rendering PDF files in Common Lisp.

# Notes
All I wanted to do was extract text from some PDFs. This has turned out to be surprisingly difficult, and has led me through the detours of dealing with (selected parts of) compression, embedded font parsing and, while I was doing that, rendering a visual representation (which is, after all, what PDFs are all about).

These are _very_ early days for the project.

Because PDF is a binary format that pretends to be ASCII at some points, I use reader macros #! and #", which are analagous to #\ and " in that they take a character or a string, but return an octet (instead of a Lisp character) or a vector of octets (instead of a Lisp string). There is a naive octets= for comparison of octet vectors. (Because Gnu Emacs doesn't understand these reader macros, parsing with #!( and #!) confuses it mightily, which is why I use the actual char-codes in certain parts.)

Because I wrote the parsing code in a PEEK-and-READ style, and beacuase there isn't a PEEK-BYTE in the standard, there are simple octet-file and octet-vector streams in utils.lisp that implement a version of PEEK-BYTE based on EdiWare. In particular, it uses a different (less annoying?) order of arguments from PEEK-CHAR.

# Acknowledgements
I continue to be inspired by the following work:
* [cl-pdf](https://github.com/mbattyani/cl-pdf) ([license](https://github.com/mbattyani/cl-pdf/blob/master/license.txt))
* [pdf.js](https://github.com/mozilla/pdf.js) ([license](https://github.com/mozilla/pdf.js/blob/master/LICENSE))
* [poppler](https://github.com/freedesktop/poppler) ([license](https://github.com/freedesktop/poppler/blob/master/COPYING)) and other descendants of Glyph & Cog's [XPDF](https://www.xpdfreader.com/)
* [mupdf](https://github.com/ArtifexSoftware/mupdf) ([license](https://github.com/ArtifexSoftware/mupdf/blob/master/COPYING))
