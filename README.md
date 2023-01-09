# BestBooks - COBOL
A COBOL version of BestBooks CORE.

# Open Cobol
OpenCobol is used to build BestBooks COBOL.

To install,

	sudo apt install gnucobol3

To compile a cobol source file,

	cobc -m bestbooks.cob

## OpenCobol Manual
The manual is available at https://edoras.sdsu.edu/doc/opencobol.pdf and https://gnucobol.sourceforge.io/guides/OpenCOBOL%20Programmers%20Guide.pdf

# Build the COBOL Example

	cobc -x main.cob lib/node-exec*.cbl



