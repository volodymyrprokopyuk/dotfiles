#!/usr/bin/env bash

set -eu

# Merge multiple PDF files into a single PDF file
# Usage: ./merge_pdf.sh output.pdf input1.pdf input2.pdf
function merge_pdf {
    gs -dNOPAUSE -dBATCH -dQUIET -dPDFSETTINGS=/prepress -sDEVICE=pdfwrite \
        -sOutputFile="${@}"
}

# Reduce size/quality of a PDF from merged JPEG images
# convert -density 200 -quality 60 -compress jpeg input.pdf output.pdf

# Convert the first page of a PDF file to a PNG image
# convert -density 300 -quality 90 -alpha remove 'input.pdf[0]' output.png

merge_pdf "${@}"
