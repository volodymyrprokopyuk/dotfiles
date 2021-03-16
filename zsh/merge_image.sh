#!/usr/bin/env bash

set -eu

# Merge multiple image files into a single PDF file
# Usage: ./merge_jpg.sh output.pdf input1.jpg input2.png
function merge_image {
    local output="${1?ERROR: missing target}"
    shift
    img2pdf "${@}" --output "${output}"
}

merge_image "${@}"
