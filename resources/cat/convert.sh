#!/usr/bin/env nix-script
#!> shell
#! shell   | imagemagick inkscape mediainfo

cd $(dirname $0)

SVG=./cat.svg

function peval() {
    echo $@
    eval $@
}

width=72
height=72

PNG=./cat.png

peval inkscape \
    --export-filename=$PNG \
    --export-width=$width \
    --export-height=$height \
    $SVG

mediainfo $PNG

JPG=./cat.jpg

peval convert $PNG $JPG

mediainfo $JPG

for t in truecolor bilevel grayscale palette colorseparation optimize; do

BMP=./cat-$t.bmp

peval convert $PNG -type $t $BMP

mediainfo $BMP

done
