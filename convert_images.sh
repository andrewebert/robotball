#!/bin/zsh

for i in static/img/*.svg static/img/*/*.svg
    do
        #inkscape -D -d=300 -e=${i%.*}.png $i
        inkscape -T --export-plain-svg=${i%.*}.path.svg $i
    done
