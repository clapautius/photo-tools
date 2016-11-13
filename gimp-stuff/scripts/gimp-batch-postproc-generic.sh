#!/bin/bash

# Ver: 2016-11-13

# uncomment this for debug messages (:fixme: - add cmd. line param.)
#g_debug=1

usage()
{
    echo "Usage: gimp-batch-postproc <sch-file> <gimp-function> [<output-dir>]"
}


if [ "$1"_ == "-h_" ]; then
    usage
    exit 1
fi

if [ -z "$1" -o -z "$2" ]; then
    usage
    exit 1
fi

if [ -n "$3" ]; then
    dest_dir="$3"
else
    dest_dir="."
fi

if [ -n "$4" ]; then
    dest_ext="$4"
fi

echo "Using dir. $dest_dir as destination."

{
cat "$(dirname $(realpath $0))/$1"

find . -maxdepth 1 \( -iname '*.png' -o -iname '*.jpg' -o -iname '*.jpeg' \) | while read i; do
    fname=$(basename "$i")
    extension=$(echo "$fname" | sed "s/.*\(\.[^.]*\)$/\1/")
    if [ -n "$dest_ext" ]; then
        output_ext="$dest_ext"
    else
        output_ext="$extension"
    fi
    outputfile="$dest_dir/$(basename "$i" "$extension")-pp$output_ext"
    ((g_debug)) && echo ":debug: $i -> $outputfile"
    if [ ! -e "$outputfile" ]; then
        echo "(gimp-message \"Converting $i (saving to $outputfile)\")"
        echo "($2 \"$i\" \"$outputfile\")"
    else
        echo "(gimp-message \"$outputfile already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
