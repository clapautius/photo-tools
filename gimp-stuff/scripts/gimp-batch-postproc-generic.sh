#!/bin/bash

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

echo "Using dir. $dest_dir as destination."

{
cat "$(dirname $(realpath $0))/$1"

for i in *.jpeg; do
    outputfile="$dest_dir/$(basename "$i" .jpeg)-pp.jpeg"
    if [ ! -e "$outputfile" ]; then
        echo "(gimp-message \"Converting $i (saving to $outputfile)\")"
        echo "($2 \"$i\" \"$outputfile\")"
    else
        echo "(gimp-message \"$outputfile already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
