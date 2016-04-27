#!/bin/bash

usage()
{
    echo "Usage: gimp-batch-scan-postproc [<output-dir>]"
}


if [ "$1"_ == "-h_" ]; then
    usage
    exit 1
fi

if [ -n "$1" ]; then
    dest_dir="$1"
else
    dest_dir="."
fi

echo "Using dir. $dest_dir as destination."

{
cat "$(dirname $(realpath $0))/gimp-batch-functions.sch"

for i in *.png; do
    outputfile="$dest_dir/$(basename "$i" .png)-pp.jpg"
    if [ ! -e "$outputfile" ]; then
        echo "(gimp-message \"Converting $i (saving to $outputfile)\")"
        echo "(postproc-scan-img-and-save-jpeg \"$i\" \"$outputfile\" TRUE TRUE)"
    else
        echo "(gimp-message \"$outputfile already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
