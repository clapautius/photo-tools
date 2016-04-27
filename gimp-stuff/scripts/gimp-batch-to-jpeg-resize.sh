#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: gimp-batch-to-jpeg-resize.sh <size> [<output-dir>]"
    exit 1
fi

new_size="$1"
shift

if [ -n "$1" ]; then
    dest_dir="$1"
else
    dest_dir="."
fi

echo "Using dir. $dest_dir as destination."

{
cat "$(dirname $(realpath $0))/gimp-batch-functions.sch"

for i in *.xcfgz; do
    if [ ! -e "$dest_dir/${i%%.xcfgz}.jpg" ]; then
        echo "(gimp-message \"Converting $i\")"
        echo "(convert-xcf-to-jpeg-resize \"$i\" \"$dest_dir/${i%%.xcfgz}.jpg\" $new_size)"
    else
        echo "(gimp-message \"$dest_dir/${i%%.xcfgz}.jpg already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
