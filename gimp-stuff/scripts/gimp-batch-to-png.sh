#!/bin/bash

if [ -n "$1" ]; then
    dest_dir="$1"
else
    dest_dir="."
fi

echo "Using dir. $dest_dir as destination."

{
cat "$(dirname $(realpath $0))/gimp-batch-functions.sch"

for i in *.xcfgz; do
    if [ ! -e "$dest_dir/${i%%.xcfgz}.png" ]; then
        echo "(gimp-message \"Converting $i\")"
        echo "(convert-xcf-to-png \"$i\" \"$dest_dir/${i%%.xcfgz}.png\")"
    else
        echo "(gimp-message \"$dest_dir/${i%%.xcfgz}.png already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
