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
    if [ ! -e "$dest_dir/${i%%.xcfgz}.jpg" ]; then
        echo "(gimp-message \"Converting $i\")"
        echo "(convert-xcf-to-jpeg \"$i\" \"$dest_dir/${i%%.xcfgz}.jpg\")"
    else
        echo "(gimp-message \"$dest_dir/${i%%.xcfgz}.jpg already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
