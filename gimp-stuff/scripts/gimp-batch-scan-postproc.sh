#!/bin/bash

# ver. 2017-04-27-0

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

# Quality values:
# QUAL-COLOR-300DPI
# QUAL-GREY-300DPI
# See sfd-scan-postproc.scm for more.
quality="QUAL-COLOR-300DPI"

for i in *.png; do
    outputfile="$dest_dir/$(basename "$i" .png)-pp.jpg"
    if [ ! -e "$outputfile" ]; then
        echo "(gimp-message \"Converting $i (saving to $outputfile)\")"
        echo "(let ((result (postproc-scan-img-and-save-jpeg \"$i\" \"$outputfile\" TRUE TRUE $quality))) (when (= result 0) (gimp-message \"Aborting!\") (gimp-quit 0)))"
    else
        echo "(gimp-message \"$outputfile already exists\")"
    fi
done

echo "(gimp-quit 0)"
} | gimp -i -b -
