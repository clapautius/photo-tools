# collection of useful commands
# ver. 2011-11-10-0

# to avoid executing this file
exit 1;

# fix xmp sidecar filenames
# rename a.xmp (generated by old photoRenameTool.pl) to a.(jpg|nef).xmp 
for i in *.xmp; do base=$(basename "$i" .xmp); if [ -e "$base.jpg" ]; then echo "$base.xmp -> $base.jpg.xmp"; mv -i "$base.xmp" "$base.jpg.xmp"; elif [ -e "$base.nef" ]; then echo "$base.xmp -> $base.nef.xmp"; mv -i "$base.xmp" "$base.nef.xmp"; else echo -e "\nERROR. No image file for $base.xmp"; fi; done

# replace invalid comment (containing only spaces) from xmp files generated by old exiftool
for i in *.xmp; do sed -i.orig "s#<rdf:li xml:lang='x-default'> \+</rdf:li>#<rdf:li xml:lang='x-default'/>#" "$i"; done

