#!/bin/bash

# ver: 2018-12-10-0

# KDE 5.52
# Dimensions taken from ~/.config/plasma-org.kde.plasma.desktop-appletsrc
#[Containments][7][Applets][51]
#plugin=org.kde.plasma.mediaframe
# ...
#[Containments][7][General]
#ItemsGeometries=Applet-51:288,48,1008,912,0;
# Numbers: start-x, start-y, width, height

if [ -f ~/.pod-change-desktop ]; then
    local_config=$(cat ~/.pod-change-desktop)
    image_width=$(echo "$local_config" | grep -m 1 "^image_width=" | sed s/^image_width=//)
    image_height=$(echo "$local_config" | grep -m 1 "^image_height=" | sed s/^image_height=//)
    html_width=$(echo "$local_config" | grep -m 1 "^html_width=" | sed s/^html_width=//)
fi

image_width=${image_width:-1000}
image_height=${image_height:-700}
html_width=${html_width:-1010}
#echo "image_width: $image_width; image_height: $image_height; html_width: $html_width" # :debug:

echo "Changing desktop"; displayToUse=$(w -s | egrep -m 1 -o "^$USER +([^ ])* +:[0-9]" | egrep -o ":[0-9]$"); [ -n "$displayToUse" ] && "$HOME/bin/photo-collection-pod.pl" -m ${image_width}x${image_height} && DISPLAY="$displayToUse" ~/bin/CutyCapt --min-width=${html_width} --out="$HOME/var/run/pod/output/output.png" --url="file:///$HOME/var/run/pod/photo-pod.html" 1>>/dev/null 2>>/dev/null && echo "New desktop - OK" || echo "Error (CutyCapt or photo-collection-pod.pl)"
