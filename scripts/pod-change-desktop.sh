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

echo "Changing desktop"; displayToUse=$(w -s | egrep -m 1 -o "^$USER +([^ ])* +:[0-9]" | egrep -o ":[0-9]$"); [ -n "$displayToUse" ] && "$HOME/bin/photo-collection-pod.pl" -m 980x850 && DISPLAY="$displayToUse" ~/bin/CutyCapt --min-width=1008 --out="$HOME/var/run/pod/output/output.png" --url="file:///$HOME/var/run/pod/photo-pod.html" 1>>/dev/null 2>>/dev/null && echo "New desktop - OK" || echo "Error (CutyCapt or photo-collection-pod.pl)"
