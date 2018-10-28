#!/bin/bash

# ver: 2018-10-28-0

# dimensions taken from plasma-desktop-appletsrc:
# [Containments][1][Applets][18]
# geometry=0,35,969,832

echo "Changing desktop"; displayToUse=$(w -s | egrep -m 1 -o "^$USER +([^ ])* +:[0-9]" | egrep -o ":[0-9]$"); [ -n "$displayToUse" ] && "$HOME/bin/photo-collection-pod.pl" && DISPLAY="$displayToUse" ~/bin/CutyCapt --min-width=969 --out="$HOME/var/run/pod/output/output.png" --url="file:///home/me/var/run/pod/photo-pod.html" 1>>/dev/null 2>>/dev/null && echo "New desktop - OK" || echo "CutyCapt error"
