#!/bin/bash
echo $*
for fullfilename in $*
do
    filename="${fullfilename##*/}"
    extension="${filename##*.}"
    if [ $extension = "fit" ]
    then
       gpxfilename="${filename}.gpx"
       echo "${fullfilename} -> ${gpxfilename}"
       gpsbabel -t -r -w -i garmin_fit -f $fullfilename -o gpx -F $gpxfilename
    fi
done