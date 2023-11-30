a few tools to help plan rides
------------------------------

[![Continuous Integration](https://github.com/benoitpas/rides-planner/actions/workflows/main.yml/badge.svg)](https://github.com/benoitpas/rides-planner/actions/workflows/main.yml)

* fit2gpx.sh converts Garmin fit files (using gpsbabel) which are binary to gpx files (xml) as xml files are easier to process
* the next step is to write a program that will extract route segments from the fit files
* then assembling the segments it will be possible to create ride routes with a given length/elevation