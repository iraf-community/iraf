# Create demo data if needed.

unlearn background calibrate identify illumination reidentify response
unlearn sensfunc setairmass setjd splot standard fitcoords transform
imdel demo*.imh
cl (< "demos$mktestt.cl")
delete demolist,demodelfile,demologfile,demoplotfile,demostdfile v- >& dev$null
if (access ("database"))
    delete database/* v- >& dev$null
;
reidentify.logfile="demologfile"
fitcoords.deletions="demodelfile"
fitcoords.logfiles="STDOUT,demologfile"
fitcoords.plotfile="demoplotfile"
transform.logfiles="STDOUT,demologfile"

# Execute playback.
if (substr (envget("stdgraph"), 1, 6) == "xgterm")
    stty (playback="demos$xgtest.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
