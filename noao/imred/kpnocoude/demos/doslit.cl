# Create demo data if needed.

cl (< "demos$mkdoslit.cl")

unlearn doslit
sparams.extras = no
sparams.coordlist = "linelists$idhenear.dat"
sparams.match = 10.
delete demologfile,demoplotfile verify=no >& dev$null

# Execute playback.
if (substr (envget("stdgraph"), 1, 6) == "xgterm")
    stty (playback="demos$xgdoslit.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
