# Create demo data if needed.

cl (< "demos$mkdoslit.cl")

unlearn doslit
sparams.extras = no
sparams.coordlist = "linelists$idhenear.dat"
delete demologfile,demoplotfile verify=no >& dev$null

# Execute playback.
if (substr (envget("stdgraph"), 1, 7) == "xtermjh")
    stty (playback="demos$xdoslit.dat", nlines=24, verify=no, delay=0)
else if (substr (envget("stdgraph"), 1, 5) == "gterm")
    stty (playback="demos$gdoslit.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
