# Create demo data if needed.

cl (< "demos$mkdoargus.cl")

delete ("demologfile,demoplotfile", verify=no, >& "dev$null")

unlearn doargus params

# Execute playback.
if (substr (envget("stdgraph"), 1, 7) == "xtermjh")
    stty (playback="demos$xdoargus.dat", nlines=24, verify=no, delay=0)
else if (substr (envget("stdgraph"), 1, 5) == "gterm")
    stty (playback="demos$gdoargus.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
