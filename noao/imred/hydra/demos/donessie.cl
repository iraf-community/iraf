# Create demo data if needed.

cl (< "demos$mkdonessie.cl")

unlearn dohydra params
delete ("demologfile,demoplotfile", verify=no, >& "dev$null")

# Execute playback.
if (envget("stdgraph") == "xtermjh")
    stty (playback="demos$xdonessie.dat", nlines=24, verify=no, delay=0)
else if (envget("stdgraph") == "gterm")
    stty (playback="demos$gdonessie.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
