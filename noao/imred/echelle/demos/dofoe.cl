# Create demo data if needed.

task $mkdofoe=demos$mkdofoe.cl
mkdofoe

unlearn dofoe params
delete ("demologfile,demoplotfile", verify=no, >& "dev$null")

# Execute playback.
s1 = envget ("stdgraph")
if (s1 == "xtermjh")
    stty (playback="demos$xdofoe.dat", nlines=24, verify=no, delay=0)
else if (s1 == "gterm")
    stty (playback="demos$gdofoe.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Cannot run demo with current terminal type")
