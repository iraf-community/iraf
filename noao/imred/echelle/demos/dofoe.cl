# Create demo data if needed.

task $mkdofoe=demos$mkdofoe.cl
mkdofoe

unlearn dofoe params
delete ("demologfile,demoplotfile", verify=no, >& "dev$null")

# Execute playback.
if (substr (envget("stdgraph"), 1, 6) == "xgterm")
    stty (playback="demos$xgdofoe.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
