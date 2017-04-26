# Create demo data if needed.

task $mkdoecslit=demos$mkdoecslit.cl
mkdoecslit
imdel ("demo*.??h,sens*", verify=no, >& "dev$null")
imcopy ("Bdemoobj1", "demoobj1", verbose=no)
imcopy ("Bdemoobj2", "demoobj2", verbose=no)
imcopy ("Bdemoarc", "demoarc", verbose=no)
imcopy ("Bdemostd", "demostd", verbose=no)

unlearn doecslit apscat1 apscat2
sparams.extras = no
sparams.bandwidth = 3
sparams.bandsep = 3
delete ("demologfile,demoplotfile,std", verify=no, >& "dev$null")

# Execute playback.
if (substr (envget("stdgraph"), 1, 6) == "xgterm")
    stty (playback="demos$xgdoecslit.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
