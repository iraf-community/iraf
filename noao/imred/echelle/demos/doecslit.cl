# Create demo data if needed.

task $mkdoecslit=demos$mkdoecslit.cl
mkdoecslit
imdel ("demo*.??h,sens*", verify=no, >& "dev$null")
imcopy ("Bdemoobj", "demoobj", verbose=no)
imcopy ("Bdemoarc", "demoarc", verbose=no)
imcopy ("Bdemostd", "demostd", verbose=no)

unlearn doecslit
sparams.bandwidth = 3
sparams.bandsep = 3
delete ("demologfile,demoplotfile,std", verify=no, >& "dev$null")

# Execute playback.
s1 = envget ("stdgraph")
if (s1 == "xtermjh")
    stty (playback="demos$xdoecslit.dat", nlines=24, verify=no, delay=0)
else if (s1 == "gterm")
    stty (playback="demos$gdoecslit.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Cannot run demo with current terminal type")
