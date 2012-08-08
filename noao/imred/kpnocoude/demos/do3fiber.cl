# Create demo data if needed.

cl (< "demos$mkdo3fiber.cl")

unlearn do3fiber
params.coordlist = "linelists$idhenear.dat"
params.match = 10.
delete demologfile,demoplotfile verify=no >& dev$null

# Execute playback.
if (substr (envget("stdgraph"), 1, 6) == "xgterm")
    stty (playback="demos$xgdo3fiber.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
