# Create demo data if needed.

cl (< "demos$mkdo3fiber.cl")

unlearn do3fiber
params.coordlist = "linelists$idhenear.dat"
params.match = 10.
delete demologfile,demoplotfile verify=no >& dev$null

# Execute playback.
if (substr (envget("stdgraph"), 1, 7) == "xtermjh")
    stty (playback="demos$xdo3fiber.dat", nlines=24, verify=no, delay=0)
else if (substr (envget("stdgraph"), 1, 5) == "gterm")
    stty (playback="demos$gdo3fiber.dat", nlines=24, verify=no, delay=0)
else
    error (1, "Playback for current terminal type not available")
