# Create demo data if needed.

artdata
mkfibers ("demoobj", type="object", fibers="demos$mkdohydra.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkdohydra.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc", type="henear", fibers="demos$mkdohydra.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=3)

# Create the setup files.
delete ("demoapid", verify=no, >& "dev$null")
list = "demos$mkdohydra.dat"
while (fscan (list, i, j) != EOF)
    print (i, j, >> "demoapid")
list = ""
