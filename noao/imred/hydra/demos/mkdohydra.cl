# Create demo data if needed.

artdata
mkfibers ("demoobj", type="object", fibers="demos$mkdohydra1.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkdohydra1.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc", type="henear", fibers="demos$mkdohydra1.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=3)
mkfibers ("demostd", type="object", fibers="demos$mkdohydra2.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=1)

# Create the setup files.
delete ("demoapid1", verify=no, >& "dev$null")
list = "demos$mkdohydra1.dat"
while (fscan (list, i, j) != EOF)
    print (i, j, >> "demoapid1")
list = ""
delete ("demoapid2", verify=no, >& "dev$null")
list = "demos$mkdohydra2.dat"
while (fscan (list, i, j) != EOF)
    print (i, j, >> "demoapid2")
list = ""
