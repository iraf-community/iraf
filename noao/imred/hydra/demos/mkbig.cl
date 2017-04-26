# Create demo data if needed.

artdata
artdata.nxc = 5
artdata.nyc = 5
artdata.nxsub = 10
artdata.nysub = 10
artdata.nxgsub = 5
artdata.nygsub = 5
artdata.dynrange = 100000.
artdata.psfrange = 10.
artdata.ranbuf = 0

mkfibers ("demoobj", type="object", fibers="demos$mkbig.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=1500, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkbig.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=1500, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc", type="henear", fibers="demos$mkbig.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=1500, nlines=256, wstart=5786., wend=7362., seed=3)

# Create the setup files.
delete ("demoapid", verify=no, >& "dev$null")
list = "demos$mkbig.dat"
while (fscan (list, i, j) != EOF)
    print (i, j, >> "demoapid")
list = ""
