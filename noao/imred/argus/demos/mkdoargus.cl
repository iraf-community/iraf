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

mkfibers ("demoobj", type="object", fibers="demos$mkdoargus.dat",
    title="Argus artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkdoargus.dat",
    title="Argus artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc", type="henear", fibers="demos$mkdoargus.dat",
    title="Argus artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=3)
