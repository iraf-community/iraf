# Create demo data if needed.

artdata
mkfibers ("demoobj", type="object", fibers="demos$mkdoargus.dat",
    title="Argus artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkdoargus.dat",
    title="Argus artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc", type="henear", fibers="demos$mkdoargus.dat",
    title="Argus artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=3)
