# Create demo data if needed.

artdata
mkfibers ("demoobj", type="objnosky", fibers="demos$mkdo3fiber.dat",
    title="Coude artificial image", header="demos$demoobj1.dat",
    ncols=50, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkdo3fiber.dat",
    title="Coude artificial image", header="demos$demostd1.dat",
    ncols=50, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc", type="henear", fibers="demos$mkdo3fiber.dat",
    title="Coude artificial image", header="demos$demoarc1.dat",
    ncols=50, nlines=256, wstart=5786., wend=7362., seed=3)
