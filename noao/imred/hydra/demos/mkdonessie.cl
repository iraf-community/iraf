# Create demo data if needed.

artdata
mkfibers ("demoobj", type="object", fibers="demos$mkdonessie.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=1)
mkfibers ("demoflat", type="flat", fibers="demos$mkdonessie.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=2)
mkfibers ("demoarc1", type="ehenear", fibers="demos$mkdonessie.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=3)
mkfibers ("demoarc2", type="ohenear", fibers="demos$mkdonessie.dat",
    title="Hydra artificial image", header="demos$header.dat",
    ncols=100, nlines=256, wstart=5786., wend=7362., seed=4)
#mkfibers ("demoarc3", type="mercury", fibers="demos$mkdonessie.dat",
#    title="Hydra artificial image", header="demos$header.dat",
#    ncols=100, nlines=256, wstart=5786., wend=7362., seed=4)

# Create the setup files.
delete ("demoapid,demoarcrep", verify=no, >& "dev$null")
list = "demos$mkdonessie.dat"
while (fscan (list, i, j) != EOF)
    print (i, j, >> "demoapid")
list = ""
print ("demoarc1 demoarc2 1x2", > "demoarcrep")
