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

mkexample ("longslit", "demoarc1", oseed=5,  nseed=1,
    errors=no, verbose=yes, list=no)
mkheader ("demoarc1", "demos$demoarc1.dat", append=no, verbose=no)
mkexample ("longslit", "demoobj1", oseed=1,  nseed=1,
    errors=no, verbose=yes, list=no)
mkheader ("demoobj1", "demos$demoobj1.dat", append=no, verbose=no)
mkexample ("longslit", "demostd1", oseed=2, nseed=2,
    errors=no, verbose=yes, list=no)
mkheader ("demostd1", "demos$demostd1.dat", append=no, verbose=no)
mkexample ("longslit", "demoarc2", oseed=5,  nseed=2,
    errors=no, verbose=yes, list=no)
mkheader ("demoarc2", "demos$demoarc2.dat", append=no, verbose=no)
