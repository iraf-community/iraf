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

mkexample ("longslit", "Demoflat", oseed=4,  nseed=3,
    errors=no, verbose=yes, list=no)
mkheader ("Demoflat", "demos$demoflat.dat", append=no, verbose=no)
mkexample ("longslit", "Demoarc1", oseed=5,  nseed=1,
    errors=no, verbose=yes, list=no)
mkheader ("Demoarc1", "demos$demoarc1.dat", append=no, verbose=no)
mkexample ("longslit", "Demoobj", oseed=1,  nseed=1,
    errors=no, verbose=yes, list=no)
mkheader ("Demoobj", "demos$demoobj.dat", append=no, verbose=no)
mkexample ("longslit", "Demostd", oseed=2, nseed=2,
    errors=no, verbose=yes, list=no)
mkheader ("Demostd", "demos$demostd.dat", append=no, verbose=no)
mkexample ("longslit", "Demoarc2", oseed=5,  nseed=2,
    errors=no, verbose=yes, list=no)
mkheader ("Demoarc2", "demos$demoarc2.dat", append=no, verbose=no)
imcopy ("Demoflat,Demoarc1,Demoobj,Demostd,Demoarc2",
	"demoflat,demoarc1,demoobj,demostd,demoarc2",
	verbose=yes)
