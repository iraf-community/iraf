# ecobj2d - Echelle object slit spectrum

file	out, hdr

out = s1
hdr = "mkexamples$objhdr.dat"
i = 10

mkechelle (out, yes, ncols=512, nlines=512, norders=i,
    title="Artificial Echelle Spectrum", header=hdr,
    list=no, make=yes, comments=b1, xc=235.5, yc=INDEF, pixsize=0.027,
    profile="gaussian", width=4., scattered=25., f=590., gmm=31.6, blaze=63.,
    theta=69., order=112, wavelength=5007.49, dispersion=2.61, cf=590.,
    cgmm=226., cblaze=4.53, ctheta=-11.97, corder=1, cwavelength=6700.,
    cdispersion=70., rv=0., z=no, continuum=500., temperature=7700.,
    lines="", nrandom=500, peak=-0.5, sigma=0.5, seed=i, >& "dev$null")

mkechelle (out, yes, ncols=512, nlines=512, norders=i,
    title="Artificial Echelle Spectrum", header="",
    list=no, make=yes, comments=b1, xc=235.5, yc=INDEF, pixsize=0.027,
    profile="slit", width=20., scattered=10., f=590., gmm=31.6, blaze=63.,
    theta=69., order=112, wavelength=5007.49, dispersion=2.61, cf=590.,
    cgmm=226., cblaze=4.53, ctheta=-11.97, corder=1, cwavelength=6700.,
    cdispersion=70., rv=0., z=no, continuum=200., temperature=5700.,
    lines="", nrandom=20, peak=5.0, sigma=0.1, seed=i+1, >& "dev$null")

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=10., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)
