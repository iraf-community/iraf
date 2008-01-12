# ecarcdc - Echelle thorium-argon arc (calibrated)

file	out, hdr

out = s1
hdr = "mkexamples$archdr.dat"
i = 10

mkechelle (out, yes, ncols=512, nlines=512, norders=i,
    title="Artificial Echelle Spectrum", header=hdr,
    list=no, make=yes, comments=b1, xc=235.5, yc=INDEF, pixsize=0.027,
    profile="extracted", width=20., scattered=0., f=590., gmm=INDEF,
    blaze=INDEF, theta=INDEF, order=112, wavelength=5007.49,
    dispersion=2.61, cf=590., cgmm=226., cblaze=4.53, ctheta=-11.97,
    corder=1, cwavelength=6700., cdispersion=70., rv=0., z=no,
    continuum=20., temperature=0., lines="mkexamples$ecthorium.dat",
    sigma=0.05, >& "dev$null")

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=10., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)
