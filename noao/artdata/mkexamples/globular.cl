# GLOBULAR - Globular cluster

file	image, dat

image = s1
dat = mktemp ("art")

starlist (dat, 5000, "", "", interactive=no, spatial="hubble",
    xmin=1., xmax=512., ymin=1., ymax=512., xcenter=INDEF,
    ycenter=INDEF, core_radius=30., base=0., sseed=i,
    luminosity="bands", minmag=-7., maxmag=0., mzero=-4., power=0.6,
    alpha=0.74, beta=0.04, delta=0.294, mstar=1.28, lseed=i,
    nssample=100, sorder=10, nlsample=100, lorder=10,
    rbinsize=10., mbinsize=0.5, graphics="stdgraph", cursor="")

mkobjects (image, output="", ncols=512, nlines=512,
    title="Example artificial globular cluster",
    header="artdata$stdheader.dat", background=1000., objects=dat,
    xoffset=0., yoffset=0., star="moffat", radius=1.0, beta=2.5,
    ar=1., pa=0., distance=1., exptime=1., magzero=7.,
    gain=3., rdnoise=10., poisson=yes, seed=j, comments=b1)

delete (dat, verify=no)
