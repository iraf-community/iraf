# LONGSLIT - Long slit example

real	w1, w2
file	out, obj, bkg, dat, hdr

out = s1
hdr = s2
obj = mktemp ("art")
bkg = mktemp ("art")
dat = mktemp ("art")

w1 = 4209 + i
w2 = 7361 + i

if (i == 2) {		# Featureless hot continuum + sky
    mk1dspec (obj, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=10000., lines="", nlines=0, peak=-0.5, profile="gaussian",
	gfwhm=23.5, seed=1, comments=b1)
    mk1dspec (bkg, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=5800., lines="", nlines=20, peak=1., profile="gaussian",
	gfwhm=12, seed=1, comments=b1)
    print (obj, " 1 gauss 3 0 50 .002", > dat)
    print (bkg, " 10 slit 90 0 50 0", >> dat)
} else if (i == 3) {	# Sky only
    mk1dspec (bkg, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=5800., lines="", nlines=20, peak=1., profile="gaussian",
	gfwhm=12, seed=1, comments=b1)
    print (bkg, " 10 slit 90 0 50 0", > dat)
} else if (i == 4) {	# Flat field
    mk1dspec (bkg, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=8000., lines="", nlines=0, peak=-0.5, profile="gaussian",
	gfwhm=23.5, seed=1, comments=b1)
    print (bkg, " 10 slit 90 0 50 0", > dat)
} else if (i == 5) {	# HE-NE-AR
    mk1dspec (bkg, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=0.5, slope=0.,
	temperature=0., lines="mkexamples$henear2.dat", profile="gaussian",
	gfwhm=14, comments=b1)
    print (bkg, " 100 slit 90 0 50 0", > dat)
} else if (i == 6) {	# Galaxy absorption line spectrum + sky
    mk1dspec (obj, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=7000., lines="", nlines=50, peak=-0.5, profile="gaussian",
	gfwhm=23.5, seed=i+1, comments=b1)
    mk1dspec (bkg, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=5800., lines="", nlines=20, peak=1., profile="gaussian",
	gfwhm=12, seed=1, comments=b1)
    print (obj, " 10 gauss 30 0 50 .002", > dat)
    print (bkg, " 10 slit 200 0 50 0", >> dat)
} else {		# Star absorption line spectrum + sky
    mk1dspec (obj, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=7000., lines="", nlines=50, peak=-0.5, profile="gaussian",
	gfwhm=23.5, seed=i+1, comments=b1)
    mk1dspec (bkg, output="", ap=1, rv=0., z=no, title="", header="",
	ncols=512, naps=1, wstart=w1, wend=w2, continuum=1000., slope=0.,
	temperature=5800., lines="", nlines=20, peak=1., profile="gaussian",
	gfwhm=12, seed=1, comments=b1)
    print (obj, " 1 gauss 3 0 50 .002", > dat)
    print (bkg, " 10 slit 90 0 50 0", >> dat)
}

mk2dspec (out, output="", model=dat, comments=b1, ncols=100, nlines=512,
    title="Example artificial long slit image",  header=hdr)

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=3., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)

imdelete (obj, verify=no, >& "dev$null")
imdelete (bkg, verify=no, >& "dev$null")
delete (dat, verify=no, >& "dev$null")
