# LONGSLIT - Long slit example

file	out, obj, sky, dat

out = s1
obj = mktemp ("art")
sky = mktemp ("art")
dat = mktemp ("art")

mk1dspec (obj, output="", rv=0., z=no, title="", header="", ncols=512,
    wstart=4000., wend=8000., continuum=1000., slope=0., temperature=5700.,
    lines="", nlines=50, peak=-0.5, sigma=10., seed=1)
mk1dspec (sky, output="", rv=0., z=no, title="", header="", ncols=512,
    wstart=4000., wend=8000., continuum=1000., slope=0., temperature=4000.,
    lines="", nlines=20, peak=1., sigma=5., seed=2)

print (obj, " 1 gauss 3 0 50 .002", > dat)
print (sky, " 10 slit 90 0 50 0", >> dat)
mk2dspec (out, output="", model=dat, ncols=100, nlines=512,
    title="Example artificial long slit image",  header="artdata$stdheader.dat")

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=3., poisson=yes, seed=1, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0.)

imdelete (obj, verify=no)
imdelete (sky, verify=no)
delete (dat, verify=no)
