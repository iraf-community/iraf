# HENEAR2D - Longslit Helium-Neon-Argon Arc Spectrum

file	out, arc, dat

out = s1
arc = mktemp ("art")
dat = mktemp ("art")

mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=512, naps=1,
    wstart=4209.0+i, wend=7361.7+i, format="onedspec",
    title="Helium-Neon-Argon Arc Example", header="artdata$stdheader.dat",
    continuum=0.5, slope=0., temperature=0., lines="mkexamples$henear2.dat",
    nlines=50, peak=-0.5, sigma=6., seed=i, comments=b1)

print (arc, " 100 slit 90 0 50 0", >> dat)
mk2dspec (out, output="", model=dat, comments=b1, ncols=100, nlines=512,
    title="Helium-Neon-Argon Arc Example",  header="artdata$stdheader.dat")

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=10., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)

imdelete (arc, verify=no)
delete (dat, verify=no)
