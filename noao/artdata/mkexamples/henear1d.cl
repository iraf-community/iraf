# HENEAR1D - 1D Helium-Neon-Argon Arc Spectrum

file	out

out = s1

mk1dspec (out, output="", ap=1, rv=0., z=no, ncols=512, naps=1,
    format="onedspec", wstart=4209.0+i, wend=7361.7+i,
    title="Helium-Neon-Argon Arc Example", header="artdata$stdheader.dat",
    continuum=0.5, slope=0., temperature=0., lines="mkexamples$henear2.dat",
    nlines=50, peak=-0.5, sigma=6., seed=i, comments=b1)

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=10., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)

hedit (out, "w0,wpc,crpix1,crval1,cdelt1,dc-flag", add=no, delete=yes,
    verify=no, show=no, update=yes)
