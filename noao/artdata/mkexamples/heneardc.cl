# heneardc - Helium-Neon-Argon spectrum (calibrated)

file	out, hdr

out = s1
hdr = "mkexamples$archdr.dat"

for (k=1; k<=i; k+=1) {
    mk1dspec (out, output="", ap=k, rv=0., z=no, ncols=512, naps=i,
	wstart=4210.0, wend=7362.7, title="Helium-Neon-Argon Arc Example",
	header=hdr, continuum=0.5, slope=0., temperature=0.,
	lines="mkexamples$henear1.dat", profile="gaussian",
	gfwhm=14, comments=b1)
}

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=10., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)
