# spectrum - Object spectrum (calibrated)

file	out, hdr

out = s1
hdr = "mkexamples$objhdr.dat"

for (k=1; k<=i; k+=1) {
    x = k * 100.
    mk1dspec (out, output="", ap=k, rv=0., z=no, ncols=512, naps=i,
	wstart=4210.0, wend=7362.7, title="Artificial Spectrum",
	header=hdr, continuum=x, slope=0., temperature=5700.,
        lines="", nlines=50, peak=-0.5, profile="gaussian", gfwhm=24,
	seed=i, comments=b1)
}

mknoise (out, output="", ncols=512, nlines=512, title="", header="",
    background=0., gain=1., rdnoise=3., poisson=yes, seed=j, cosrays="",
    ncosrays=0, energy=30000., radius=0.5, ar=1., pa=0., comments=b1)
