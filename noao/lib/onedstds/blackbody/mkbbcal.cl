procedure mkbbcal (band, w1, w2, dw, weff, mab)

string	band		{prompt="Bandpass"}
real	w1		{prompt="Starting wavelength (um)"}
real	w2		{prompt="Ending wavelength (um)"}
real	dw		{prompt="Wavelength step (um)"}

real	weff		{prompt="Effective wavelength (um)"}
real	mab		{prompt="M_AB of Vega at eff. wavelength"}

begin
	string	b
	real	x1, x2, dx, x, m

	b = band
	x1 = w1
	x2 = w2
	dx = dw
	x = weff
	m = mab

	printf ("# type blackbody\n")
	printf ("# units microns\n")
	printf ("# band %s\n", b)
	printf ("# weff %g\n\n", x)
	for (x=x1; x<=x2; x=x+dx)
	    printf ("%.5g\t%.6g\t%g\n", x, m, dx)
end
