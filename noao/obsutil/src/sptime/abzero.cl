procedure abzero (w, logf)

real	w		{prompt="Wavelength (microns)"}
real	logf		{prompt="Log f_lambda (W/cm^2/micron)"}

begin
	x = 10000 * w
	y = -2.5 * (logf + 3) - 5 * log10 (x) - 2.4
	printf ("%d %.3f\n", x, y)
end
