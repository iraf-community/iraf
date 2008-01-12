procedure blazeang (g, w)

real	g = 316		{prompt="l/mm"}
real	w = 7500	{prompt="Blaze wavelength (A)"}
real	phi = 46.	{prompt="Camera-collimator angle (deg)"}
real	m = 1		{prompt="Order"}
real	n = 1.		{prompt="Index of refraction"}
real	prism = 22	{prompt="Prism angle (deg)"}

begin
	real	dtor, val

	dtor = 3.14159 / 180.

	if (n <= 1.) {
	    val = g * w * m  / cos (phi/2*dtor) / 2e7
	    val = atan2 (val, sqrt (1 - val**2)) / dtor
	} else {
#	    val = g * w * m  / 1e7 / (n - 1.)
#	    val = atan2 (val, sqrt (1 - val**2)) / dtor
	    val = g * w * m / 1e7 / sin (dtor * prism) + 1
	}
	printf ("%.4g\n", val)
end
