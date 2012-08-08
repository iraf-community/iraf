# SEP -- Separation between two celestial coordinates.

procedure sep (ra1, dec1, ra2, dec2)

real	ra1			{ prompt="RA (hr|deg)"}
real	dec1			{ prompt="DEC (deg)"}
real	ra2			{ prompt="RA (hr|deg)"}
real	dec2			{ prompt="DEC (deg)"}
string	raunit = "hr"		{ prompt="RA unit (hr|deg)", enum="hr|deg" }
bool	verbose = no		{ prompt="Verbose?"}
real	sep			{ prompt="Separation (arcsec)"}

begin
	real	r1, d1, r2, d2
	real	c1, c2, x, y, z

	if (raunit == "hr") {
	    r1 = ra1 * 15.
	    d1 = dec1
	    r2 = ra2 * 15.
	    d2 = dec2
	} else {
	    r1 = ra1
	    d1 = dec1
	    r2 = ra2
	    d2 = dec2
	}

	c1 = dcos(d1)
	c2 = dcos(d2)
	x = dcos(r1) * c1 - dcos(r2) * c2
	y = dsin(r1) * c1 - dsin(r2) * c2
	z = dsin(d1) - dsin(d2)
	c1 = (x*x + y*y + z*z) / 4.
	c2 = max (0., 1.-c1)
	sep = 2 * datan2(sqrt(c1),sqrt(c2)) * 3600

	if (verbose)
	    printf ("%.2f arcsec = (%.2H, %.1h) - (%.2H, %.1h)\n",
		sep, r1, d1, r2, d2)
end
