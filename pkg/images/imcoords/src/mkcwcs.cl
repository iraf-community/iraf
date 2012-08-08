# MKCWCS -- Make celestial WCS.

procedure mkcwcs (wcsname)

file	wcsname			{prompt="WCS to create"}
file	wcsref = ""		{prompt="WCS reference\n"}

real	equinox = INDEF		{prompt="Equinox (years)"}
real	ra = INDEF		{prompt="RA (hours)"}
real	dec = INDEF		{prompt="DEC (degrees)"}
real	scale = INDEF		{prompt="Celestial pixel scale (arcsec/pix)"}
real	pa = 0.			{prompt="Position angle (deg)"}
bool	lefthanded = yes	{prompt="Left-handed system?"}
string	projection = "tan"	{prompt="Celestial projection\n",
				enum="linear|tan|sin"}

real	rapix = INDEF		{prompt="RA reference pixel"}
real	decpix = INDEF		{prompt="DEC reference pixel"}

begin
	int	wcsdim = 2
	real	c, s, lh
	file	name, ref, wcs

	# Determine the input and reference images.
	name = wcsname
	if (fscan (wcsref, ref) > 0)
	    wcscopy (name, ref)

	# Set the axes.
	if (imaccess (name)) {
	    hedit (name, "ctype1", "RA---TAN",
		add+, addonly-, verify-, show-, update+)
	    hedit (name, "ctype2", "DEC---TAN",
		add+, addonly-, verify-, show-, update+)
	}
	wcsedit (name, "axtype", "ra", "1", wcsdim=wcsdim,
	    wcs="world", interactive-, verbose-, update+)
	wcsedit (name, "axtype", "dec", "2", wcsdim=wcsdim,
	    wcs="world", interactive-, verbose-, update+)
	wcsedit (name, "wtype", projection, "1,2", wcsdim=wcsdim,
	    wcs="world", interactive-, verbose-, update+)

	# Set the celestial equinox if desired.  Note this is not WCS.
	if (equinox != INDEF)
	    hedit (name, "equinox", equinox,
	        add+, addonly-, verify-, show-, update+)

	# Set the reference point if desired.
	if (ra != INDEF)
	    wcsedit (name, "crval", ra*15, "1", wcsdim=wcsdim,
		wcs="world", interactive-, verbose-, update+)
	if (dec != INDEF)
	    wcsedit (name, "crval", dec, "2", wcsdim=wcsdim,
		wcs="world", interactive-, verbose-, update+)

	# Set the scales and celestial position angle.
	if (scale != INDEF) {
	    if (pa != INDEF) {
		c = cos (pa * 3.14159 / 180.) / 3600.
		s = sin (pa * 3.14159 / 180.) / 3600.
	    } else {
	        c = 1.
		s = 0.
	    }
	    if (lefthanded) {
		wcsedit (name, "cd", -scale*c, "1", "1", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
		wcsedit (name, "cd", -scale*s, "1", "2", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
		wcsedit (name, "cd", -scale*s, "2", "1", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
		wcsedit (name, "cd", scale*c, "2", "2", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
	    } else {
		wcsedit (name, "cd", scale*c, "1", "1", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
		wcsedit (name, "cd", -scale*s, "1", "2", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
		wcsedit (name, "cd", scale*s, "2", "1", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
		wcsedit (name, "cd", scale*c, "2", "2", wcsdim=wcsdim,
		    wcs="world", interactive-, verbose-, update+)
	    }
	}

	# Set reference pixel if desired.
	if (rapix != INDEF)
	    wcsedit (name, "crpix", rapix, "1", wcsdim=wcsdim,
		wcs="world", interactive-, verbose-, update+)
	if (decpix != INDEF)
	    wcsedit (name, "crpix", decpix, "2", wcsdim=wcsdim,
		wcs="world", interactive-, verbose-, update+)
end
