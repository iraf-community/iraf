include "starlist.h"

procedure st_show (st)

pointer st		# pointer to starlist structure

begin
	call printf ("nstars = %d\n")
	    call pargi (ST_NSTARS(st))

	call printf ("spatial: %s\n")
	    call pargstr (ST_SPSTRING(st))
	call printf ("    xmin = %g  xmax = %g\n")
	    call pargr (ST_XMIN(st))
	    call pargr (ST_XMAX(st))
	call printf ("    ymin = %g  ymax = %g\n")
	    call pargr (ST_YMIN(st))
	    call pargr (ST_YMAX(st))
	call printf ("    xc = %g  yc = %g\n")
	    call pargr (ST_XC(st))
	    call pargr (ST_YC(st))
	if (ST_SPATIAL(st) == ST_HUBBLE) {
	    call printf ("    core = %g  base = %g\n")
	        call pargr (ST_CORE(st))
	        call pargr (ST_BASE(st))
	} else if (ST_SPATIAL(st) == ST_SPFILE) {
	    call printf ("    file: %s\n")
	        call pargstr (ST_SFILE(st))
	}
	call printf ("    nssample = %d  sorder = %d\n")
	    call pargi (ST_NSSAMPLE(st))
	    call pargi (ST_SORDER(st))
	call printf ("    rbinsize = %d\n")
	    call pargr (ST_RBINSIZE(st))

	call printf ("luminosity = %s\n")
	    call pargstr (ST_LFSTRING(st))
	call printf ("    minmag = %g  maxmag = %g\n")
	    call pargr (ST_MINMAG(st))
	    call pargr (ST_MAXMAG(st))
	if (ST_LUMINOSITY(st) == ST_BANDS) {
	    call printf ( "    bands: alpha = %g  beta = %g\n") 
	        call pargr (ST_ALPHA(st))
	        call pargr (ST_BETA(st))
	    call printf ("    delta = %g  mstar = %g  mzero = %g\n")
	        call pargr (ST_DELTA(st))
	        call pargr (ST_MSTAR(st))
		call pargr (ST_MZERO(st))
	} else if (ST_LUMINOSITY(st) == ST_SALPETER) {
	    call printf ("    mzero = %g\n")
		call pargr (ST_MZERO(st))
	} else if (ST_LUMINOSITY(st) == ST_POWLAW) {
	    call printf ("    power = %g\n")
		call pargr (ST_POWER(st))
	} else if (ST_LUMINOSITY(st) == ST_LFFILE) {
	    call printf ("    file: %s\n")
	        call pargstr (ST_LFILE(st))
	}
	call printf ( "    nlsample = %d  lorder = %d\n")
	    call pargl (ST_NLSAMPLE(st))
	    call pargi (ST_LORDER(st))
	call printf ("    mbinsize = %g\n")
	    call pargr (ST_MBINSIZE(st))
end


# ST_GSHOW -- Display the GALAXIES parameters.

procedure st_gshow (st)

pointer st		# pointer to starfield structure

begin
	call printf ("ngals = %d\n")
	    call pargi (ST_NSTARS(st))

	call printf ("spatial: %s\n")
	    call pargstr (ST_SPSTRING(st))
	call printf ("    xmin = %g  xmax = %g\n")
	    call pargr (ST_XMIN(st))
	    call pargr (ST_XMAX(st))
	call printf ("    ymin = %g  ymax = %g\n")
	    call pargr (ST_YMIN(st))
	    call pargr (ST_YMAX(st))
	call printf ("    xc = %g  yc = %g\n")
	    call pargr (ST_XC(st))
	    call pargr (ST_YC(st))
	if (ST_SPATIAL(st) == ST_HUBBLE) {
	    call printf ("    core = %g  base = %g\n")
	        call pargr (ST_CORE(st))
	        call pargr (ST_BASE(st))
	} else if (ST_SPATIAL(st) == ST_SPFILE) {
	    call printf ("    file: %s\n")
	        call pargstr (ST_SFILE(st))
	}
	call printf ("    nssample = %d  sorder = %d\n")
	    call pargi (ST_NSSAMPLE(st))
	    call pargi (ST_SORDER(st))
	call printf ("    rbinsize = %d\n")
	    call pargr (ST_RBINSIZE(st))

	call printf ("luminosity = %s\n")
	    call pargstr (ST_LFSTRING(st))
	call printf ("    minmag = %g  maxmag = %g\n")
	    call pargr (ST_MINMAG(st))
	    call pargr (ST_MAXMAG(st))
	if (ST_LUMINOSITY(st) == ST_POWLAW) {
	    call printf ( "    powlaw: power = %g\n") 
	        call pargr (ST_POWER(st))
	} else if (ST_LUMINOSITY(st) == ST_SCHECTER) {
	    call printf ( "    schecter: alpha = %g  mstar = %g  mzero = %g\n") 
	        call pargr (ST_ALPHA(st))
	        call pargr (ST_MSTAR(st))
		call pargr (ST_MZERO(st))
	} else if (ST_LUMINOSITY(st) == ST_LFFILE) {
	    call printf ("    file: %s\n")
	        call pargstr (ST_LFILE(st))
	}
	call printf ("    nlsample = %d  lorder = %d\n")
	    call pargi (ST_NLSAMPLE(st))
	    call pargi (ST_LORDER(st))
	call printf ("    mbinsize = %d\n")
	    call pargr (ST_MBINSIZE(st))
	call printf ("    eradius = %g  sradius = %g  dbinsize = %g\n")
	    call pargr (ST_ERADIUS(st))
	    call pargr (ST_SRADIUS(st))
	    call pargr (ST_DBINSIZE(st))
	call printf ("    z = %g\n")
	    call pargr (ST_Z(st))
	call printf ("    absorption = %g\n")
	    call pargr (ST_ABSORPTION(st))

	call eprintf ("egalmix = %g\n")
	    call pargr (ST_EGALMIX(st))
	call printf ("ar = %g\n")
	    call pargr (ST_AR(st))
	call printf ("    ebinsize = %g\n")
	    call pargr (ST_EBINSIZE(st))
	call printf ("posmin = 0.0  posmax = 360.0\n")
	call printf ("    pbinsize = %g\n")
	    call pargr (ST_PBINSIZE(st))
end
