include <fset.h>
include "starlist.h"

procedure t_starlist ()

pointer	starlist		# pointer to the name of the output file
pointer	graphics		# pointer to the graphics device name

int	sf, lf
long	seed, sseed, lseed
long	sseed1, lseed1
pointer	sp, str, x, y, mag, dt, st, gd

bool	clgetb()
int	clgeti(), clgwrd(), open()
long	clgetl(), clktime()
pointer	dtmap(), gopen()
real	clgetr()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (starlist, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate the starlist structure.
	call malloc (st, LEN_STSTRUCT, TY_STRUCT)
	ST_TYPE(st) = ST_STARS

	# Get the principal parameters.
	call clgstr ("starlist", Memc[starlist], SZ_FNAME)
	ST_NSTARS(st) = clgeti ("nstars")

	# Get the charactersitics of the spatial density function.
	ST_SPATIAL(st) = clgwrd ("spatial", ST_SPSTRING(st), SZ_FNAME, SPFUNCS)
	ST_XMIN(st) = clgetr ("xmin")
	ST_XMAX(st) = clgetr ("xmax")
	ST_YMIN(st) = clgetr ("ymin")
	ST_YMAX(st) = clgetr ("ymax")
	ST_SFILE(st) = EOS
	switch (ST_SPATIAL(st)) {
	case ST_UNIFORM:
	    sf = NULL
	    ST_XC(st) = (ST_XMAX(st) + ST_XMIN(st)) / 2.0
	    ST_YC(st) = (ST_YMAX(st) + ST_YMIN(st)) / 2.0
	case ST_HUBBLE:
	    sf = NULL
	    ST_XC(st) = clgetr ("xcenter")
	    if (IS_INDEFR(ST_XC(st)))
	        ST_XC(st) = (ST_XMAX(st) + ST_XMIN(st)) / 2.0
	    ST_YC(st) = clgetr ("ycenter")
	    if (IS_INDEFR(ST_YC(st)))
	        ST_YC(st) = (ST_YMAX(st) + ST_YMIN(st)) / 2.0
	case ST_SPFILE:
	    call clgstr ("sfile", ST_SFILE(st), SZ_FNAME)
	    sf = open (ST_SFILE(st), READ_ONLY, TEXT_FILE)
	    ST_XC(st) = clgetr ("xcenter")
	    if (IS_INDEFR(ST_XC(st)))
	        ST_XC(st) = (ST_XMAX(st) + ST_XMIN(st)) / 2.0
	    ST_YC(st) = clgetr ("ycenter")
	    if (IS_INDEFR(ST_YC(st)))
	        ST_YC(st) = (ST_YMAX(st) + ST_YMIN(st)) / 2.0
	}
	ST_CORE(st) = clgetr ("core_radius")
	ST_BASE(st) = clgetr ("base")


	# Get the luminosity function parameters.
	ST_LUMINOSITY(st) = clgwrd ("luminosity", ST_LFSTRING(st), SZ_FNAME,
	    LUMFUNCS)
	ST_MINMAG(st) = clgetr ("minmag")
	ST_MAXMAG(st) = clgetr ("maxmag")
	ST_LFILE(st) = EOS
	switch (ST_LUMINOSITY(st)) {
	case ST_UNIFORM, ST_SALPETER, ST_BANDS, ST_POWLAW:
	    lf = NULL
	case ST_LFFILE:
	    call clgstr ("lfile", ST_LFILE(st), SZ_FNAME)
	    lf = open (ST_LFILE(st), READ_ONLY, TEXT_FILE)
	}
	ST_POWER(st) = clgetr ("power")
	ST_MZERO(st) = clgetr ("mzero")
	ST_ALPHA(st) = clgetr ("alpha")
	ST_BETA(st) = clgetr ("beta")
	ST_DELTA(st) = clgetr ("delta")
	ST_MSTAR(st) = clgetr ("mstar")

	# Get the function sampling parameters.
	seed = clktime (long (0))
	sseed1 = clgetl ("sseed")
	if (IS_INDEFL(sseed1))
	    sseed = sseed + seed
	else
	    sseed = sseed1
	ST_SSEED(st) = sseed
	lseed1 = clgetl ("lseed")
	if (IS_INDEFL(lseed1))
	    lseed = lseed + seed + 1
	else
	    lseed = lseed1
	ST_LSEED(st) = lseed
	ST_NSSAMPLE(st) = clgeti ("nssample")
	ST_NLSAMPLE(st) = clgeti ("nlsample")
	ST_SORDER(st) = clgeti ("sorder")
	ST_LORDER(st) = clgeti ("lorder")


	#  Compute the initial spatial and luminosity functions.
	x = NULL
	y = NULL
	mag = NULL
	call st_mkspatial (sf, st, x, y, mag)
	call st_mklum (lf, st, x, y, mag)

	# Plot the results.
	if (clgetb ("interactive")) {
	    call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	    ST_RBINSIZE(st) = clgetr ("rbinsize")
	    ST_MBINSIZE(st) = clgetr ("mbinsize")
	    if (Memc[graphics] != EOS) {
	        gd = gopen (Memc[graphics], NEW_FILE, STDGRAPH)
	        call st_plots (sf, lf, gd, st, x, y, mag)
	        call gclose (gd)
	    }
	}

	# Write the database.
	dt = dtmap (Memc[starlist], APPEND)
	call st_dtinit (dt, st, Memc[starlist], sseed, lseed)
	call st_dtwrite (dt, Memr[x], Memr[y], Memr[mag], ST_NSTARS(st))
	call dtunmap (dt)

	# Free up memory.
	if (x != NULL)
	    call mfree (x, TY_REAL)
	if (y != NULL)
	    call mfree (y, TY_REAL)
	if (mag != NULL)
	    call mfree (mag, TY_REAL)
	call mfree (st, TY_STRUCT)

	# Close files.
	if (sf != NULL)
	    call close (sf)
	if (lf != NULL)
	    call close (lf)

	call sfree (sp)
end


# ST_MKSPATIAL -- Compute the spatial density function.

procedure st_mkspatial (sf, st, x, y, mag)

int	sf			# spatial function file descriptor
pointer	st			# pointer to the starlist structure
pointer	x			# pointer to the x coordinate array
pointer	y			# pointer to the y coordinate array
pointer	mag			# pointer to the magnitude array

int	nsf
pointer	r, rprob 
int	st_gfetchxy()

begin
	# Dynamically reallocate the x, y and magnitude arrays.
	if (x == NULL)
	    call malloc (x, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (x, ST_NSTARS(st), TY_REAL)
	if (y == NULL)
	    call malloc (y, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (y, ST_NSTARS(st), TY_REAL)
	if (mag == NULL)
	    call malloc (mag, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (mag, ST_NSTARS(st), TY_REAL)

	# Compute the x and y values.
	switch (ST_SPATIAL(st)) {
	case ST_UNIFORM:
	    call st_xyuniform (Memr[x], Memr[y], ST_NSTARS(st), ST_XMIN(st),
	        ST_XMAX(st), ST_YMIN(st), ST_YMAX(st), ST_SSEED(st))

	case ST_HUBBLE:
	    call st_hbsample (Memr[x], Memr[y], ST_NSTARS(st), ST_CORE(st),
	        ST_BASE(st), ST_XC(st), ST_YC(st), ST_XMIN(st), ST_XMAX(st),
		ST_YMIN(st), ST_YMAX(st), ST_NSSAMPLE(st), ST_SORDER(st),
		ST_SSEED(st)) 

	case ST_SPFILE:
	    if (sf == NULL) {
		call printf ("The spatial density file is not open.\n")
		call amovkr ((ST_XMIN(st) + ST_XMAX(st)) / 2.0, Memr[x],
		    ST_NSTARS(st))
		call amovkr ((ST_YMIN(st) + ST_YMAX(st)) / 2.0, Memr[y],
		    ST_NSTARS(st))
	    } else {
	        nsf = st_gfetchxy (sf, r, rprob)
	        if (nsf > 0) {
	            call st_sfsample (Memr[r], Memr[rprob], nsf, Memr[x],
		        Memr[y], ST_NSTARS(st), ST_NSSAMPLE(st), ST_SORDER(st),
			ST_XC(st), ST_YC(st), ST_XMIN(st), ST_XMAX(st),
			ST_YMIN(st), ST_YMAX(st), ST_SSEED(st))
	        } else {
		    call printf (
		        "The spatial density function file is empty.\n")
		    call amovkr ((ST_XMIN(st) + ST_XMAX(st)) / 2.0, Memr[x],
		        ST_NSTARS(st))
		    call amovkr ((ST_YMIN(st) + ST_YMAX(st)) / 2.0, Memr[y],
		        ST_NSTARS(st))
	        }
	        call mfree (r, TY_REAL)
	        call mfree (rprob, TY_REAL)
	    }

	default:
	    call printf ("Unknown spatial density function.\n")
	}
end


# ST_MKLUM -- Compute the luminosity function.

procedure st_mklum (lf, st, x, y, mag)

int	lf		# luminsosity function file descriptor
pointer	st		# pointer to starlist structure
pointer	x		# pointer to the x coordinate array
pointer	y		# pointer to the y coordinate array
pointer	mag		# pointer to the magnitude array

int	nlf
pointer	m, mprob
int	st_gfetchxy()

begin
	# Dynamically reallocate the array space.
	if (x == NULL)
	    call malloc (x, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (x, ST_NSTARS(st), TY_REAL)
	if (y == NULL)
	    call malloc (y, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (y, ST_NSTARS(st), TY_REAL)
	if (mag == NULL)
	    call malloc (mag, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (mag, ST_NSTARS(st), TY_REAL)

	# Compute the magnitudes.
	switch (ST_LUMINOSITY(st)) {
	case ST_UNIFORM:
	    call st_maguniform (Memr[mag], ST_NSTARS(st), ST_MINMAG(st),
	        ST_MAXMAG(st), ST_LSEED(st))

	case ST_POWLAW:
	    call st_power (Memr[mag], ST_NSTARS(st), ST_POWER(st),
	        ST_MINMAG(st), ST_MAXMAG(st), ST_LSEED(st))

	case ST_SALPETER:
	    call st_salpeter (Memr[mag], ST_NSTARS(st),
	        ST_MINMAG(st), ST_MAXMAG(st), ST_MZERO(st),
		ST_NLSAMPLE(st), ST_LORDER(st), ST_LSEED(st))

	case ST_BANDS:
	    call st_bands (Memr[mag], ST_NSTARS(st), ST_ALPHA(st),
	        ST_BETA(st), ST_DELTA(st), ST_MSTAR(st),
	        ST_MINMAG(st), ST_MAXMAG(st), ST_MZERO(st),
		ST_NLSAMPLE(st), ST_LORDER(st), ST_LSEED(st))

	case ST_LFFILE:
	    if (lf == NULL) {
		call printf ("The luminosity function file is not open.\n")
		call amovkr ((ST_MINMAG(st) + ST_MAXMAG(st)) / 2.0, Memr[mag],
		    ST_NSTARS(st))
	    } else {
	        nlf = st_gfetchxy (lf, m, mprob)
	        if (nlf > 0) {
	            call st_lfsample (Memr[m], Memr[mprob], nlf, Memr[mag],
	                ST_NSTARS(st),
			ST_MINMAG(st), ST_MAXMAG(st),
		        ST_NLSAMPLE(st), ST_LORDER(st), ST_LSEED(st))
	        } else {
		    call printf (
		    "The luminosity function file is empty.\n")
		    call amovkr ((ST_MINMAG(st) + ST_MAXMAG(st)) / 2.0,
		        Memr[mag], ST_NSTARS(st))
	        }
	        call mfree (m, TY_REAL)
	        call mfree (mprob, TY_REAL)
	    }

	default:
	    call printf ("The luminosity function is unknown.\n")
	}
end
