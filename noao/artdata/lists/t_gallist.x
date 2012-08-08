include <fset.h>
include "starlist.h"

procedure t_gallist()

pointer	galaxies		# pointer to the name of the output file
pointer	graphics		# poionter to graphics device name

int	sf, lf
long	seed, sseed, lseed
long	sseed1, lseed1
pointer	sp, str, x, y, mag, egal, axis, round, phi, dt, st, gd

bool	clgetb()
int	clgeti(), clgwrd(), open()
long	clgetl(), clktime()
pointer	dtmap(), gopen()
real	clgetr()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (galaxies, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate the starlist / galaxies structure.
	call malloc (st, LEN_STSTRUCT, TY_STRUCT)
	ST_TYPE(st) = ST_GALAXIES

	# Get the parameters.
	call clgstr ("gallist", Memc[galaxies], SZ_FNAME)
	ST_NSTARS(st) = clgeti ("ngals")

	# Get the parameters of the spatial density function.
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


	# Get the parameters of the luminosity function.
	ST_LUMINOSITY(st) = clgwrd ("luminosity", ST_LFSTRING(st), SZ_FNAME,
	    GLUMFUNCS)
	ST_MINMAG(st) = clgetr ("minmag")
	ST_MAXMAG(st) = clgetr ("maxmag")
	ST_LFILE(st) = EOS
	switch (ST_LUMINOSITY(st)) {
	case ST_UNIFORM, ST_POWLAW, ST_SCHECTER:
	    lf = NULL
	case ST_LFFILE:
	    call clgstr ("lfile", ST_LFILE(st), SZ_FNAME)
	    lf = open (ST_LFILE(st), READ_ONLY, TEXT_FILE)
	}
	ST_POWER(st) = clgetr ("power")
	ST_MZERO(st) = clgetr ("mzero")
	ST_ALPHA(st) = clgetr ("alpha")
	ST_MSTAR(st) = clgetr ("mstar")

	# Get the remaining parameters.
	ST_Z(st) = clgetr ("z")
	ST_AR(st) = clgetr ("ar")
	ST_ERADIUS(st) = clgetr ("eradius")
	ST_SRADIUS(st) = clgetr ("sradius")
	ST_EGALMIX(st) = clgetr ("egalmix")
	ST_ABSORPTION(st) = clgetr ("absorption")

	# Get the spatial density and luminosity function sampling parameters.
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
	ST_RBINSIZE(st) = clgetr ("rbinsize")
	ST_MBINSIZE(st) = clgetr ("mbinsize")
	ST_DBINSIZE(st) = clgetr ("dbinsize")
	ST_EBINSIZE(st) = clgetr ("ebinsize")
	ST_PBINSIZE(st) = clgetr ("pbinsize")

	x = NULL
	y = NULL
	mag = NULL
	egal = NULL
	axis = NULL
	round = NULL
	phi = NULL

	#  Compute the spatial and luminosity functions.
	call st_gmkspatial (sf, st, x, y, mag, egal, axis, round, phi)
	call st_gmklum (lf, st, x, y, mag, egal, axis, round, phi)
	call st_gmkmix (st, x, y, mag, egal, axis, round, phi)
	call st_gmkaxis (st, x, y, mag, egal, axis, round, phi)
	call st_gmkround (st, x, y, mag, egal, axis, round, phi)
	call st_gmkphi (st, x, y, mag, egal, axis, round, phi)

	# Plot the results.
	if (clgetb ("interactive")) {
	    call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	    if (Memc[graphics] != EOS) {
	        gd = gopen (Memc[graphics], NEW_FILE, STDGRAPH)
	        call st_gplots (sf, lf, gd, st, x, y, mag, egal, axis, round,
		    phi)
	        call gclose (gd)
	    }
	}

	# Write the database.
	dt = dtmap (Memc[galaxies], APPEND)
	call st_dtginit (dt, st, Memc[galaxies], sseed, lseed)
	call st_dtgwrite (dt, Memr[x], Memr[y], Memr[mag], Memi[egal],
	    Memr[axis], Memr[round], Memr[phi], ST_NSTARS(st))
	call dtunmap (dt)

	# Free up memory.
	if (x != NULL)
	    call mfree (x, TY_REAL)
	if (y != NULL)
	    call mfree (y, TY_REAL)
	if (mag != NULL)
	    call mfree (mag, TY_REAL)
	if (egal != NULL)
	    call mfree (egal, TY_INT)
	if (axis != NULL)
	    call mfree (axis, TY_REAL)
	if (round != NULL)
	    call mfree (round, TY_REAL)
	if (phi != NULL)
	    call mfree (phi, TY_REAL)
	call mfree (st, TY_STRUCT)

	# Close files.
	if (sf != NULL)
	    call close (sf)
	if (lf != NULL)
	    call close (lf)

	call sfree (sp)
end


# ST_GMKSPATIAL -- Compute the galactic spatial density function.

procedure st_gmkspatial (sf, st, x, y, mag, egal, axis, round, phi)

int	sf			# spatial density function file descriptor
pointer	st			# pointer to the starlist strucuture
pointer	x			# pointer to the x array
pointer	y			# pointer to the y array
pointer	mag			# pointer to the magnitude array
pointer	egal			# pointer to the galaxy type array
pointer	axis			# pointer to half-power diameter array
pointer	round			# pointer to roundness array
pointer	phi			# pointer to an array of position angles

int	nsf
pointer	r, rprob 
int	st_gfetchxy()

begin
	# Check the sizes of the arrays.
	call st_gmalloc (st, x, y, mag, egal, axis, round, phi)

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


# ST_GMKLUM -- Compute the luminosity function and the diameter distribution
# function.

procedure st_gmklum (lf, st, x, y, mag, egal, axis, round, phi)

int	lf		# luminsosity function file descriptor
pointer	st		# pointer to starlist structure
pointer	x		# pointer to the x array
pointer	y		# pointer to the y array
pointer	mag		# pointer to magnitude array
pointer	egal		# pointer to the galaxy type array
pointer	axis		# pointer to half-power diameter array
pointer	round		# pointer to roundness array
pointer	phi		# pointer to an array of position angles

int	nlf
pointer	m, mprob
int	st_gfetchxy()

begin
	# Check the sizes of the arrays.
	call st_gmalloc (st, x, y, mag, egal, axis, round, phi)

	# Compute the luminosity  function.
	switch (ST_LUMINOSITY(st)) {
	case ST_UNIFORM:
	    call st_maguniform (Memr[mag], ST_NSTARS(st), ST_MINMAG(st),
	        ST_MAXMAG(st), ST_LSEED(st))

	case ST_POWLAW:
	    call st_power (Memr[mag], ST_NSTARS(st), ST_POWER(st),
	        ST_MINMAG(st), ST_MAXMAG(st), ST_LSEED(st))

	case ST_SCHECTER:
	    call st_schecter (Memr[mag], ST_NSTARS(st), ST_ALPHA(st),
	        ST_MSTAR(st), ST_MINMAG(st), ST_MAXMAG(st), ST_MZERO(st),
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
	                ST_NSTARS(st), ST_MINMAG(st), ST_MAXMAG(st),
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


# ST_GMKMIX -- Compute the percentage of elliptical versus spiral galaxies.

procedure st_gmkmix (st, x, y, mag, egal, axis, round, phi)

pointer	st		# pointer to the starlist structure
pointer	x		# pointer to the x array
pointer	y		# pointer to the y array
pointer	mag		# pointer to magnitude array
pointer	egal		# pointer to the galaxy type array
pointer	axis		# pointer to half-power diameter array
pointer	round		# pointer to roundness array
pointer	phi		# pointer to an array of position angles

begin
	# Check the sizes of the arrays.
	call st_gmalloc (st, x, y, mag, egal, axis, round, phi)

	# Compute the elliptical / spiral galaxy mix.
	call st_esmix (Memi[egal], ST_NSTARS(st), ST_EGALMIX(st), ST_SSEED(st))
end


# ST_GMKROUND -- Compute the roundness values.

procedure st_gmkround (st, x, y, mag, egal, axis, round, phi)

pointer	st		# pointer to the starlist structure
pointer	x		# pointer to the x array
pointer	y		# pointer to the y array
pointer	mag		# pointer to magnitude array
pointer	egal		# pointer to galaxy type array
pointer	axis		# pointer to half-power diameter array
pointer	round		# pointer to roundness array
pointer	phi		# pointer to an array of position angles

begin
	# Check the sizes of the arrays.
	call st_gmalloc (st, x, y, mag, egal, axis, round, phi)

	# Compute the roundness values.
	call st_round (Memi[egal], Memr[mag], Memr[round], ST_NSTARS(st),
	    ST_AR(st), ST_ABSORPTION(st),  ST_SSEED(st))
end


# ST_GMKPHI -- Compute the position angles.

procedure st_gmkphi (st, x, y, mag, egal, axis, round, phi)

pointer	st		# pointer to the starlist structure
pointer	x		# pointer to the x array
pointer	y		# pointer to the y array
pointer	mag		# pointer to mag array
pointer	egal		# pointer to the  galaxy type array
pointer	axis		# pointer to half-power diameter array
pointer	round		# pointer to roundness array
pointer	phi		# pointer to an array of position angles

begin
	# Check the sizes of the arrays.
	call st_gmalloc (st, x, y, mag, egal, axis, round, phi)

	# Compute the position angles.
	call st_phi (Memr[phi], ST_NSTARS(st), ST_SSEED(st))
end


# ST_GMKAXIS -- Compute the semi-major axes.

procedure st_gmkaxis (st, x, y, mag, egal, axis, round, phi)

pointer	st		# pointer to the starlist structure
pointer	x		# pointer to the x array
pointer	y		# pointer to the y array
pointer	mag		# pointer to magnitude array
pointer	egal		# pointer to E or S galaxy array
pointer	axis		# pointer to half-power diameter array
pointer	round		# pointer to roundness array
pointer	phi		# pointer to an array of position angles

begin
	# Check the sizes of the arrays.
	call st_gmalloc (st, x, y, mag, egal, axis, round, phi)

	# Make the effective diameters array.
	switch (ST_LUMINOSITY(st)) {
	case ST_POWLAW:
	    call st_zdiameters (Memr[mag], Memi[egal], Memr[axis],
		ST_NSTARS(st), ST_MINMAG(st), ST_MAXMAG(st), ST_Z(st),
		ST_ERADIUS(st), ST_SRADIUS(st), ST_LSEED(st))
	default:
	    call st_diameters (Memr[mag], Memi[egal], Memr[axis],
		ST_NSTARS(st), ST_MINMAG(st), ST_MAXMAG(st),
		ST_ERADIUS(st), ST_SRADIUS(st), ST_LSEED(st))
	}
end


# ST_GMALLOC -- Allocate array space for the gallist task.

procedure st_gmalloc (st, x, y, mag, egal, axis, round, phi)

pointer	st		# pointer to the starlist structure
pointer	x		# pointer to the x array
pointer	y		# pointer to the y array
pointer	mag		# pointer to magnitude array
pointer	egal		# pointer to galaxy type array
pointer	axis		# pointer to half-power diameter array
pointer	round		# pointer to roundness array
pointer	phi		# pointer to an array of position angles

begin
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
	if (egal == NULL)
	    call malloc (egal, ST_NSTARS(st), TY_INT)
	else
	    call realloc (egal, ST_NSTARS(st), TY_INT)
	if (axis == NULL)
	    call malloc (axis, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (axis, ST_NSTARS(st), TY_REAL)
	if (round == NULL)
	    call malloc (round, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (round, ST_NSTARS(st), TY_REAL)
	if (phi == NULL)
	    call malloc (phi, ST_NSTARS(st), TY_REAL)
	else
	    call realloc (phi, ST_NSTARS(st), TY_REAL)
end
