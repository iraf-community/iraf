include	<error.h>
include	<smw.h>

# Function types.
define  CHEBYSHEV       1       # CURFIT Chebyshev polynomial
define  LEGENDRE        2       # CURFIT Legendre polynomial
define  SPLINE3         3       # CURFIT cubic spline
define  SPLINE1         4       # CURFIT linear spline
define  PIXEL           5       # pixel coordinate array
define  SAMPLE          6       # sampled coordinates


# T_SSHIFT -- Shift the spectral coordinates
 
procedure t_sshift ()
 
int	list			# Input list of spectra
double	shift			# Shift to apply
pointer	aps			# Aperture list
bool	verbose			# Verbose?
 
int	ap, beam, dtype, nw
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	sp, image, coeff, tmp, im, mw

bool	clgetb()
double	clgetd()
int	imtopenp(), imtgetim()
pointer	rng_open(), immap(), smw_openim()
errchk	immap, smw_openim, smw_gwattrs, smw_swattrs, sshift
 
begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	coeff = NULL

	list = imtopenp ("spectra")
	shift = clgetd ("shift")
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	verbose = clgetb ("verbose")

	iferr (aps = rng_open (Memc[image], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture list")

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    im = NULL
	    mw = NULL
	    iferr {
	        tmp = immap (Memc[image], READ_WRITE, 0); im = tmp
		tmp = smw_openim (im); mw = tmp

		switch (SMW_FORMAT(mw)) {
		case SMW_ND:
		    call smw_gwattrs (mw, 1, 1, ap, beam, dtype,
			w1, dw, nw, z, aplow, aphigh, coeff)
		    w1 = w1 + shift
		    call smw_swattrs (mw, 1, 1, ap, beam, dtype,
			w1, dw, nw, z, aplow, aphigh, Memc[coeff])
		    if (verbose) {
			call printf ("%s: shift = %g, %g --> %g\n")
			    call pargstr (Memc[image])
			    call pargd (shift)
			    call pargd (w1 - shift)
			    call pargd (w1)
		    }
		case SMW_ES, SMW_MS:
		    call sshift (im, mw, Memc[image], aps, shift,
			verbose)
		}
	    } then
		call erract (EA_WARN)

	    if (mw != NULL) {
		call smw_saveim (mw, im)
		call smw_close (mw)
	    }
	    if (im != NULL)
		call imunmap (im)
	}

	call rng_close (aps)
	call imtclose (list)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# SSHIFT -- Shift coordinate zero point of selected aperture in
# MULTISPEC and EQUISPEC images.

procedure sshift (im, mw, image, aps, shift, verbose)

pointer	im			# IMIO pointer
pointer	mw			# MWCS pointer
char	image[ARB]		# Image name
pointer	aps			# Aperture range list
double	shift			# Shift to add
bool	verbose			# Verbose?

int	i, ap, beam, dtype, nw, naps
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	coeff, coeffs
bool	rng_elementi()
errchk	sshift1

begin
	coeff = NULL
	coeffs = NULL
 
	# Go through each spectrum and change the selected apertures.
	naps = 0
	do i = 1, SMW_NSPEC(mw) {
	    # Get aperture info
	    iferr (call smw_gwattrs (mw, i, 1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff))
		break

	    # Check if aperture is to be changed
	    if (!rng_elementi (aps, ap))
		next

	    # Apply shift
	    w1 = w1 + shift
	    if (dtype == 2)
		call sshift1 (shift, coeff)

	    call smw_swattrs (mw, i, 1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, Memc[coeff])

	    # Make record
	    if (verbose) {
		if (naps == 1) {
		    call printf ("%s: shift = %g\n")
			call pargstr (image)
			call pargd (shift)
		}
		call printf ("  Aperture %d:  %g --> %g\n")
		    call pargi (ap)
		    call pargd (w1 - shift)
		    call pargd (w1)
	    }
	}

	call mfree (coeff, TY_CHAR)
	call mfree (coeffs, TY_DOUBLE)
end


# SSHIFT1 -- Shift coordinate zero point of nonlinear functions.

procedure sshift1 (shift, coeff)

double	shift			# Shift to add
pointer	coeff			# Attribute function coefficients

int	i, j, ip, nalloc, ncoeff, type, order, fd
double	dval
pointer	coeffs
int	ctod(), stropen()
errchk	stropen

begin
	if (coeff == NULL)
	    return
	if (Memc[coeff] == EOS)
	    return

	coeffs = NULL
	ncoeff = 0
	ip = 1
	while (ctod (Memc[coeff], ip, dval) > 0) {
	    if (coeffs == NULL) {
		nalloc = 10
		call malloc (coeffs, nalloc, TY_DOUBLE)
	    } else if (ncoeff == nalloc) {
		nalloc = nalloc + 10
		call realloc (coeffs, nalloc, TY_DOUBLE)
	    }
	    Memd[coeffs+ncoeff] = dval
	    ncoeff = ncoeff + 1
	}
	ip = ip + SZ_LINE
	call realloc (coeff, ip, TY_CHAR)
	call aclrc (Memc[coeff], ip)
	fd = stropen (Memc[coeff], ip, NEW_FILE)

	ip = 0
	while (ip < ncoeff) {
	    if (ip > 0)
		call fprintf (fd, " ")
	    Memd[coeffs+ip+1] = Memd[coeffs+ip+1] + shift
	    type = nint (Memd[coeffs+ip+2])
	    order = nint (Memd[coeffs+ip+3])
	    call fprintf (fd, "%.3g %g %d %d")
		call pargd (Memd[coeffs+ip])
		call pargd (Memd[coeffs+ip+1])
		call pargi (type)
		call pargi (order)
	    switch (type) {
	    case CHEBYSHEV, LEGENDRE:
		j = 6 + order
	    case SPLINE3:
		j = 9 + order
	    case SPLINE1:
		j = 7 + order
	    case PIXEL:
		j = 4 + order
	    case SAMPLE:
		j = 5 + order
	    }
	    do i = 4, j-1 {
		call fprintf (fd, " %g")
		    call pargd (Memd[coeffs+ip+i])
	    }
	    ip = ip + j
	}
	call strclose (fd)

	call mfree (coeffs, TY_DOUBLE)
end
