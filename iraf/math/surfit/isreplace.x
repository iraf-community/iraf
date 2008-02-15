# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISREPLACE -- Procedure to restore the surface fit stored by SIFSAVE
# to the surface descriptor for use by the evaluating routines. The
# surface parameters, surface type, xorder (or number of polynomial
# pieces in x), yorder (or number of polynomial pieces in y), xterms,
# number of columns and number of lines, are stored in the first
# six elements of the real array fit, followed by the SF_NYCOEFF(sf) *
# SF_NYCOEFF(sf) surface coefficients. The coefficient of B(i,x) * B(j,y)
# is stored in element number 6 + (i - 1) * SF_NYCOEFF(sf) + j of the
# array fit.

procedure isreplace (sf, fit)

pointer	sf		# surface descriptor
real	fit[ARB]	# array containing the surface parameters and
			# coefficients

int	surface_type, xorder, yorder, ncols, nlines

begin
	# allocate space for the surface descriptor
	call calloc (sf, LEN_SFSTRUCT, TY_STRUCT)

	xorder = nint (SF_SAVEXORDER(fit))
	if (xorder < 1)
	    call error (0, "SFRESTORE: Illegal x order.")
	yorder = nint (SF_SAVEYORDER(fit))
	if (yorder < 1)
	    call error (0, "SFRESTORE: Illegal y order.")

	ncols = nint (SF_SAVENCOLS(fit))
	if (ncols < 1)
	    call error (0, "SFRESTORE: Illegal x range.")
	nlines = nint (SF_SAVENLINES(fit))
	if (nlines < 1)
	    call error (0, "SFRESTORE: Illegal y range.")

	# set surface type dependent surface descriptor parameters
	surface_type = nint (SF_SAVETYPE(fit))

	switch (surface_type) {
	case SF_LEGENDRE, SF_CHEBYSHEV:
	    SF_NXCOEFF(sf) = xorder
	    SF_XORDER(sf) = xorder
	    SF_XRANGE(sf) = 2. / real (ncols + 1)
	    SF_XMAXMIN(sf) =  - real (ncols + 1) / 2.
	    SF_XMIN(sf) = 0.
	    SF_XMAX(sf) = real (ncols + 1)
	    SF_NYCOEFF(sf) = yorder
	    SF_YORDER(sf) = yorder
	    SF_YRANGE(sf) = 2. / real (nlines + 1)
	    SF_YMAXMIN(sf) =  - real (nlines + 1) / 2.
	    SF_YMIN(sf) = 0.
	    SF_YMAX(sf) = real (nlines + 1)
	    SF_XTERMS(sf) = SF_SAVEXTERMS(fit)

	case SF_SPLINE3:
	    SF_NXCOEFF(sf) = (xorder + SPLINE3_ORDER - 1)
	    SF_XORDER(sf) = SPLINE3_ORDER
	    SF_NXPIECES(sf) = xorder - 1
	    SF_XSPACING(sf) = xorder / real (ncols + 1)
	    SF_XMIN(sf) = 0.
	    SF_XMAX(sf) = real (ncols + 1)
	    SF_NYCOEFF(sf) = (yorder + SPLINE3_ORDER - 1)
	    SF_YORDER(sf) = SPLINE3_ORDER
	    SF_NYPIECES(sf) = yorder - 1
	    SF_YSPACING(sf) = yorder / real (nlines + 1)
	    SF_YMIN(sf) = 0.
	    SF_YMAX(sf) = real (nlines + 1)
	    SF_XTERMS(sf) = YES

	case SF_SPLINE1:
	    SF_NXCOEFF(sf) = (xorder + SPLINE1_ORDER - 1)
	    SF_XORDER(sf) = SPLINE1_ORDER
	    SF_NXPIECES(sf) = xorder - 1
	    SF_XSPACING(sf) = xorder / real (ncols + 1)
	    SF_XMIN(sf) = 0.
	    SF_XMAX(sf) = real (ncols + 1)
	    SF_NYCOEFF(sf) = (yorder + SPLINE1_ORDER - 1)
	    SF_YORDER(sf) = SPLINE1_ORDER
	    SF_NYPIECES(sf) = yorder - 1
	    SF_YSPACING(sf) = yorder / real (nlines + 1)
	    SF_YMIN(sf) = 0.
	    SF_YMAX(sf) = real (nlines + 1)
	    SF_XTERMS(sf) = YES

	default:
	    call error (0, "SFRESTORE: Unknown surface type.")
	}

	# set remaining curve parameters
	SF_TYPE(sf) = surface_type
	SF_NLINES(sf) = nlines
	SF_NCOLS(sf) = ncols

	# allocate space for the coefficient array
	SF_XBASIS(sf) = NULL
	SF_YBASIS(sf) = NULL
	SF_XMATRIX(sf) = NULL
	SF_YMATRIX(sf) = NULL
	SF_XCOEFF(sf) = NULL
	SF_WZ(sf) = NULL
	SF_TLEFT(sf) = NULL

	call calloc (SF_COEFF(sf), SF_NXCOEFF(sf) * SF_NYCOEFF(sf), MEM_TYPE)

	# restore coefficient array
	call amovr (fit[SF_SAVECOEFF+1], COEFF(SF_COEFF(sf)), SF_NYCOEFF(sf) *
	    SF_NXCOEFF(sf))
end
