# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISINIT --  Procedure to set up the surface  descriptor.

procedure isinit (sf, surf_type, xorder, yorder, xterms, ncols, nlines)

pointer	sf		# pointer to surface descriptor structure
int	surf_type	# type of surface to be fitted
int	xorder		# x order of surface to be fit, or in the case of the
			# spline the number of polynomial pieces in x to be fit
int	yorder		# y order of surface to be fit, or in the case of the
			# spline the number of polynomial pieces in y to be fit
int	xterms		# cross terms for polynomials?
size_t	ncols		# number of columns in the surface
size_t	nlines		# number of lines in the surface

size_t	sz_val
long	i
pointer	x, y
pointer	sp
errchk	malloc, calloc

begin
	# allocate space for the surface descriptor
	sz_val = LEN_SFSTRUCT
	call malloc (sf, sz_val, TY_STRUCT)

	if (xorder < 1 || yorder < 1)
	    call error (0, "SFLINIT: Illegal order.")

	if (ncols < 1)
	    call error (0, "SFLINIT: x data range is 0.")
	if (nlines < 1)
	    call error (0, "SFLINIT: y data range is 0.")

	switch (surf_type) {

	case SF_CHEBYSHEV, SF_LEGENDRE:
	    SF_NXCOEFF(sf) = xorder
	    SF_XORDER(sf) = xorder
	    SF_XRANGE(sf) = 2. / real (ncols + 1)
	    SF_XMAXMIN(sf) = - real (ncols + 1) / 2.
	    SF_XMIN(sf) = 0.
	    SF_XMAX(sf) = real (ncols + 1)
	    SF_NYCOEFF(sf) = yorder
	    SF_YORDER(sf) = yorder
	    SF_YRANGE(sf) = 2. / real (nlines + 1)
	    SF_YMAXMIN(sf) = - real (nlines + 1) / 2.
	    SF_YMIN(sf) = 0.
	    SF_YMAX(sf) = real (nlines + 1)
	    SF_XTERMS(sf) = xterms

	case SF_SPLINE3:
	    SF_NXCOEFF(sf) = (xorder + SPLINE3_ORDER - 1)
	    SF_XORDER(sf) = SPLINE3_ORDER
	    SF_NXPIECES(sf) = xorder - 1
	    SF_XSPACING(sf) = xorder / real (ncols + 1)
	    SF_NYCOEFF(sf) = (yorder + SPLINE3_ORDER - 1)
	    SF_YORDER(sf) = SPLINE3_ORDER
	    SF_NYPIECES(sf) = yorder - 1
	    SF_YSPACING(sf) = yorder / real (nlines + 1)
	    SF_XMIN(sf) = 0.
	    SF_XMAX(sf) = real (ncols + 1)
	    SF_YMIN(sf) = 0.
	    SF_YMAX(sf) = real (nlines + 1)
	    SF_XTERMS(sf) = YES

	case SF_SPLINE1:
	    SF_NXCOEFF(sf) = (xorder + SPLINE1_ORDER - 1)
	    SF_XORDER(sf) = SPLINE1_ORDER
	    SF_NXPIECES(sf) = xorder - 1
	    SF_XSPACING(sf) = xorder / real (ncols + 1)
	    SF_NYCOEFF(sf) = (yorder + SPLINE1_ORDER - 1)
	    SF_YORDER(sf) = SPLINE1_ORDER
	    SF_NYPIECES(sf) = yorder - 1
	    SF_YSPACING(sf) = yorder / real (nlines + 1)
	    SF_XMIN(sf) = 0.
	    SF_XMAX(sf) = real (ncols + 1)
	    SF_YMIN(sf) = 0.
	    SF_YMAX(sf) = real (nlines + 1)
	    SF_XTERMS(sf) = YES

	default:
	    call error (0, "SFINIT: Unknown surface type.")
	}

	SF_TYPE(sf) = surf_type
	SF_NLINES(sf) = nlines
	SF_NCOLS(sf) = ncols

	# allocate space for the matrix and vectors
	sz_val = SF_XORDER(sf) * SF_NCOLS(sf)
	call calloc (SF_XBASIS(sf), sz_val, MEM_TYPE)
	sz_val = SF_YORDER(sf) * SF_NLINES(sf)
	call calloc (SF_YBASIS(sf), sz_val, MEM_TYPE)
	sz_val = SF_XORDER(sf) * SF_NXCOEFF(sf)
	call calloc (SF_XMATRIX(sf), sz_val, MEM_TYPE)
	sz_val = SF_NLINES(sf) * SF_NXCOEFF(sf)
	call calloc (SF_XCOEFF(sf), sz_val, MEM_TYPE)
	sz_val = SF_YORDER(sf) * SF_NYCOEFF(sf)
	call calloc (SF_YMATRIX(sf), sz_val, MEM_TYPE)
	sz_val = SF_NXCOEFF(sf) * SF_NYCOEFF(sf)
	call calloc (SF_COEFF(sf), sz_val, MEM_TYPE)

	# allocate temporary space
	call smark (sp)
	sz_val = SF_NCOLS(sf)
	call salloc (x, sz_val, MEM_TYPE)
	sz_val = SF_NLINES(sf)
	call salloc (y, sz_val, MEM_TYPE)

	# calculate all possible x basis functions and store
	do i = 1, SF_NCOLS(sf)
	    Memr[x+i-1] = i

	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE:
	    SF_XLEFT(sf) = NULL
	    call sf_bleg (Memr[x], SF_NCOLS(sf), SF_XORDER(sf), SF_XMAXMIN(sf),
		SF_XRANGE(sf), XBASIS(SF_XBASIS(sf)))

	case SF_CHEBYSHEV:
	    SF_XLEFT(sf) = NULL
	    call sf_bcheb (Memr[x], SF_NCOLS(sf), SF_XORDER(sf), SF_XMAXMIN(sf),
		SF_XRANGE(sf), XBASIS(SF_XBASIS(sf)))

	case SF_SPLINE3:
	    sz_val = SF_NCOLS(sf)
	    call calloc (SF_XLEFT(sf), sz_val, TY_POINTER)
	    call sf_bspline3 (Memr[x], SF_NCOLS(sf), SF_NXPIECES(sf),
	        -SF_XMIN(sf), SF_XSPACING(sf), XBASIS(SF_XBASIS(sf)),
	        XLEFT(SF_XLEFT(sf)))

	case SF_SPLINE1:
	    sz_val = SF_NCOLS(sf)
	    call calloc (SF_XLEFT(sf), sz_val, TY_POINTER)
	    call sf_bspline1 (Memr[x], SF_NCOLS(sf), SF_NXPIECES(sf),
	        -SF_XMIN(sf), SF_XSPACING(sf), XBASIS(SF_XBASIS(sf)),
	        XLEFT(SF_XLEFT(sf)))
	}

	# calculate all possible y basis functions and store
	do i = 1, SF_NLINES(sf)
	    Memr[y+i-1] = i

	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE:
	    SF_YLEFT(sf) = NULL
	    call sf_bleg (Memr[y], SF_NLINES(sf), SF_YORDER(sf),
	        SF_YMAXMIN(sf), SF_YRANGE(sf), YBASIS(SF_YBASIS(sf)))

	case SF_CHEBYSHEV:
	    SF_YLEFT(sf) = NULL
	    call sf_bcheb (Memr[y], SF_NLINES(sf), SF_YORDER(sf),
	        SF_YMAXMIN(sf), SF_YRANGE(sf), YBASIS(SF_YBASIS(sf)))

	case SF_SPLINE3:
	    sz_val = SF_NLINES(sf)
	    call calloc (SF_YLEFT(sf), sz_val, TY_POINTER)
	    call sf_bspline3 (Memr[y], SF_NLINES(sf), SF_NYPIECES(sf),
	        -SF_YMIN(sf), SF_YSPACING(sf), YBASIS(SF_YBASIS(sf)),
	        YLEFT(SF_YLEFT(sf)))

	case SF_SPLINE1:
	    sz_val = SF_NLINES(sf)
	    call calloc (SF_YLEFT(sf), sz_val, TY_POINTER)
	    call sf_bspline1 (Memr[y], SF_NLINES(sf), SF_NYPIECES(sf),
	        -SF_YMIN(sf), SF_YSPACING(sf), YBASIS(SF_YBASIS(sf)),
	        YLEFT(SF_YLEFT(sf)))
	}

	SF_WZ(sf) = NULL
	SF_TLEFT(sf) = NULL

	call sfree (sp)
end
