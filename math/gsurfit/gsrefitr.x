# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "gsurfitdef.h"

# GSREFIT -- Procedure to refit the surface assuming that the x, y and w
# values and the matrices MATRIX and CHOFAC have remained unchanged. It
# is necessary only to accumulate a new VECTOR. The new coefficients
# are calculated by forward and back subsitution and stored in COEFF.

procedure gsrefit (sf, x, y, z, w, ier)

pointer	sf		# surface descriptor
real	x[ARB]		# array of x values
real	y[ARB]		# array of y values
real	z[ARB]		# data array
real	w[ARB]		# array of weights
int	ier		# ier = OK, everything OK
			# ier = SINGULAR, matrix is singular, 1 or more
			# coefficients are 0.
			# ier = NO_DEG_FREEDOM, too few points to solve matrix

int	k, l
int	xorder, nfree, maxorder
pointer	sp, vzptr, vindex, bxptr, byptr, bwz

real	adotr()

errchk	smark, salloc, sfree

begin
	# clear accumulator
	call aclrr (VECTOR(GS_VECTOR(sf)), GS_NCOEFF(sf))

	# if first call to gsefit calculate basis functions
	if (GS_XBASIS(sf) == NULL || GS_YBASIS(sf) == NULL) {

	    call malloc (GS_WZ(sf), GS_NPTS(sf), TY_REAL)

	    switch (GS_TYPE(sf)) {
	    case GS_LEGENDRE:
	        call malloc (GS_XBASIS(sf), GS_NPTS(sf) * GS_XORDER(sf),
		    TY_REAL)
	        call malloc (GS_YBASIS(sf), GS_NPTS(sf) * GS_YORDER(sf),
		    TY_REAL)
	        call rgs_bleg (x, GS_NPTS(sf), GS_XORDER(sf), GS_XMAXMIN(sf),
	    		  GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
	        call rgs_bleg (y, GS_NPTS(sf), GS_YORDER(sf), GS_YMAXMIN(sf),
	    		  GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    case GS_CHEBYSHEV:
	        call malloc (GS_XBASIS(sf), GS_NPTS(sf) * GS_XORDER(sf),
		    TY_REAL)
	        call malloc (GS_YBASIS(sf), GS_NPTS(sf) * GS_YORDER(sf),
		    TY_REAL)
	        call rgs_bcheb (x, GS_NPTS(sf), GS_XORDER(sf), GS_XMAXMIN(sf),
	    		  GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
	        call rgs_bcheb (y, GS_NPTS(sf), GS_YORDER(sf), GS_YMAXMIN(sf),
	    		  GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    case GS_POLYNOMIAL:
	        call malloc (GS_XBASIS(sf), GS_NPTS(sf) * GS_XORDER(sf),
		    TY_REAL)
	        call malloc (GS_YBASIS(sf), GS_NPTS(sf) * GS_YORDER(sf),
		    TY_REAL)
	        call rgs_bpol (x, GS_NPTS(sf), GS_XORDER(sf), GS_XMAXMIN(sf),
	    		  GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
	        call rgs_bpol (y, GS_NPTS(sf), GS_YORDER(sf), GS_YMAXMIN(sf),
	    		  GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    default:
		call error (0, "GSREFIT: Unknown curve type.")
	    }

	}

	call smark (sp)
	call salloc (bwz, GS_NPTS(sf), TY_REAL) 

	# index the pointers
	vzptr = GS_VECTOR(sf) - 1
	byptr = GS_YBASIS(sf)

	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    call amulr (w, z, Memr[GS_WZ(sf)], GS_NPTS(sf))
	    xorder = GS_XORDER(sf)
	    maxorder = max (GS_XORDER(sf) + 1, GS_YORDER(sf) + 1)

	    do l = 1, GS_YORDER(sf) {
		call amulr (Memr[GS_WZ(sf)], YBASIS(byptr), Memr[bwz],
		    GS_NPTS(sf))
		bxptr = GS_XBASIS(sf)
	        do k = 1, xorder {
		    vindex = vzptr + k
	            VECTOR(vindex) = VECTOR(vindex) + adotr (Memr[bwz],
		        XBASIS(bxptr), GS_NPTS(sf))
		    bxptr = bxptr + GS_NPTS(sf)
	        }

		vzptr = vzptr + xorder
		switch (GS_XTERMS(sf)) {
		case GS_XNONE:
		    xorder = 1
		case GS_XHALF:
		    if ((l + GS_XORDER(sf) + 1) > maxorder)
		        xorder = xorder - 1
		default:
		    ;
		}
		byptr = byptr + GS_NPTS(sf)
	    }

	default:
	    call error (0, "GSACCUM: Unknown curve type.")
	}

	# test for number of degrees of freedom
	ier = OK
	nfree = GS_NPTS(sf) - GS_NCOEFF(sf)
	if (nfree < 0) {
	    ier = NO_DEG_FREEDOM
	    return
	}

	# calculate the values of the coefficients
	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    # solve for the coefficients by forward and back substitution
	    call rgschoslv (CHOFAC(GS_CHOFAC(sf)), GS_NCOEFF(sf),
	        GS_NCOEFF(sf), VECTOR(GS_VECTOR(sf)), COEFF(GS_COEFF(sf)))
	default:
	    call error (0, "GSSOLVE: Illegal surface type.")
	}

	# release the space
	call sfree (sp)
end
