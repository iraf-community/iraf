# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "dgsurfitdef.h"

# GSDER -- Procedure to calculate a new surface which is a derivative of
# the previous surface

procedure dgsder (sf1, x, y, zfit, npts, nxd, nyd)

pointer	sf1		# pointer to the previous surface
double	x[npts]		# x values
double	y[npts]		# y values
double	zfit[npts]	# fitted values
int	npts		# number of points
int	nxd, nyd	# order of the derivatives in x and y

double	norm
int	ncoeff, nxder, nyder, i, j
int	order, maxorder1, maxorder2, nmove1, nmove2
pointer	sf2, sp, coeff, ptr1, ptr2

begin
	if (sf1 == NULL)
	    return

	if (nxd < 0 || nyd < 0)
	    call error (0, "GSDER: Order of derivatives cannot be < 0")

	if (nxd == 0 && nyd == 0) {
	    call dgsvector (sf1, x, y, zfit, npts)
	    return
	}

	# allocate space for new surface
	call calloc (sf2, LEN_GSSTRUCT, TY_STRUCT)

	# check the order of the derivatives and return 0 if the order is
	# high
	nxder = min (nxd, GS_NXCOEFF(sf1))
	nyder = min (nyd, GS_NYCOEFF(sf1))
	if (nxder >= GS_NXCOEFF(sf1) && nyder >= GS_NYCOEFF(sf1))
	    call amovkd (double(0.0), zfit, npts)

	# set up new surface
	GS_TYPE(sf2) = GS_TYPE(sf1)

	# set the derivative surface parameters
	switch (GS_TYPE(sf2)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    GS_XTERMS(sf2) = GS_XTERMS(sf1)

	    # find the order of the new surface
	    switch (GS_XTERMS(sf2)) {
	    case GS_XNONE: 
		if (nxder > 0 && nyder > 0) {
		    GS_NXCOEFF(sf2) = 1
		    GS_XORDER(sf2) = 1
		    GS_NYCOEFF(sf2) = 1
		    GS_YORDER(sf2) = 1
		    GS_NCOEFF(sf2) = 1
		} else if (nxder > 0) {
		    GS_NXCOEFF(sf2) = max (1, GS_NXCOEFF(sf1) - nxder)
		    GS_XORDER(sf2) = max (1, GS_NXCOEFF(sf1) - nxder)
		    GS_NYCOEFF(sf2) = 1
		    GS_YORDER(sf2) = 1
		    GS_NCOEFF(sf2) = GS_NXCOEFF(sf2)
		} else if (nyder > 0) {
		    GS_NXCOEFF(sf2) = 1
		    GS_XORDER(sf2) = 1
		    GS_NYCOEFF(sf2) = max (1, GS_NYCOEFF(sf1) - nyder)
		    GS_YORDER(sf2) = max (1, GS_NYCOEFF(sf1) - nyder)
		    GS_NCOEFF(sf2) = GS_NYCOEFF(sf2)
		}

	    case GS_XHALF:
		if ((nxder >= GS_NXCOEFF(sf1)) || (nyder >= GS_NYCOEFF(sf1)) ||
		    (nxder + nyder) >= max (GS_NXCOEFF(sf1),
		    GS_NYCOEFF(sf1))) {
		    GS_NXCOEFF(sf2) = 1
		    GS_XORDER(sf2) = 1
		    GS_NYCOEFF(sf2) = 1
		    GS_YORDER(sf2) = 1
		    GS_NCOEFF(sf2) = 1
		} else {
		    maxorder1 = max (GS_XORDER(sf1) + 1, GS_YORDER(sf1) + 1)
		    order = max (1, min (maxorder1 - 1 - nyder - nxder,
		        GS_NXCOEFF(sf1) - nxder))
	            GS_NXCOEFF(sf2) = order
	            GS_XORDER(sf2) = order
		    order = max (1, min (maxorder1 - 1 - nyder - nxder,
		        GS_NYCOEFF(sf1) - nyder))
	            GS_NYCOEFF(sf2) = order
	            GS_YORDER(sf2) = order
		    order = min (GS_XORDER(sf2), GS_YORDER(sf2))
		    GS_NCOEFF(sf2) = GS_NXCOEFF(sf2) * GS_NYCOEFF(sf2)  -
			order * (order - 1) / 2
		}

	    default:
		if (nxder >= GS_NXCOEFF(sf1) || nyder >= GS_NYCOEFF(sf1)) {
		    GS_NXCOEFF(sf2) = 1
		    GS_XORDER(sf2) = 1
		    GS_NYCOEFF(sf2) = 1
		    GS_YORDER(sf2) = 1
		    GS_NCOEFF(sf2) = 1
		} else {
	            GS_NXCOEFF(sf2) = max (1, GS_NXCOEFF(sf1) - nxder)
	            GS_XORDER(sf2) = max (1, GS_XORDER(sf1) - nxder)
	            GS_NYCOEFF(sf2) = max (1, GS_NYCOEFF(sf1) - nyder)
	            GS_YORDER(sf2) = max (1, GS_YORDER(sf1) - nyder)
		    GS_NCOEFF(sf2) = GS_NXCOEFF(sf2) * GS_NYCOEFF(sf2) 
		}
	    }

	    # define the data limits
	    GS_XMIN(sf2) = GS_XMIN(sf1)
	    GS_XMAX(sf2) = GS_XMAX(sf1)
	    GS_XRANGE(sf2) = GS_XRANGE(sf1)
	    GS_XMAXMIN(sf2) = GS_XMAXMIN(sf1)
	    GS_YMIN(sf2) = GS_YMIN(sf1)
	    GS_YMAX(sf2) = GS_YMAX(sf1)
	    GS_YRANGE(sf2) = GS_YRANGE(sf1)
	    GS_YMAXMIN(sf2) = GS_YMAXMIN(sf1)

	default:
	    call error (0, "GSDER: Unknown surface type.")
	}

	# set remaining surface pointers to NULL
	GS_XBASIS(sf2) = NULL
	GS_YBASIS(sf2) = NULL
	GS_MATRIX(sf2) = NULL
	GS_CHOFAC(sf2) = NULL
	GS_VECTOR(sf2) = NULL
	GS_COEFF(sf2) = NULL
	GS_WZ(sf2) = NULL

	# allocate space for coefficients
	call calloc (GS_COEFF(sf2), GS_NCOEFF(sf2), TY_DOUBLE)

	# get coefficients
	call smark (sp)
	call salloc (coeff, GS_NCOEFF(sf1), TY_DOUBLE)
	call dgscoeff (sf1, Memd[coeff], ncoeff)

	# compute the new coefficients
	switch (GS_XTERMS(sf2)) {
	case GS_XFULL:
	    if (nxder >= GS_NXCOEFF(sf1) || nyder >= GS_NYCOEFF(sf1))
		COEFF(GS_COEFF(sf2)) = 0.
	    else {
	        ptr2 = GS_COEFF(sf2) + (GS_NYCOEFF(sf2) - 1) * GS_NXCOEFF(sf2)
	        ptr1 = coeff + (GS_NYCOEFF(sf1) - 1) * GS_NXCOEFF(sf1)
	        do i = GS_NYCOEFF(sf1), nyder + 1, -1 {
		    call amovd (Memd[ptr1+nxder], COEFF(ptr2),
		        GS_NXCOEFF(sf2))
	            ptr2 = ptr2 - GS_NXCOEFF(sf2)
	            ptr1 = ptr1 - GS_NXCOEFF(sf1)
	        }
	    }

	case GS_XHALF:
	    if ((nxder >= GS_NXCOEFF(sf1)) || (nyder >= GS_NYCOEFF(sf1)) ||
	        (nxder + nyder) >= max (GS_NXCOEFF(sf1), GS_NYCOEFF(sf1)))
		COEFF(GS_COEFF(sf2)) = 0.
	    else {
	        maxorder1 = max (GS_XORDER(sf1) + 1, GS_YORDER(sf1) + 1)
	        maxorder2 = max (GS_XORDER(sf2) + 1, GS_YORDER(sf2) + 1)
	        ptr2 = GS_COEFF(sf2) + GS_NCOEFF(sf2)
	        ptr1 = coeff + GS_NCOEFF(sf1)
	        do i = GS_NYCOEFF(sf1), nyder + 1, -1 {
		    nmove1 = max (0, min (maxorder1 - i, GS_NXCOEFF(sf1)))
		    nmove2 = max (0, min (maxorder2 - i + nyder,
		        GS_NXCOEFF(sf2)))
		    ptr1 = ptr1 - nmove1
		    ptr2 = ptr2 - nmove2
		    call amovd (Memd[ptr1+nxder], COEFF(ptr2), nmove2)
	        }
	    }

	default:
	    if (nxder > 0 && nyder > 0)
		COEFF(GS_COEFF(sf2)) = 0.
	    else if (nxder > 0) { 
		if (nxder >= GS_NXCOEFF(sf1))
		    COEFF(GS_COEFF(sf2)) = 0.
		else {
		    ptr1 = coeff
		    ptr2 = GS_COEFF(sf2) + GS_NCOEFF(sf2) - 1
		    do j = GS_NXCOEFF(sf1), nxder + 1, -1 {
		        COEFF(ptr2) = Memd[ptr1+j-1]
		        ptr2 = ptr2 - 1
		    }
		}
	    } else if (nyder > 0) {
		if (nyder >= GS_NYCOEFF(sf1))
		    COEFF(GS_COEFF(sf2)) = 0.
		else {
		    ptr1 = coeff + GS_NCOEFF(sf1) - 1
		    ptr2 = GS_COEFF(sf2)
		    do i = GS_NYCOEFF(sf1), nyder + 1, -1
		        ptr1 = ptr1 - 1
		    call amovd (Memd[ptr1+1], COEFF(ptr2), GS_NCOEFF(sf2))
		}
	    }
	}

	# evaluate the derivatives
	switch (GS_TYPE(sf2)) {
	case GS_POLYNOMIAL:
	    call dgs_derpoly (COEFF(GS_COEFF(sf2)), x, y, zfit, npts,
	        GS_XTERMS(sf2), GS_XORDER(sf2), GS_YORDER(sf2), nxder,
		nyder, GS_XMAXMIN(sf2), GS_XRANGE(sf2), GS_YMAXMIN(sf2),
		GS_YRANGE(sf2))

	case GS_CHEBYSHEV:
	    call dgs_dercheb (COEFF(GS_COEFF(sf2)), x, y, zfit, npts,
	        GS_XTERMS(sf2), GS_XORDER(sf2), GS_YORDER(sf2), nxder,
	        nyder, GS_XMAXMIN(sf2), GS_XRANGE(sf2), GS_YMAXMIN(sf2),
	        GS_YRANGE(sf2))

	case GS_LEGENDRE:
	    call dgs_derleg (COEFF(GS_COEFF(sf2)), x, y, zfit, npts,
	        GS_XTERMS(sf2), GS_XORDER(sf2), GS_YORDER(sf2), nxder,
	        nyder, GS_XMAXMIN(sf2), GS_XRANGE(sf2), GS_YMAXMIN(sf2),
	        GS_YRANGE(sf2))

	default:
	    call error (0, "GSVECTOR: Unknown surface type.")
	}

        # Normalize.
        if (GS_TYPE(sf2) != GS_POLYNOMIAL) {
            norm = (2. / (GS_XMAX(sf2) - GS_XMIN(sf2))) ** nxder * (2. /
                (GS_YMAX(sf2) - GS_YMIN(sf2))) ** nyder
            call amulkd (zfit, norm, zfit, npts)
        }

	# free the space
	call dgsfree (sf2)
	call sfree (sp)
end
