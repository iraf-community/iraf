# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>

include "gsurfitdef.h"

# GSADD -- Procedure to add the fits from two surfaces together. The surfaces
# must be the same type and the fit must cover the same range of data in x
# and y. This is a special function.

procedure gsadd (sf1, sf2, sf3)

pointer	sf1		# pointer to the first surface
pointer	sf2		# pointer to the second surface
pointer	sf3		# pointer to the output surface

int	i
pointer	sfx, sfn, ptr1, ptr2, ptr3
bool	fpequalr()

begin
	# test for NULL surface
	if (sf1 == NULL && sf2 == NULL) {
	    sf3 = NULL
	    return
	} else if (sf1 == NULL) {
	    call gscopy (sf2, sf3)
	    return
	} else if (sf2 == NULL) {
	    call gscopy (sf1, sf3)
	    return
	}

	# test that function type is the same
	if (GS_TYPE(sf1) != GS_TYPE(sf2))
	    call error (0, "GSADD: Incompatable surface types.")

	# test that mins and maxs are the same
	if (! fpequalr (GS_XMIN(sf1), GS_XMIN(sf2)))
	    call error (0, "GSADD: X ranges not identical.")
	if (! fpequalr (GS_XMAX(sf1), GS_XMAX(sf2)))
	    call error (0, "GSADD: X ranges not identical.")
	if (! fpequalr (GS_YMIN(sf1), GS_YMIN(sf2)))
	    call error (0, "GSADD: Y ranges not identical.")
	if (! fpequalr (GS_YMAX(sf1), GS_YMAX(sf2)))
	    call error (0, "GSADD: Y ranges not identical.")

	# allocate space for the pointer
	call calloc (sf3, LEN_GSSTRUCT, TY_STRUCT)

	# copy parameters
	GS_TYPE(sf3) = GS_TYPE(sf1)

	switch (GS_TYPE(sf3)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:
	    GS_NXCOEFF(sf3) = max (GS_NXCOEFF(sf1), GS_NXCOEFF(sf2))
	    GS_XORDER(sf3) = max (GS_XORDER(sf1), GS_XORDER(sf2))
	    GS_XMIN(sf3) = GS_XMIN(sf1)
	    GS_XMAX(sf3) = GS_XMAX(sf1)
	    GS_XRANGE(sf3) = GS_XRANGE(sf1)
	    GS_XMAXMIN(sf3) = GS_XMAXMIN(sf1)
	    GS_NYCOEFF(sf3) = max (GS_NYCOEFF(sf1), GS_NYCOEFF(sf2))
	    GS_YORDER(sf3) = max (GS_YORDER(sf1), GS_YORDER(sf2))
	    GS_YMIN(sf3) = GS_YMIN(sf1)
	    GS_YMAX(sf3) = GS_YMAX(sf1)
	    GS_YRANGE(sf3) = GS_YRANGE(sf1)
	    GS_YMAXMIN(sf3) = GS_YMAXMIN(sf1)
	    if (GS_XTERMS(sf1) == YES || GS_XTERMS(sf2) == YES)
		GS_XTERMS(sf3) = YES
	    else
		GS_XTERMS(sf3) = NO
	    if (GS_XTERMS(sf3) == NO)
		GS_NCOEFF(sf3) = GS_NXCOEFF(sf3) + GS_NYCOEFF(sf3) - 1
	    else
	        GS_NCOEFF(sf3) = GS_NXCOEFF(sf3) * GS_NYCOEFF(sf3)
	default:
	    call error (0, "GSADD: Unknown curve type.")
	}

	# set pointers to NULL
	GS_XBASIS(sf3) = NULL
	GS_YBASIS(sf3) = NULL
	GS_MATRIX(sf3) = NULL
	GS_CHOFAC(sf3) = NULL
	GS_VECTOR(sf3) = NULL
	GS_COEFF(sf3) = NULL
	GS_WZ(sf3) = NULL

	# calculate the coefficients
	call calloc (GS_COEFF(sf3), GS_NCOEFF(sf3), TY_REAL)

	if (GS_XTERMS(sf1) == YES && GS_XTERMS(sf2) == YES) {

	    ptr1 = GS_COEFF(sf1)
	    ptr3 = GS_COEFF(sf3)
	    do i = 1, GS_NYCOEFF(sf1) {
		call amovr (COEFF(ptr1), COEFF(ptr3), GS_NXCOEFF(sf1))
		ptr1 = ptr1 + GS_NXCOEFF(sf1)
		ptr3 = ptr3 + GS_NXCOEFF(sf3)
	    }

	    ptr2 = GS_COEFF(sf2)
	    ptr3 = GS_COEFF(sf3)
	    do i = 1, GS_NYCOEFF(sf2) {
		call aaddr (COEFF(ptr3), COEFF(ptr2), COEFF(ptr3),
		    GS_NXCOEFF(sf2)) 
		ptr2 = ptr2 + GS_NXCOEFF(sf2)
		ptr3 = ptr3 + GS_NXCOEFF(sf3)
	    }

	} else if (GS_XTERMS(sf1) == NO && GS_XTERMS(sf2) == NO) {

	    ptr1 = GS_COEFF(sf1)
	    ptr2 = GS_COEFF(sf2)
	    ptr3 = GS_COEFF(sf3)
	    call amovr (COEFF(ptr1), COEFF(ptr3), GS_NXCOEFF(sf1))
	    call aaddr (COEFF(ptr3), COEFF(ptr2), COEFF(ptr3), GS_NXCOEFF(sf2))

	    ptr1 = ptr1 + GS_NXCOEFF(sf1)
	    ptr2 = ptr2 + GS_NXCOEFF(sf2)
	    ptr3 = ptr3 + GS_NXCOEFF(sf3)
	    call amovr (COEFF(ptr1), COEFF(ptr3), GS_NXCOEFF(sf1) - 1)
	    call aaddr (COEFF(ptr3), COEFF(ptr2), COEFF(ptr3),
	        GS_NXCOEFF(sf2) - 1)

	} else {
	    
	    # determine which surface has cross terms
	    if (GS_XTERMS(sf1) == YES) {
		sfx = sf1
		sfn = sf2
	    } else {
		sfx = sf2
		sfn = sf1
	    }

	    # add in surface with cross terms
	    ptr1 = GS_COEFF(sfx)
	    ptr3 = GS_COEFF(sf3)
	    do i = 1, GS_NYCOEFF(sf3) {
		call amovr (COEFF(ptr1), COEFF(ptr3), GS_NXCOEFF(sfx))
		ptr1 = ptr1 + GS_NXCOEFF(sfx)
		ptr3 = ptr3 + GS_NXCOEFF(sf3)
	    }

	    # add remaining surface
	    ptr2 = GS_COEFF(sfn)
	    ptr3 = GS_COEFF(sf3)
	    call aaddr (COEFF(ptr3), COEFF(ptr2), COEFF(ptr3), GS_NXCOEFF(sfn))

	    ptr2 = ptr2 + GS_NXCOEFF(sfn)
	    ptr3 = ptr3 + GS_NXCOEFF(sf3)
	    do i = 2, GS_NYCOEFF(sfn) {
		COEFF(ptr3) = COEFF(ptr3) + COEFF(ptr2)
	        ptr2 = ptr2 + 1
	        ptr3 = ptr3 + GS_NXCOEFF(sf3)
	    }
	}
end
