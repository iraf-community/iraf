# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "im2interpdef.h"
include <math/iminterp.h>

# MSISQGRL -- Procedure to integrate the 2D interpolant over a rectangular
# region in x and y.

real procedure msisqgrl (msi, x1, x2, y1, y2)

pointer	msi		# pointer to the interpolant descriptor structure
real	x1, x2		# x integration limits
real	y1, y2		# y integration limits

int	interp_type, nylmin, nylmax, offset, i
pointer	xintegrl, ptr
real	xmin, xmax, ymin, ymax, accum
real	ii_1dinteg()

begin
	# set up 1D interpolant type to match 2-D interpolant
	switch (MSI_TYPE(msi)) {
	case II_BINEAREST:
	    interp_type = II_NEAREST
	case II_BILINEAR:
	    interp_type = II_LINEAR
	case II_BIPOLY3:
	    interp_type = II_POLY3
	case II_BIPOLY5:
	    interp_type = II_POLY5
	case II_BISPLINE3:
	    interp_type = II_SPLINE3
	}

	# set up temporary storage for x integrals
	call calloc (xintegrl, MSI_NYCOEFF(msi), TY_REAL)

	# find the appropriate range in y of the coeff array
	offset = mod (MSI_FSTPNT(msi), MSI_NXCOEFF(msi)) 
	nylmin = int (y1 + 0.5) + offset
	nylmax = int (y2 + 0.5) + offset

	# switch order of x integration at the end
	xmin = x1
	xmax = x2
	if (x2 < x1) {
	    xmax = x2
	    xmin = x1
	}

	# integrate in x
	ptr = MSI_COEFF(msi) + offset + (nylmin - 1) * MSI_NXCOEFF(msi)
	do i = nylmin, nylmax {
	    Memr[xintegrl+i-1] = ii_1dinteg (COEFF(ptr), xmin, xmax,
	        interp_type)
	    if (x2 < x1)
		Memr[xintegrl+i-1] = - Memr[xintegrl+i-1]
	    ptr = ptr + MSI_NXCOEFF(msi)
	}

	# switch order of y integration at end
	ymin = y1
	ymax = y2
	if (y2 < y1) {
	    ymax = y2
	    ymin = y1
	}

	# integrate in y
	accum = ii_1dinteg (Memr[xintegrl+offset], y1, y2, II_NEAREST)
	if (interp_type == II_SPLINE3)
	    accum = accum * 6.

	# free space
	call mfree (xintegrl, TY_REAL)
	if (y2 < y1)
	    return (-accum)
	else
	    return (accum)
end
