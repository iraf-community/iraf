# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "im2interpdef.h"
include <math/iminterp.h>

# MSISQGRL -- Procedure to integrate the 2D interpolant over a rectangular
# region.

real procedure msisqgrl (msi, x1, x2, y1, y2)

pointer	msi		# pointer to the interpolant descriptor structure
real	x1, x2		# x integration limits
real	y1, y2		# y integration limits

int	i, interp_type, nylmin, nylmax, offset 
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
	case II_BIDRIZZLE:
	    interp_type = II_DRIZZLE
	case II_BIPOLY3:
	    interp_type = II_POLY3
	case II_BIPOLY5:
	    interp_type = II_POLY5
	case II_BISPLINE3:
	    interp_type = II_SPLINE3
	case II_BISINC:
	    interp_type = II_SINC
	case II_BILSINC:
	    interp_type = II_LSINC
	}

	# set up temporary storage for x integrals
	call calloc (xintegrl, MSI_NYCOEFF(msi), TY_REAL)

	# switch order of x integration at the end
	xmin = x1
	xmax = x2
	if (x2 < x1) {
	    xmax = x2
	    xmin = x1
	}

	# switch order of y integration at end
	ymin = y1
	ymax = y2
	if (y2 < y1) {
	    ymax = y2
	    ymin = y1
	}

	# find the appropriate range in y in the coeff array
	offset = mod (MSI_FSTPNT(msi), MSI_NXCOEFF(msi)) 
	nylmin = max (1, min (MSI_NYCOEFF(msi), int (ymin + 0.5))) 
	nylmax = min (MSI_NYCOEFF(msi), max (1, int (ymax + 0.5)))
	nylmin = nylmin + offset
	nylmax = nylmax + offset

	# integrate in x
	ptr = MSI_COEFF(msi) + offset + (nylmin - 1) * MSI_NXCOEFF(msi)
	do i = nylmin, nylmax {
	    Memr[xintegrl+i-1] = ii_1dinteg (COEFF(ptr), MSI_NXCOEFF(msi),
	        xmin, xmax, interp_type, MSI_NSINC(msi), DX, MSI_XPIXFRAC(msi))
	    if (x2 < x1)
		Memr[xintegrl+i-1] = - Memr[xintegrl+i-1]
	    ptr = ptr + MSI_NXCOEFF(msi)
	}

	# integrate in y
	if (interp_type == II_SPLINE3) {
	    call amulkr (Memr[xintegrl], 6.0, Memr[xintegrl],
	        MSI_NYCOEFF(msi))
	    accum = ii_1dinteg (Memr[xintegrl+offset], MSI_NYCOEFF(msi),
	        ymin, ymax, II_NEAREST, MSI_NSINC(msi), DY, MSI_YPIXFRAC(msi))
	} else
	    accum = ii_1dinteg (Memr[xintegrl+offset], MSI_NYCOEFF(msi),
	        ymin, ymax, II_NEAREST, MSI_NSINC(msi), DY, MSI_YPIXFRAC(msi))

	# free space
	call mfree (xintegrl, TY_REAL)

	# correct for integration error.
	if (y2 < y1)
	    return (-accum)
	else
	    return (accum)
end
