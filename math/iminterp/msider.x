# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIDER -- Calculate the derivatives of the interpolant. The derivative
# der[i,j] = d f(x,y) / dx (i-1) dy (j-1). Therefore der[1,1] contains
# the value of the interpolant, der[2,1] the 1st derivative in x and
# der[1,2] the first derivative in y.

procedure msider (msi, x, y, der, nxder, nyder, len_der)

pointer	msi			# pointer to interpolant descriptor structure
real	x			# x value
real	y			# y value
real	der[len_der,ARB]	# derivative array
int	nxder			# number of x derivatives
int	nyder			# number of y derivatives
int	len_der			# row length of der, len_der >= nxder

int	first_point, len_coeff
int	nxterms, nyterms, nx, ny, nyd, nxd
int	i, j, ii, jj
real	sx, sy, tx, ty
real	pcoeff[MAX_NTERMS,MAX_NTERMS], pctemp[MAX_NTERMS,MAX_NTERMS]
real	sum[MAX_NTERMS], accum
pointer	index, ptr

begin
	if (nxder < 1 || nyder < 1)
	    return

	# set up coefficient array parameters
	len_coeff = MSI_NXCOEFF(msi)
	index = MSI_COEFF(msi) + MSI_FSTPNT(msi) - 1

	# zero the derivatives
	do j = 1, nyder {
	    do i = 1, nxder
		der[i,j] = 0.
	}

	# calculate the appropriate number of terms of the polynomials in
	# x and y

	switch (MSI_TYPE(msi)) {

	case II_BINEAREST:

	    nx = x + 0.5
	    ny = y + 0.5

	    ptr = index + (ny - 1) * len_coeff + nx
	    der[1,1] = COEFF(ptr) 

	    return

	case II_BILINEAR:

	    nx = x
	    ny = y
	    sx = x - nx
	    sy = y - ny
	    tx = 1. - sx
	    ty = 1. - sy

	    ptr = index + (ny - 1) * len_coeff + nx
	    der[1,1] = tx * ty * COEFF(ptr) + sx * ty * COEFF(ptr+1) +
		       sy * tx * COEFF(ptr+len_coeff) +
		       sx * sy * COEFF(ptr+len_coeff+1)

	    if (nxder > 1)
		der[2,1] = -ty * COEFF(ptr) + ty * COEFF(ptr+1) -
			   sy * COEFF(ptr+len_coeff) +
			   sy * COEFF(ptr+len_coeff+1)

	    if (nyder > 1)
		der[1,2] = -tx * COEFF(ptr) - sx * COEFF(ptr+1) +
			   tx * COEFF(ptr+len_coeff) +
			   sx * COEFF(ptr+len_coeff+1)

	    if (nyder > 1 && nxder > 1)
		der[2,2] = COEFF(ptr) - COEFF(ptr+1) - COEFF(ptr+len_coeff) +
			   COEFF(ptr+len_coeff+1)

	    return

	case II_BIPOLY3:

	    nxterms = 4
	    nyterms = 4

	    nxd = min (nxder, 4)
	    nyd = min (nyder, 4) 

	    nx = x
	    sx = x - nx
	    ny = y
	    sy = y - ny

	    first_point = MSI_FSTPNT(msi) + (ny - 2) * len_coeff + nx
	    call ii_pcpoly3 (COEFF(MSI_COEFF(msi)), first_point, len_coeff,
	    		     pcoeff, 6)

	case II_BIPOLY5:

	    nxterms = 6
	    nyterms = 6

	    nxd = min (nxder, 6)
	    nyd = min (nyder, 6)

	    nx = x
	    sx = x - nx
	    ny = y
	    sy = y - ny

	    first_point = MSI_FSTPNT(msi) + (ny - 3) * len_coeff + nx
	    call ii_pcpoly5 (COEFF(MSI_COEFF(msi)), first_point, len_coeff,
	    		    pcoeff, 6)

	case II_BISPLINE3:

	    nxterms = 4
	    nyterms = 4

	    nxd = min (nxder, 4)
	    nyd = min (nyder, 4)

	    nx = x
	    sx = x - nx
	    ny = y
	    sy = y - ny

	    first_point = MSI_FSTPNT(msi) + (ny - 2) * len_coeff + nx
	    call ii_pcspline3 (COEFF(MSI_COEFF(msi)), first_point, len_coeff,
	    			pcoeff, 6)
	}

	# evaluate the derivatives by nested multiplication
	do j = 1, nyd {

	    # set pctemp
	    do jj =  nyterms, j, -1 {
		do ii = 1, nxterms
		    pctemp[ii,jj] = pcoeff[ii,jj]
	    }

	    do i = 1, nxd {

		# accumulate the partial sums in x
		do jj = nyterms, j, -1 {
		    sum[jj] = pctemp[nxterms,jj]
		    do ii = nxterms - 1, i, -1
			sum[jj] = pctemp[ii,jj] + sum[jj] * sx
		}

		# accumulate the sum in y
		accum = sum[nyterms]
		do jj = nyterms - 1, j, -1
		    accum = sum[jj] + accum * sy

		# evaluate derivative
		der[i,j] = accum

		# differentiate in x
		do jj = nyterms, j, -1 {
		    do ii = nxterms, i + 1, -1
		        pctemp[ii,jj] = (ii - i) * pctemp[ii,jj]
		}
	    }

	    # differentiate in y
	    do jj = 1, nxterms {
		do ii = nyterms, j + 1, -1
		    pcoeff[jj,ii] = (ii - j) * pcoeff[jj,ii]
	    }
	}
end
