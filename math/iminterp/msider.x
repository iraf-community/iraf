# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIDER -- Calculate the derivatives of the interpolant. The derivative
# der[i,j] = d f(x,y) / dx (i-1) dy (j-1). Therefore der[1,1] contains
# the value of the interpolant, der[2,1] the 1st derivative in x and
# der[1,2] the first derivative in y.

procedure msider (msi, x, y, der, nxder, nyder, len_der)

pointer	msi			# pointer to interpolant descriptor structure
real	x[ARB]			# x value
real	y[ARB]			# y value
real	der[len_der,ARB]	# derivative array
int	nxder			# number of x derivatives
int	nyder			# number of y derivatives
int	len_der			# row length of der, len_der >= nxder

int	first_point, len_coeff
int	nxterms, nyterms, nx, ny, nyd, nxd
int	i, j, ii, jj
real	sx, sy, tx, ty, xmin, xmax, ymin, ymax
real	pcoeff[MAX_NTERMS,MAX_NTERMS], pctemp[MAX_NTERMS,MAX_NTERMS]
real	sum[MAX_NTERMS], accum, deltax, deltay, tmpx[4], tmpy[4]
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

	    nx = x[1] + 0.5
	    ny = y[1] + 0.5

	    ptr = index + (ny - 1) * len_coeff + nx
	    der[1,1] = COEFF(ptr) 

	    return

	case II_BISINC, II_BILSINC:

	    call ii_bisincder (x[1], y[1], der, nxder, nyder, len_der,
	        COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi), MSI_NXCOEFF(msi),
		MSI_NYCOEFF(msi), MSI_NSINC(msi), DX, DY)

	    return

	case II_BILINEAR:

	    nx = x[1]
	    ny = y[1]
	    sx = x[1] - nx
	    sy = y[1] - ny
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

	case II_BIDRIZZLE:
            if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0)
                call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                    MSI_NXCOEFF(msi), x, y, der[1,1], 1, MSI_BADVAL(msi))
            #else if (MSI_XPIXFRAC(msi) <= 0.0 && MSI_YPIXFRAC(msi) <= 0.0)
                #call ii_bidriz0 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                    #MSI_NXCOEFF(msi), x, y, der[1,1], 1, MSI_BADVAL(msi))
            else
                call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                    MSI_NXCOEFF(msi), x, y, der[1,1], 1, MSI_XPIXFRAC(msi),
                    MSI_YPIXFRAC(msi), MSI_BADVAL(msi))

	    if (nxder > 1) {
		xmax = max (x[1], x[2], x[3], x[4])
		xmin = min (x[1], x[2], x[3], x[4])
		ymax = max (y[1], y[2], y[3], y[4])
		ymin = min (y[1], y[2], y[3], y[4])
		deltax = xmax - xmin
		if (deltax == 0.0)
		    der[2,1] = 0.0
		else {
		    tmpx[1] = xmin; tmpy[1] = ymin
		    tmpx[2] = (xmax - xmin) / 2.0; tmpy[2] = ymin
		    tmpx[3] = (xmax - xmin) / 2.0; tmpy[3] = ymax
		    tmpx[4] = xmin; tmpy[4] = ymax
            	    if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0)
                        call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, accum, 1,
			    MSI_BADVAL(msi))
            	    #else if (MSI_XPIXFRAC(msi) <= 0.0 &&
		        #MSI_YPIXFRAC(msi) <= 0.0)
                        #call ii_bidriz0 (COEFF(MSI_COEFF(msi)),
			    #MSI_FSTPNT(msi), MSI_NXCOEFF(msi), tmpx, tmpy,
			    #accum, 1, MSI_BADVAL(msi))
            	    else
                        call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, accum, 1,
			    MSI_XPIXFRAC(msi), MSI_YPIXFRAC(msi),
			    MSI_BADVAL(msi))
		    tmpx[1] = (xmax - xmin) / 2.0; tmpy[1] = ymin
		    tmpx[2] = xmax; tmpy[2] = ymin
		    tmpx[3] = xmax; tmpy[3] = ymax
		    tmpx[4] = (xmax - xmin) / 2.0; tmpy[4] = ymax
            	    if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0)
                        call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, der[2,1], 1,
			    MSI_BADVAL(msi))
            	    #else if (MSI_XPIXFRAC(msi) <= 0.0 &&
		        #MSI_YPIXFRAC(msi) <= 0.0)
                        #call ii_bidriz0 (COEFF(MSI_COEFF(msi)),
			    #MSI_FSTPNT(msi), MSI_NXCOEFF(msi), tmpx, tmpy,
			    #der[2,1], 1, MSI_BADVAL(msi))
            	    else
                        call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, der[2,1], 1,
			    MSI_XPIXFRAC(msi), MSI_YPIXFRAC(msi),
			    MSI_BADVAL(msi))
		    der[2,1] = 2.0 * (der[2,1] - accum) / deltax
		}
	    }
	    if (nyder > 1) {
		deltay = ymax - ymin
		if (deltay == 0.0)
		    der[1,2] = 0.0
		else {
		    tmpx[1] = xmin; tmpy[1] =  ymin
		    tmpx[2] = xmax; tmpy[2] = ymin
		    tmpx[3] = xmax; tmpy[3] = (ymax - ymin) / 2.0
		    tmpx[4] = xmin; tmpy[4] = (ymax - ymin) / 2.0
            	    if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0)
                        call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, accum, 1,
			    MSI_BADVAL(msi))
            	    #else if (MSI_XPIXFRAC(msi) <= 0.0 &&
		        #MSI_YPIXFRAC(msi) <= 0.0)
                        #call ii_bidriz0 (COEFF(MSI_COEFF(msi)),
			    #MSI_FSTPNT(msi), MSI_NXCOEFF(msi), tmpx, tmpy,
			    #accum, 1, MSI_BADVAL(msi))
            	    else
                        call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, accum, 1,
			    MSI_XPIXFRAC(msi), MSI_YPIXFRAC(msi),
			    MSI_BADVAL(msi))
		    tmpx[1] = xmin; tmpy[1] = (ymax - ymin) /  2.0
		    tmpx[2] = xmax; tmpy[2] = (ymax - ymin) / 2.0
		    tmpx[3] = xmax; tmpy[3] = ymax
		    tmpx[4] = xmin; tmpy[4] = ymax
            	    if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0)
                        call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, der[1,2], 1,
			    MSI_BADVAL(msi))
            	    #else if (MSI_XPIXFRAC(msi) <= 0.0 &&
		        #MSI_YPIXFRAC(msi) <= 0.0)
                        #call ii_bidriz0 (COEFF(MSI_COEFF(msi)),
			    #MSI_FSTPNT(msi), MSI_NXCOEFF(msi), tmpx, tmpy,
			    #der[1,2], 1, MSI_BADVAL(msi))
            	    else
                        call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
                            MSI_NXCOEFF(msi), tmpx, tmpy, der[1,2], 1,
			    MSI_XPIXFRAC(msi), MSI_YPIXFRAC(msi),
			    MSI_BADVAL(msi))
		    der[1,2] = 2.0 * (der[1,2] - accum) / deltay
		}
	    }

	    return

	case II_BIPOLY3:

	    nxterms = 4
	    nyterms = 4

	    nxd = min (nxder, 4)
	    nyd = min (nyder, 4) 

	    nx = x[1]
	    sx = x[1] - nx
	    ny = y[1]
	    sy = y[1] - ny

	    first_point = MSI_FSTPNT(msi) + (ny - 2) * len_coeff + nx
	    call ii_pcpoly3 (COEFF(MSI_COEFF(msi)), first_point, len_coeff,
	    		     pcoeff, 6)

	case II_BIPOLY5:

	    nxterms = 6
	    nyterms = 6

	    nxd = min (nxder, 6)
	    nyd = min (nyder, 6)

	    nx = x[1]
	    sx = x[1] - nx
	    ny = y[1]
	    sy = y[1] - ny

	    first_point = MSI_FSTPNT(msi) + (ny - 3) * len_coeff + nx
	    call ii_pcpoly5 (COEFF(MSI_COEFF(msi)), first_point, len_coeff,
	    		    pcoeff, 6)

	case II_BISPLINE3:

	    nxterms = 4
	    nyterms = 4

	    nxd = min (nxder, 4)
	    nyd = min (nyder, 4)

	    nx = x[1]
	    sx = x[1] - nx
	    ny = y[1]
	    sy = y[1] - ny

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
