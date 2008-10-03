# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MRIDER -- Procedure to evaluate the derivatives of the interpolant
# without the storage overhead required by the sequential version.
# The derivatives are stored such that der[1,1] = the value of the
# interpolant at x and y, der[2,1] = the first derivative in x and
# der[2,1] = the first derivative in y.

procedure mrider (x, y, datain, nxpix, nypix, len_datain, der, nxder, nyder,
		    len_der, interp_type)

real	x[ARB]				# x value
real	y[ARB]				# y value
real	datain[len_datain,ARB]		# data array
size_t	nxpix				# number of x data points
size_t	nypix				# number of y data points
size_t	len_datain			# row length of datain
real	der[len_der, ARB]		# array of derivatives
int	nxder				# number of derivatives in x
int	nyder				# number of derivatives in y
int	len_der				# row length of der, len_der >= nxder
int	interp_type			# interpolant type

size_t	sz_val
size_t	c_1
int	nx, ny, li, lj, xindex, yindex
int	nxterms, nyterms, row_length
int	index, first_row, last_row
int	i, j, ii, jj
size_t	kx, ky
pointer	tmp
real	coeff[SPLPTS+3,SPLPTS+3], pcoeff[MAX_NTERMS,MAX_NTERMS]
real	pctemp[MAX_NTERMS,MAX_NTERMS], sum[MAX_NTERMS]
real	hold21, hold12, hold22, accum, deltax, deltay, tmpx[4], tmpy[4]
real	xmin, xmax, ymin, ymax, sx, sy, tx, ty

errchk	malloc, calloc, mfree

begin
	c_1 = 1

	if (nxder < 1 || nyder < 1)
	    return

	# zero the derivatives
	do j = 1, nyder {
	    do i = 1, nxder
		der[i,j] = 0.
	}

	switch (interp_type) {

	case II_BINEAREST:

	    der[1,1] = datain[int (x[1]+.5), int (y[1]+.5)]

	    return

	case II_BISINC, II_BILSINC:

	    call ii_bisincder (x[1], y[1], der, nxder, nyder, len_der, datain,
		0, len_datain, nypix, NSINC, DX, DY)

	    return

	case II_BILINEAR:

	    nx = x[1]
	    sx = x[1] - nx
	    tx = 1. - sx

	    ny = y[1]
	    sy = y[1] - ny
	    ty = 1. - sy

	    # protect against the case where x = nxpix and/or y = nypix
	    if (nx >= nxpix)
		hold21 = 2. * datain[nx,ny] - datain[nx-1,ny]
	    else
		hold21 = datain[nx+1,ny]
	    if (ny >= nypix)
		hold12 = 2. * datain[nx,ny] - datain[nx,ny-1]
	    else
		hold12 = datain[nx,ny+1]
	    if (nx >= nxpix && ny >= nypix)
		hold22 = 2. * hold21 - (2. * datain[nx,ny-1] -
			 datain[nx-1,ny-1])
	    else if (nx >= nxpix)
		hold22 = 2. * hold12 - datain[nx-1,ny+1] 
	    else if (ny >= nypix)
		hold 22 = 2. * hold21 - datain[nx+1,ny-1] 
	    else
		hold22 = datain[nx+1,ny+1]

	    # evaluate the derivatives
	    der[1,1] = tx * ty * datain[nx,ny] + sx * ty * hold21 +
		    sy * tx * hold12 + sx * sy * hold22
	    if (nxder > 1)
		der[2,1] = - ty * datain[nx,ny] + ty * hold21 -
			     sy * hold12 + sy * hold22
	    if (nyder > 1)
		der[1,2] = - tx * datain[nx,ny] - sx * hold21 +
			     tx * hold12 + sx * hold22 
	    if (nxder > 1 && nyder > 1)
		der[2,2] = datain[nx,ny] - hold21 - hold12 + hold22


	    return

	case II_BIDRIZZLE:
            call ii_bidriz1 (datain, 0, len_datain, x, y, der[1,1], c_1, BADVAL)
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
                    call ii_bidriz1 (datain, 0, len_datain, tmpx, tmpy,
			accum, c_1, BADVAL)
                    tmpx[1] = (xmax - xmin) / 2.0; tmpy[1] = ymin
                    tmpx[2] = xmax; tmpy[2] = ymin
                    tmpx[3] = xmax; tmpy[3] = ymax
                    tmpx[4] = (xmax - xmin) / 2.0; tmpy[4] = ymax
                    call ii_bidriz1 (datain, 0, len_datain, tmpx, tmpy,
			der[2,1], c_1, BADVAL)
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
                    call ii_bidriz1 (datain, 0, len_datain, tmpx, tmpy,
		        accum, c_1, BADVAL)
                    tmpx[1] = xmin; tmpy[1] = (ymax - ymin) /  2.0
                    tmpx[2] = xmax; tmpy[2] = (ymax - ymin) / 2.0
                    tmpx[3] = xmax; tmpy[3] = ymax
                    tmpx[4] = xmin; tmpy[4] = ymax
                    call ii_bidriz1 (datain, 0, len_datain, tmpx, tmpy,
			der[1,2], c_1, BADVAL)
                    der[1,2] = 2.0 * (der[1,2] - accum) / deltay
                }
            }

	    return

	case II_BIPOLY3:

	    row_length = SPLPTS + 3

	    nxterms = 4
	    nyterms = 4

	    nx = x[1]
	    ny = y[1]

	    sx = x[1] - nx
	    sy = y[1] - ny

	    # use boundary projection to extend the data rows
	    yindex = 1
	    for (lj = ny - 1; lj <= ny + 2; lj = lj + 1) {

		# check that the data row is defined
		if (lj >= 1 && lj <= nypix) {

		    # extend the rows
		    xindex = 1
		    for (li = nx - 1; li <= nx + 2; li = li + 1) {
			if (li < 1)
			    coeff[xindex,yindex] = 2. * datain[1,lj] -
			    			  datain[2-li,lj]
			else if (li > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,lj] -
					   	  datain[2*nxpix-li,lj] 
			else
			    coeff[xindex,yindex] = datain[li,lj]
			xindex = xindex + 1
		    }
		} else if (lj == (nypix + 2)) {

		    # allow for the final row
		    xindex = 1
		    for (li = nx - 1; li <= nx + 2; li = li + 1) {
			if (li < 1)
			    coeff[xindex,nyterms] = 2. * datain[1,nypix-2] -
			    			  datain[2-li,nypix-2]
			else if (li > nxpix)
			    coeff[xindex,nyterms] = 2. * datain[nxpix,nypix-2] -
					   	  datain[2*nxpix-li,nypix-2] 
			else
			    coeff[xindex,nyterms] = datain[li,nypix-2]
			xindex = xindex + 1
		    }

		}

		yindex = yindex + 1
	    }


	    # project columns
	    first_row = max (1, 3 - ny)
	    if (first_row > 1) {
	        for (j = 1; j < first_row; j = j + 1) {
		    sz_val = nxterms
		    call awsur (coeff[1, first_row], coeff[1, 2*first_row-j],
		    coeff[1,j], sz_val, 2., -1.)
		}
	    }

	    last_row = min (nxterms, nypix - ny + 2)
	    if (last_row < nxterms) {
	        for (j = last_row + 1; j <= nxterms - 1; j = j + 1) {
		    sz_val = nxterms
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-j],
		    	        coeff[1,j], sz_val, 2., -1.)
		}
		if (last_row == 2) {
		    sz_val = nxterms
		    call awsur (coeff[1,last_row], coeff[1,4], coeff[1,4],
				sz_val, 2., -1.)
		} else {
		    sz_val = nxterms
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-4],
				coeff[1,4], sz_val, 2., -1.)
		}
	    }

	    # calculate the coefficients of the bicubic polynomial
	    call ii_pcpoly3 (coeff, 2, row_length, pcoeff, MAX_NTERMS)

	case II_BIPOLY5:
	    row_length = SPLPTS + 3

	    nxterms = 6
	    nyterms = 6

	    nx = x[1]
	    ny = y[1]

	    sx = x[1] - nx
	    sy = y[1] - ny

	    # extend rows of data
	    yindex = 1
	    for (lj = ny - 2; lj <= ny + 3; lj = lj + 1) {

		# select the  rows containing data
		if (lj >= 1 && lj <= nypix) {

		    # extend the rows
		    xindex = 1
		    for (li = nx - 2; li <= nx + 3; li = li + 1) {
			if (li < 1)
			    coeff[xindex,yindex] = 2. * datain[1,lj] -
			    			  datain[2-li,lj]
			else if (li > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,lj] -
					   	  datain[2*nxpix-li,lj] 
			else
			    coeff[xindex,yindex] = datain[li,lj]
			xindex = xindex + 1
		    }

		} else if (lj == (ny + 3)) {

		    # extend the rows
		    xindex = 1
		    for (li = nx - 2; li <= nx + 3; li = li + 1) {
			if (li < 1)
			    coeff[xindex,yindex] = 2. * datain[1,nypix-3] -
			    			  datain[2-li,nypix-3]
			else if (li > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,nypix-3] -
					   	  datain[2*nxpix-li,nypix-3] 
			else
			    coeff[xindex,yindex] = datain[li,nypix-3]
			xindex = xindex + 1
		    }

		}

		yindex = yindex + 1
	    }

	    # project columns

	    first_row = max (1, 4 - ny)
	    if (first_row > 1) {
	        for (j = 1; j < first_row; j = j + 1) {
		    sz_val = nxterms
		    call awsur (coeff[1,first_row], coeff[1,2*first_row-j],
		    		coeff[1,j], sz_val, 2., -1.)
		}
	    }

	    last_row = min (nxterms, nypix - ny + 3) 
	    if (last_row < nxterms) {
	        for (j = last_row + 1; j <= nxterms - 1; j = j + 1) {
		    sz_val = nxterms
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-j],
		    		coeff[1,j], sz_val, 2., -1.)
		}
		if (last_row == 3) {
		    sz_val = nxterms
		    call awsur (coeff[1,last_row], coeff[1,6], coeff[1,6],
				sz_val, 2., -1.)
		} else {
		    sz_val = nxterms
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-6],
				coeff[1,6], sz_val, 2., -1.)
		}
	    }

	    # caculate the polynomial coeffcients
	    call ii_pcpoly5 (coeff, 3, row_length, pcoeff, MAX_NTERMS)


	case II_BISPLINE3:
	    row_length = SPLPTS + 3

	    nxterms = 4
	    nyterms = 4

	    nx = x[1]
	    ny = y[1]

	    sx = x[1] - nx
	    sy = y[1] - ny

	    # allocate space for temporary array and 0 file
	    sz_val = row_length * row_length
	    call calloc (tmp, sz_val, TY_REAL)

	    ky = 0
	    # maximum number of points used in each direction is SPLPTS
	    for (lj = ny - SPLPTS/2 + 1; lj <= ny + SPLPTS/2; lj = lj + 1) {

		if (lj < 1 || lj > nypix)
		    ;
		else {
		    ky = ky + 1
		    if (ky == 1)
			yindex = ny - lj + 1

	    	    kx = 0
		    for (li = nx - SPLPTS/2 + 1; li <= nx + SPLPTS/2; li = li + 1) {
			if (li < 1 || li > nxpix)
			    ;
			else {
			    kx = kx + 1
			    if (kx == 1)
				xindex = nx - li + 1
			    coeff[kx+1,ky+1] = datain[li,lj]
			}
		    }

		    coeff[1,ky+1] = 0.
		    coeff[kx+2,ky+1] = 0.
		    coeff[kx+3,ky+1] = 0.

		}
	    }

	    # zero out 1st and last 2 rows
	    sz_val = kx+3
	    call amovkr (0., coeff[1,1], sz_val)
	    call amovkr (0., coeff[1,ky+2], sz_val)
	    call amovkr (0., coeff[1,ky+3], sz_val)

	    # calculate the spline coefficients
	    call ii_spline2d (coeff, Memr[tmp], kx, ky+2, row_length,
			      row_length)
	    call ii_spline2d (Memr[tmp], coeff, ky, kx+2, row_length,
	    		      row_length)

	    # calculate the polynomial coefficients
	    index = (yindex - 1) * row_length + xindex + 1
	    call ii_pcspline3 (coeff, index, row_length, pcoeff, MAX_NTERMS)

	    # free space
	    call mfree (tmp, TY_REAL)
	}

	# evaluate the derivatives of the higher order interpolants
	do j = 1, nyder {

	    # set pctemp
	    do jj = nyterms, j, -1 {
		do ii = 1, nxterms
		    pctemp[ii,jj] = pcoeff[ii,jj]
	    }

	    do i = 1, nxder {

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

		# evaulate the derivative
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
