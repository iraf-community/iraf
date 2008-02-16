# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MRIEVAL -- Procedure to evaluate the 2D interpolant at a given value
# of x and y. MRIEVAL allows the interpolation of a few interpolated
# points without the computing time and storage required for the
# sequential version. The routine assumes that 1 <= x <= nxpix and
# 1 <= y <= nypix.

real procedure mrieval (x, y, datain, nxpix, nypix, len_datain, interp_type)

real	x[ARB]				# x value
real	y[ARB]				# y value
real	datain[len_datain,ARB]		# data array
int	nxpix				# number of x data points
int	nypix				# number of y data points
int	len_datain			# row length of datain
int	interp_type			# interpolant type

int	nx, ny, nterms, row_length
int	xindex, yindex, first_row, last_row
int	kx, ky
int	i, j
pointer	tmp
real	coeff[SPLPTS+3,SPLPTS+3]
real	hold21, hold12, hold22
real	sx, sy, tx, ty
real	xval, yval, value
errchk	malloc, calloc, mfree

begin
	switch (interp_type) {

	case II_BINEAREST:
	    return (datain[int (x[1]+0.5), int (y[1]+0.5)])

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
		hold22 = 2. * hold21 - datain[nx+1,ny-1]
	    else
		hold22 = datain[nx+1,ny+1]

	    # evaluate the interpolant
	    value = tx * ty * datain[nx,ny] + sx * ty * hold21 +
		    sy * tx * hold12 + sx * sy * hold22

	    return (value)

	case II_BIDRIZZLE:
	    call ii_bidriz1 (datain, 0, len_datain, x, y, value, 1, BADVAL)

	    return (value)

	case II_BIPOLY3:
	    row_length = SPLPTS + 3
	    nterms = 4
	    nx = x[1]
	    ny = y[1]

	    # major problem is that near the edge the interior polynomial
	    # must be defined

	    # use boundary projection to extend the data rows
	    yindex = 1
	    for (j = ny - 1; j <= ny + 2; j = j + 1) {

		# check that the data row is defined
		if (j >= 1 && j <= nypix) {

		    # extend the rows
		    xindex = 1
		    for (i = nx - 1; i <= nx + 2; i = i + 1) {
			if (i < 1)
			    coeff[xindex,yindex] = 2. * datain[1,j] -
			    			  datain[2-i,j]
			else if (i > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,j] -
					   	  datain[2*nxpix-i,j] 
			else
			    coeff[xindex,yindex] = datain[i,j]
			xindex = xindex + 1
		    }

		} else if (j == (ny + 2)) {

		    # extend the rows
		    xindex = 1
		    for (i = nx - 1; i <= nx + 2; i = i + 1) {
			if (i < 1)
			    coeff[xindex,yindex] = 2. * datain[1,nypix-2] -
			    			  datain[2-i,nypix-2]
			else if (i > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,nypix-2] -
					   	  datain[2*nxpix-i,nypix-2] 
			else
			    coeff[xindex,yindex] = datain[i,nypix-2]
			xindex = xindex + 1
		    }

		}

		yindex = yindex + 1
	    }

	    # project columns

	    first_row = max (1, 3 - ny)
	    if (first_row > 1) {
	        for (j = 1; j < first_row; j = j + 1)
		    call awsur (coeff[1, first_row], coeff[1, 2*first_row-j],
		    coeff[1,j], nterms, 2., -1.)
	    }

	    last_row = min (nterms, nypix - ny + 2)
	    if (last_row < nterms) {
	        for (j = last_row + 1; j <= nterms - 1; j = j + 1)
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-j],
		    	        coeff[1,j], nterms, 2., -1.)
		if (last_row == 2)
		    call awsur (coeff[1,last_row], coeff[1,4], coeff[1,4],
				nterms, 2., -1.)
		else
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-4],
				coeff[1,4], nterms, 2., -1.)
	    }


	    # center the x value and call evaluation routine
	    xval = 2 + (x[1] - nx)
	    yval = 2 + (y[1] - ny)
	    call ii_bipoly3 (coeff, 0, row_length, xval, yval, value, 1)

	    return (value)

	case II_BIPOLY5:
	    row_length = SPLPTS + 3
	    nterms = 6
	    nx = x[1]
	    ny = y[1]

	    # major problem is to define interior polynomial near the edge

	    # loop over the rows of data
	    yindex = 1
	    for (j = ny - 2; j <= ny + 3; j = j + 1) {

		# select the  rows containing data
		if (j >= 1 && j <= nypix) {

		    # extend the rows
		    xindex = 1
		    for (i = nx - 2; i <= nx + 3; i = i + 1) {
			if (i < 1)
			    coeff[xindex,yindex] = 2. * datain[1,j] -
			    			  datain[2-i,j]
			else if (i > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,j] -
					   	  datain[2*nxpix-i,j] 
			else
			    coeff[xindex,yindex] = datain[i,j]
			xindex = xindex + 1
		    }

		} else if (j == (ny + 3)) {

		    # extend the rows
		    xindex = 1
		    for (i = nx - 2; i <= nx + 3; i = i + 1) {
			if (i < 1)
			    coeff[xindex,yindex] = 2. * datain[1,nypix-3] -
			    			  datain[2-i,nypix-3]
			else if (i > nxpix)
			    coeff[xindex,yindex] = 2. * datain[nxpix,nypix-3] -
					   	  datain[2*nxpix-i,nypix-3] 
			else
			    coeff[xindex,yindex] = datain[i,nypix-3]
			xindex = xindex + 1
		    }

		}

		yindex = yindex + 1
	    }

	    # project columns

	    first_row = max (1, 4 - ny)
	    if (first_row > 1) {
	        for (j = 1; j < first_row; j = j + 1)
		    call awsur (coeff[1,first_row], coeff[1,2*first_row-j],
		    		coeff[1,j], nterms, 2., -1.)
	    }

	    last_row = min (nterms, nypix - ny + 3) 
	    if (last_row < nterms) {
	        for (j = last_row + 1; j <= nterms - 1; j = j + 1)
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-j],
		    		coeff[1,j], nterms, 2., -1.)
		if (last_row == 3)
		    call awsur (coeff[1,last_row], coeff[1,6], coeff[1,6],
				nterms, 2., -1.)
		else
		    call awsur (coeff[1,last_row], coeff[1,2*last_row-6],
				coeff[1,6], nterms, 2., -1.)
	    }

	    # call evaluation routine
	    xval = 3 + (x[1] - nx)
	    yval = 3 + (y[1] - ny)
	    call ii_bipoly5 (coeff, 0, row_length, xval, yval, value, 1)

	    return (value)

	case II_BISPLINE3:
	    row_length = SPLPTS + 3
	    nx = x[1]
	    ny = y[1]

	    # allocate space for temporary array and 0 file
	    call calloc (tmp, row_length * row_length, TY_REAL)

	    ky = 0
	    # maximum number of points used in each direction is SPLPTS
	    for (j = ny - SPLPTS/2 + 1; j <= ny + SPLPTS/2; j = j + 1) {

		if (j < 1 || j > nypix)
		    ;
		else {
		    ky = ky + 1
		    if (ky == 1)
			yindex = ny - j + 1

	    	    kx = 0
		    for (i = nx - SPLPTS/2 + 1; i <= nx + SPLPTS/2; i = i + 1) {
			if (i < 1 || i > nxpix)
			    ;
			else {
			    kx = kx + 1
			    if (kx == 1)
				xindex = nx - i + 1
			    coeff[kx+1,ky+1] = datain[i,j]
			}
		    }

		    coeff[1,ky+1] = 0.
		    coeff[kx+2,ky+1] = 0.
		    coeff[kx+3,ky+1] = 0.

		}
	    }

	    # zero out 1st and last 2 rows
	    call amovkr (0., coeff[1,1], kx+3)
	    call amovkr (0., coeff[1,ky+2], kx+3)
	    call amovkr (0., coeff[1,ky+3],kx+3)

	    # calculate the spline coefficients
	    call ii_spline2d (coeff, Memr[tmp], kx, ky+2, row_length,
			      row_length)
	    call ii_spline2d (Memr[tmp], coeff, ky, kx+2, row_length,
	    		      row_length)

	    # evaluate spline
	    xval = xindex + 1 + (x[1] - nx)
	    yval = yindex + 1 + (y[1] - ny)
	    call ii_bispline3 (coeff, 0, row_length, xval, yval, value, 1)

	    # free space
	    call mfree (tmp, TY_REAL)

	    return (value)

	case II_BISINC, II_BILSINC:
	    call ii_bisinc (datain, 0, len_datain, nypix, x, y, value, 1,
	        NSINC, DX, DY)

	    return (value)
	}
end
