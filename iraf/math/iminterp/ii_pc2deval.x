# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>

# II_PCPOLY3 -- Procedure to evaluate the polynomial coefficients
# of third order in x and y using Everetts formuala.

procedure ii_pcpoly3 (coeff, index, len_coeff, pcoeff, len_pcoeff)

real	coeff[ARB]		# 1D array of interpolant coeffcients
int	index			# pointer into coeff array
int	len_coeff		# row length of coeffcients
real	pcoeff[len_pcoeff,ARB]	# polynomial coefficients
int	len_pcoeff		# row length of pcoeff

int	tptr
int	i, j
real	cd20, cd21, temp[4]

begin
	# determine polynomial coefficients in x
	tptr = index
	do i = 1, 4 {

	    # calculate the central differences
	    cd20 = 1./6. * (coeff[tptr+1] - 2. * coeff[tptr] + coeff[tptr-1])
	    cd21 = 1./6. * (coeff[tptr+2] - 2. * coeff[tptr+1] + coeff[tptr])

	    # calculate the polynomial coefficients in x at each y
	    pcoeff[1,i] = coeff[tptr]
	    pcoeff[2,i] = coeff[tptr+1] - coeff[tptr] - 2. * cd20 - cd21
	    pcoeff[3,i] = 3. * cd20
	    pcoeff[4,i] = cd21 - cd20

	    tptr = tptr + len_coeff
	}

	# calculate polynomial coefficients in y
	do j = 1, 4 {

	    # calculate the central differences
	    cd20 = 1./6. * (pcoeff[j,3] - 2. * pcoeff[j,2] + pcoeff[j,1])
	    cd21 = 1./6. * (pcoeff[j,4] - 2. * pcoeff[j,3] + pcoeff[j,2])

	    # calculate the final coefficients
	    temp[1] = pcoeff[j,2] 
	    temp[2] = pcoeff[j,3] - pcoeff[j,2] - 2. * cd20 - cd21
	    temp[3] = 3. * cd20
	    temp[4] = cd21 - cd20

	    do i = 1, 4
		pcoeff[j,i] = temp[i]
	}
end


# II_PCPOLY5 -- Procedure to evaluate the polynomial coefficients
# of fifth order in x and y using Everetts formuala.

procedure ii_pcpoly5 (coeff, index, len_coeff, pcoeff, len_pcoeff)

real	coeff[ARB]		# 1D array of interpolant coeffcients
int	index			# pointer into coeff array
int	len_coeff		# row length of coeffcients
real	pcoeff[len_pcoeff,ARB]	# polynomial coefficients
int	len_pcoeff		# row length of pcoeff array

int	tptr
int	i, j
real	cd20, cd21, cd40, cd41, temp[6]

begin
	# determine polynomial coefficients in x
	tptr = index
	do i = 1, 6 {

	    # calculate the central differences
	    cd20 = 1./6. * (coeff[tptr+1] - 2. * coeff[tptr] + coeff[tptr-1])
	    cd21 = 1./6. * (coeff[tptr+2] - 2. * coeff[tptr+1] + coeff[tptr])
	    cd40 = 1./120. * (coeff[tptr-2] - 4. * coeff[tptr-1] +
		   6. * coeff[tptr] - 4. * coeff[tptr+1] +
		   coeff[tptr+2])
	    cd41 = 1./120. * (coeff[tptr-1] - 4. * coeff[tptr] +
		   6. * coeff[tptr+1] - 4. * coeff[tptr+2] +
		   coeff[tptr+3])

	    # calculate coefficients in x for each y
	    pcoeff[1,i] = coeff[tptr]
	    pcoeff[2,i] = coeff[tptr+1] - coeff[tptr] - 2. * cd20 - cd21 +
			  6. * cd40 + 4. * cd41 
	    pcoeff[3,i] = 3. * cd20 - 5. * cd40
	    pcoeff[4,i] = cd21 - cd20 - 5. * (cd40 + cd41)
	    pcoeff[5,i] = 5. * cd40
	    pcoeff[6,i] = cd41 - cd40

	    tptr = tptr + len_coeff
	}

	# calculate polynomial coefficients in y
	do j = 1, 6 {

	    # calculate the central differences
	    cd20 = 1./6. * (pcoeff[j,4] - 2. * pcoeff[j,3] + pcoeff[j,2])
	    cd21 = 1./6. * (pcoeff[j,5] - 2. * pcoeff[j,4] + pcoeff[j,3])
	    cd40 = 1./120. * (pcoeff[j,1] - 4. * pcoeff[j,2] +
	    	   6. * pcoeff[j,3] - 4. * pcoeff[j,4] + pcoeff[j,5])
	    cd41 = 1./120. * (pcoeff[j,2] - 4. * pcoeff[j,3] +
		   6. * pcoeff[j,4] - 4. * pcoeff[j,5] + pcoeff[j,6])

	    # calculate the final coefficients
	    temp[1] = pcoeff[j,3]
	    temp[2] = pcoeff[j,4] - pcoeff[j,3] - 2. * cd20 - cd21 +
		      6. * cd40 + 4. * cd41
	    temp[3] = 3. * cd20 - 5. * cd40
	    temp[4] = cd21 - cd20 - 5. * (cd40 + cd41)
	    temp[5] = 5. * cd40
	    temp[6] = cd41 - cd40

	    do i = 1, 6
		pcoeff[j,i] = temp[i]

	}

end


# II_PCSPLINE3 -- Procedure to evaluate the polynomial coefficients
# of bicubic spline.

procedure ii_pcspline3 (coeff, index, len_coeff, pcoeff, len_pcoeff)

real	coeff[ARB]		# 1D array of interpolant coeffcients
int	index			# pointer into coeff array
int	len_coeff		# row length of coeffcients
real	pcoeff[len_pcoeff,ARB]	# polynomial coefficients
int	len_pcoeff		# row length of pcoeff

int	tptr
int	i, j
real	temp[4]

begin
	# determine polynomial coefficients in x
	tptr = index
	do i = 1, 4 {

	    pcoeff[1,i] = coeff[tptr+1] + 4. * coeff[tptr] + coeff[tptr-1]
	    pcoeff[2,i] = 3. * (coeff[tptr+1] - coeff[tptr-1])
	    pcoeff[3,i] = 3. * (coeff[tptr-1] - 2. * coeff[tptr] +
	    		  coeff[tptr+1])
	    pcoeff[4,i] = -coeff[tptr-1] + 3. * coeff[tptr] -
	    		  3. * coeff[tptr+1] + coeff[tptr+2]
			    
	    tptr = tptr + len_coeff
	}

	# calculate polynomial coefficients in y
	do j = 1, 4 {

	    temp[1] = pcoeff[j,3] + 4. * pcoeff[j,2] + pcoeff[j,1]
	    temp[2] = 3. * (pcoeff[j,3] - pcoeff[j,1]) 
	    temp[3] = 3. * (pcoeff[j,1] - 2. * pcoeff[j,2] + pcoeff[j,3])
	    temp[4] = -pcoeff[j,1] + 3. * pcoeff[j,2] - 3. * pcoeff[j,3] +
	    	      pcoeff[j,4]

	    do i = 1, 4
		pcoeff[j,i] = temp[i]
	}
end

# II_BISINCDER -- Evaluate the derivatives of the 2D sinc interpolator. If the
# function value only is needed call ii_bisinc. This routine computes only
# the first 2 derivatives in x and y. The second derivative is computed
# even if only the first derivative is needed. The sinc truncation length
# is nsinc. The taper is a cosbell approximated by a quartic polynomial.
# The data value if returned if x is closer to x[i] than mindx and  y is
# closer to y[i]  than mindy.

procedure ii_bisincder (x, y, der, nxder, nyder, len_der, coeff, first_point,
	nxpix, nypix, nsinc, mindx, mindy)

real	x, y			# the input x and y values
real	der[len_der,ARB]	# the output derivatives array
int	nxder, nyder		# the number of derivatives to compute
int	len_der			# the width of the derivatives array
real	coeff[ARB]		# the coefficient array
int	first_point		# offset of first data point into the array
int	nxpix, nypix		# size of the coefficient array
int	nsinc			# the sinc truncation length
real	mindx, mindy		# the precision of the sinc interpolant

double	sumx, normx[3], normy[3], norm[3,3], sum[3,3]
int	i, j, k, jj, kk, xc, yc, nconv, index
int	minj, maxj, offj, mink, maxk, offk, last_point
pointer	sp, ac, ar
real	sconst, a2, a4, dx, dy, dxn, dyn, dx2, taper, sdx, ax, ay, ctanx, ctany
real	zx, zy
real	px[3], py[3]

begin
	# Return if no derivatives ar to be computed.
	if (nxder == 0 || nyder == 0)
	    return

	# Initialize the derivatives to zero. 
	do jj = 1, nyder {
	    do kk = 1, nxder 
		der[kk,jj] = 0.0
	}

	# Return if the data is outside range.
	xc = nint (x)
	yc = nint (y)
	if (xc < 1 || xc > nxpix || yc < 1 || yc > nypix)
	    return

	# Call ii_bsinc if only the function value is requested.
	if (nxder == 1 && nyder == 1) {
	    call ii_bisinc (coeff, first_point, nxpix, nypix, x, y, der[1,1],
		1, nsinc, mindx, mindy)
	    return
	}

	# Compute the constants for the cosine bell taper approximation.
	sconst = (HALFPI / nsinc) ** 2
	a2 = -0.49670
	a4 = 0.03705

	# Allocate some working space.
	nconv = 2 * nsinc + 1
	call smark (sp)
	call salloc (ac, 3 * nconv, TY_REAL)
	call salloc (ar, 3 * nconv, TY_REAL)
	call aclrr (Memr[ac], 3 * nconv)
	call aclrr (Memr[ar], 3 * nconv)

	# Initialize.
	dx = x - xc
	dy = y - yc
	if (dx == 0.0)
	    ctanx = 0.0
	else
	    ctanx = 1.0 / tan (PI * dx)
	if (dy == 0.0)
	    ctany = 0.0
	else
	    ctany = 1.0 / tan (PI * dy)
	index = - 1 - nsinc
	dxn = -1 - nsinc - dx
	dyn = -1 - nsinc - dy
	if (mod (nsinc, 2) == 0)
	    sdx = 1.0
	else
	    sdx = -1.0
	do jj = 1, 3 {
	    normy[jj] = 0.0d0
	    normx[jj] = 0.0d0
	}

	do i = 1, nconv {
	    dx2 = sconst * (i + index) ** 2 
	    taper = sdx * (1.0 + a2 * dx2 + a4 * dx2 * dx2) ** 2
	    #ax = dxn + i
	    #ay = dyn + i
	    ax = -dxn - i
	    ay = -dyn - i
	    if (ax == 0.0) {
		px[1] = 1.0
		px[2] = 0.0
		px[3] = - 1.0 / 3.0
	    } else if (dx == 0.0) {
		px[1] = 0.0
		px[2] = 0.0
		px[3] = 0.0
	    } else {
	        zx = 1.0 / ax
		px[1] = taper * zx 
		px[2] = px[1] * (ctanx - zx)
		px[3] = -px[1] * (1.0 + 2.0 * zx * (ctanx - zx)) 
	    }
	    if (ay == 0.0) {
		py[1] = 1.0
		py[2] = 0.0
		py[3] = - 1.0 / 3.0
	    } else if (dy == 0.0) {
		py[1] = 0.0
		py[2] = 0.0
		py[3] = 0.0
	    } else {
		zy = 1.0 / ay
		py[1] = taper * zy
		py[2] = py[1] * (ctany - zy)
		py[3] = -py[1] * (1.0 + 2.0 * zy * (ctany - zy)) 
	    }

	    Memr[ac+i-1] = px[1]
	    Memr[ac+nconv+i-1] = px[2]
	    Memr[ac+2*nconv+i-1] = px[3]
	    Memr[ar+i-1] = py[1]
	    Memr[ar+nconv+i-1] = py[2]
	    Memr[ar+2*nconv+i-1] = py[3]

	    do jj = 1, 3 {
		normx[jj] = normx[jj] + px[jj]
		normy[jj] = normy[jj] + py[jj]
	    }

	    sdx = -sdx
	}

	# Normalize.
	do jj = 1, 3 {
	    do kk = 1, 3
		norm[kk,jj] = normx[kk] * normy[jj]
	}


	# Do the convolution.
        minj = max (1, yc - nsinc)
        maxj = min (nypix, yc + nsinc)
        mink = max (1, xc - nsinc)
        maxk = min (nxpix, xc + nsinc)
	do jj = 1, nyder {
            offj = ar + (jj - 1) * nconv - yc + nsinc
	    do kk = 1, nxder {
                offk = ac + (kk - 1) * nconv - xc + nsinc
		sum[kk,jj] = 0.0d0

		# Do the convolutions.
                do j = yc - nsinc, minj - 1 {
                    sumx = 0.0d0
                    do k = xc - nsinc, mink - 1
                        sumx = sumx + Memr[k+offk] * coeff[first_point+1]
                    do k = mink, maxk
                        sumx = sumx + Memr[k+offk] * coeff[first_point+k]
                    do k = maxk + 1, xc + nsinc
                        sumx = sumx + Memr[k+offk] * coeff[first_point+nxpix]

                    sum[kk,jj] = sum[kk,jj] + Memr[j+offj] * sumx
                }


                do j = minj, maxj {
                    index = first_point + (j - 1) * nxpix
                    sumx = 0.0d0
                    do k = xc - nsinc, mink - 1
                        sumx = sumx + Memr[k+offk] * coeff[index+1]
                    do k = mink, maxk
                        sumx = sumx + Memr[k+offk] * coeff[index+k]
                    do k = maxk + 1, xc + nsinc
                        sumx = sumx + Memr[k+offk] * coeff[index+nxpix]

                    sum[kk,jj] = sum[kk,jj] + Memr[j+offj] * sumx
                }

                do j = maxj + 1, yc + nsinc {
                    last_point = first_point + (nypix - 1) * nxpix
                    sumx = 0.0d0
                    do k = xc - nsinc, mink - 1
                        sumx = sumx + Memr[k+offk] * coeff[last_point+1]
                    do k = mink, maxk
                        sumx = sumx + Memr[k+offk] * coeff[last_point+k]
                    do k = maxk + 1, xc + nsinc
                        sumx = sumx + Memr[k+offk] * coeff[last_point+nxpix]

                    sum[kk,jj] = sum[kk,jj] + Memr[j+offj] * sumx
                }

	    }
	}

	# Build the derivatives.
	der[1,1] = sum[1,1] / norm[1,1] 
	if (nxder > 1)
	    der[2,1] = sum[2,1] / norm[1,1] - (sum[1,1] * norm[2,1]) /
	               norm[1,1] ** 2 
	if (nxder > 2)
	    der[3,1] = sum[3,1] / norm[1,1] - (norm[3,1] * sum[1,1] +
	               2.0d0 * sum[2,1] * norm[2,1]) / norm[1,1] ** 2 +
		       2.0d0 * sum[1,1] * norm[2,1] * norm[2,1] / norm[1,1] ** 3
	if (nyder > 1) {
	    der[1,2] = sum[1,2] / norm[1,1] - (sum[1,1] * norm[1,2]) /
	               norm[1,1] ** 2 
	    if (nxder > 1)
		der[2,2] = sum[2,2] / norm[1,1] - (sum[2,1] * norm[1,2] +
		           sum[1,2] * norm[2,1] + norm[2,2] * sum[1,1]) /
			   norm[1,1] ** 2 + (2.0d0 * sum[1,1] * norm[2,1] *
			   norm[1,2]) / norm[1,1] ** 3
	    if (nxder > 2)
		der[3,2] = sum[3,2] / norm[1,1] - (sum[3,1] * norm[1,2] +
		           2.0 * norm[2,2] * sum[2,1] + 2.0 * sum[2,2] *
			   norm[2,1] + norm[3,1] * sum[1,2] + norm[3,2] *
			   sum[1,1]) / norm[1,1] ** 2 + (4.0 * norm[2,1] *
			   sum[2,1] * norm[1,2] + 2.0 * norm[2,1] * sum[1,2] *
			   norm[2,1] + 4.0 * norm[2,1] * norm[2,2] * sum[1,1] +
			   2.0 * norm[3,1] * norm[1,2] * sum[1,1]) /
			   norm[1,1] ** 3 - 6.0 * norm[2,1] * norm[2,1] *
			   norm[1,2] * sum[1,1] / norm[1,1] ** 4

	}
	if (nyder > 2) {
	    der[1,3] = sum[1,3] / norm[1,1] - (norm[1,3] * sum[1,1] +
	               2.0d0 * sum[1,2] * norm[1,2]) / norm[1,1] ** 2 +
		       2.0d0 * sum[1,1] * norm[1,2] * norm[1,2] / norm[1,1] ** 3
	    if (nxder > 1)
		der[2,3] = sum[2,3] / norm[1,1] - (sum[1,3] * norm[2,1] +
		           2.0 * norm[2,2] * sum[1,2] + 2.0 * sum[2,2] *
			   norm[1,2] + norm[1,3] * sum[2,1] + norm[2,3] *
			   sum[1,1]) / norm[1,1] ** 2 + (4.0 * norm[1,2] *
			   sum[1,2] * norm[2,1] + 2.0 * norm[1,2] * sum[2,1] *
			   norm[1,2] + 4.0 * norm[1,2] * norm[2,2] * sum[1,1] +
			   2.0 * norm[1,3] * norm[2,1] * sum[1,1]) /
			   norm[1,1] ** 3 - 6.0 * norm[1,2] * norm[1,2] *
			   norm[2,1] * sum[1,1] / norm[1,1] ** 4
	    if (nxder > 2)
		der[3,3] = sum[3,3] / norm[1,1] - (2.0 * sum[2,3] * norm[2,1] +
		           norm[3,1] * sum[1,3] + 2.0 * norm[3,2] * sum[1,2] +
			   4.0 * sum[2,2] * norm[2,2] + 2.0 * sum[3,2] *
			   norm[1,2] + 2.0 * norm[2,3] * sum[2,1] + sum[3,1] *
			   norm[1,3] + norm[3,3] * sum[1,1]) / norm[1,1] ** 2 +
			   (2.0 * norm[2,1] * norm[2,1] * sum[1,3] + 8.0 *
			   norm[2,1] * sum[1,2] * norm[2,2] + 8.0 * norm[2,1] *
			   norm[1,2] * sum[2,2] + 4.0 * norm[2,1] * sum[2,1] *
			   norm[1,3] + 4.0 * norm[2,1] * norm[2,3] * sum[1,1] +
			   4.0 * norm[1,2] * sum[1,2] * norm[3,1] + 8.0 *
			   norm[2,2] * sum[2,1] * norm[1,2] + 2.0 * norm[1,2] *
			   norm[1,2] * sum[3,1] + 4.0 * norm[2,2] * norm[2,2] *
			   sum[1,1] + 4.0 * norm[1,2] * norm[3,2] * sum[1,1] +
			   2.0 * norm[1,3] * norm[3,1] * sum[1,1]) /
			   norm[1,1] ** 3 - (12.0 * norm[2,1] * norm[2,1] *
			   norm[1,2] * sum[1,2] + 12.0 * norm[2,1] * norm[1,2] *
			   norm[1,2] * sum[2,1] + 24.0 * norm[2,1] * norm[1,2] *
			   norm[2,2] * sum[1,1] + 6.0 * norm[2,1] * norm[2,1] *
			   norm[1,3] * sum[1,1] + 6.0 * norm[1,2] * norm[1,2] *
			   norm[3,1] * sum[1,1]) / norm[1,1] ** 4 + ( 24.0 *
			   norm[1,2] * norm[1,2] * norm[2,1] * norm[2,1] *
			   sum[1,1]) / norm[1,1] ** 5
	}




	call sfree (sp)
end
