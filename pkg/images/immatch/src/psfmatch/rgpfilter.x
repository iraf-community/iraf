include <math.h>

# RG_PCOSBELL -- Apply a cosine bell function to the data.

procedure rg_pcosbell (fft, nxfft, nyfft, sx1, sx2, sy1, sy2, radsym)

real	fft[ARB]		#I/O the ifft to be filtered
int	nxfft			#I the x dimension of the fft
int	nyfft			#I the y dimension of the fft
real	sx1			#I inner x radius of the cosine bell filter
real	sx2			#I outer x radius of the cosine bell filter
real	sy1			#I inner y radius of the cosine bell filter
real	sy2			#I outer y radius of the cosine bell filter
int	radsym			#I radial symmetry ?

int	i, j, index, xcen, ycen
real	factorx, factory, r1, r2, r, rj, cos2

begin
	# Compute the center of the fft.
	xcen = (nxfft / 2) + 1
	ycen = (nyfft / 2) + 1

	if (radsym == NO) {

	    # Filter in the y direction independently.
	    if (IS_INDEFR(sy1)) 
		r1 = 0.0
	    else
		r1 = sy1
	    if (IS_INDEFR(sy2)) 
		r2 = nyfft - ycen + 1
	    else
		r2 = sy2
	    factory = HALFPI / (r2 - r1)
	    index = 1
	    do j = 1, nyfft {
		r = abs (ycen - j)
		if (r >= r2)
		    cos2 = 0.0
		else if (r <= r1)
		    cos2 = 1.0
		else
		    cos2 = cos ((r - r1) * factory) ** 2
		call amulkr (fft[index], cos2, fft[index], 2 * nxfft)
		index = index + 2 * nxfft
	    }

	    # Filter in the x direction independently.
	    if (IS_INDEFR(sx1)) 
		r1 = 0.0
	    else
		r1 = sx1
	    if (IS_INDEFR(sx2)) 
		r2 = nxfft - xcen + 1
	    else
		r2 = sx2
	    factorx = HALFPI / (r2 - r1)

	    do i = 1, nxfft {
		r = abs (xcen - i)
		if (r >= r2)
		    cos2 = 0.0
		else if (r <= r1)
		    cos2 = 1.0
		else
		    cos2 = cos ((r - r1) * factorx) ** 2
		do j = 2 * i - 1, 2 * nxfft * nyfft, 2 * nxfft {
		    fft[j] = fft[j] * cos2
		    fft[j+1] = fft[j+1] * cos2
		}
	    }

	} else {

	    if (IS_INDEFR(sx1) && IS_INDEFR(sy1))
		r1 = 0.0
	    else if (IS_INDEFR(sx1))
		r1 = sy1
	    else if (IS_INDEFR(sy1))
		r1 = sx1
	    else
		r1 = (sx1 + sy1) / 2.0
	    if (IS_INDEFR(sx2) && IS_INDEFR(sy2))
		r2 = (nxfft - xcen + 1 + nyfft - ycen + 1) / 2.0
	    else if (IS_INDEFR(sx2))
		r2 = sy2
	    else if (IS_INDEFR(sy2))
		r2 = sx2
	    else
		r2 = (sx2 + sy2) / 2.0
	    factorx = HALFPI / (r2 - r1)

	    index = 0
	    do j = 1, nyfft {
		rj = (ycen - j) ** 2
		do i = 1, nxfft  {
		    r = sqrt ((i - xcen) ** 2 + rj)
		    if (r >= r2) {
			fft[index+2*i-1] = 0.0
			fft[index+2*i] = 0.0
		    } else if (r > r1) {
			fft[index+2*i-1] = fft[index+2*i-1] * cos ((r - r1) *
			    factorx) ** 2
			fft[index+2*i] = fft[index+2*i] * cos ((r - r1) *
			    factorx) ** 2
		    }
		}
		index = index + 2 * nxfft
	    }
	}
end


# RG_PREPLACE -- Replace low valued regions in the kernel fft with a Gaussian
# extension.

procedure rg_preplace (fft, fftdiv, nxfft, nyfft, pthreshold, norm)

real	fft[ARB]		#I/O the fft of the kernel
real	fftdiv[ARB]		#I the divisor fft
int	nxfft			#I x dimension of the fft (complex storage)
int	nyfft			#I y dimension of the fft
real	pthreshold		#I the minimum percent amplitude in the divisor
real	norm			#I the normalization  value

pointer	sp, params
int	xcen, ycen, i, j, ri, rj, index
real	divpeak, a1, a2, a3, u, v, divisor, absv, phi

begin
	call smark (sp)
	call salloc (params, 5, TY_REAL)

	# Compute the central amplitude peak.
	xcen = nxfft / 2 + 1
	ycen = nyfft / 2 + 1
	divpeak = pthreshold * fftdiv[1+nxfft+2*(ycen-1)*nxfft]

	# Fit the parameters.
	call rg_pgaussfit (fft, fftdiv, nxfft, nyfft, divpeak, norm,
	    Memr[params])

	# Store the parameters in temporary variables.
	a1 = Memr[params]
	a2 = Memr[params+1]
	a3 = Memr[params+2]
	u = Memr[params+3]
	v = Memr[params+4]

	# Perform the extension.
	index = 0
	do j = 1, nyfft {
	    rj = j - ycen
	    do i = 1, nxfft {
	        ri = i - xcen
		divisor = sqrt (fftdiv[index+2*i-1] ** 2 +
		    fftdiv[index+2*i] ** 2)
		if (divisor < divpeak) {
		    absv = norm * exp (a1 * ri * ri + a2 * ri * rj + a3 *
		        rj * rj)
		    phi = u * ri + v * rj
		    fft[index+2*i-1] = absv * cos (phi)
		    fft[index+2*i] = absv * sin (phi)
		}
	    }
	    index = index + 2 * nxfft
	}

	# Correct the first row.
	do i = 1, 2 * nxfft, 2 {
	    fft[i] = sqrt (fft[i] ** 2 + fft[i+1] ** 2)
	    fft[i+1] = 0.0
	}

	# Correct the first column.
	index = 1
	do j = 2, nyfft {
	    fft[index] = sqrt (fft[index] ** 2 + fft[index+1] ** 2)
	    fft[index+1] = 0.0
	    index = index + 2 * nxfft
	}

	call sfree (sp)
end


# RG_PGMODEL -- Replace low values with a Gaussian mode.

procedure rg_pgmodel (fft, fftdiv, nxfft, nyfft, pthreshold, norm)

real	fft[ARB]		#I/O the fft of the kernel
real	fftdiv[ARB]		#I the divisor fft
int	nxfft			#I the x dimension of the fft
int	nyfft			#I the y dimension of the fft
real	pthreshold		#I the minimum percent amplitude in the divisor
real	norm			#I the normalization factor

pointer	sp, params
int	xcen, ycen, i, j, index
real	divpeak, a1, a2, a3, u, v, absv, phi, ri, rj

begin
	call smark (sp)
	call salloc (params, 5, TY_REAL)

	# Compute the central amplitude peak.
	xcen = nxfft / 2 + 1
	ycen = nyfft / 2 + 1
	divpeak = pthreshold * fftdiv[1+nxfft+2*(ycen-1)*nxfft]

	# Fit the parameters.
	call rg_pgaussfit (fft, fftdiv, nxfft, nyfft, divpeak, norm,
	    Memr[params])

	# Store the parameters in temporary variables
	a1 = Memr[params]
	a2 = Memr[params+1]
	a3 = Memr[params+2]
	u = Memr[params+3]
	v = Memr[params+4]

	# Perform the extension.
	index = 0
	do j = 1, nyfft {
	    rj = j - ycen
	    do i = 1, nxfft {
	        ri = i - xcen
		absv = norm * exp (a1 * ri * ri + a2 * ri * rj + a3 * rj * rj)
		phi = u * ri + v * rj
		fft[index+2*i-1] = absv * cos (phi)
		fft[index+2*i] = absv * sin (phi)
	    }
	    index = index + 2 * nxfft
	}

	# Correct the first row.
	do i = 1, 2 * nxfft, 2 {
	    fft[i] = sqrt (fft[i] ** 2 + fft[i+1] ** 2)
	    fft[i+1] = 0.0
	}

	# Correct the first column.
	index = 1
	do j = 2, nyfft {
	    fft[index] = sqrt (fft[index] ** 2 + fft[index+1] ** 2)
	    fft[index+1] = 0.0
	    index = index + 2 * nxfft
	}

	call sfree (sp)
end


# RG_PGAUSSFIT -- Procedure to compute the Gaussian parameters

procedure rg_pgaussfit (fft, fftdiv, nxfft, nyfft, divpeak, norm, param)

real	fft[ARB]		#I the fft of the kernel
real	fftdiv[ARB]		#I the divisor fft
int	nxfft			#I the x dimension of the fft
int	nyfft			#I the y dimension of the fft
real	divpeak			#I the minimum value in the divisor
real	norm			#I the normalization value norm value
real	param[ARB]		#O the output fitted parameters

int	i, j, yj, xcen, ycen
double	x, y, x2, xy, y2, z, wt, x2w, y2w, xyw, zw, xzw, yzw
double	sxxxx, sxxxy, sxxyy, sxyyy, syyyy, sxxz, sxyz, syyz, sxx, sxy
double	syy, sxz, syz
pointer	sp, mat
real	divisor

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (mat, 12, TY_DOUBLE)

	# Define the center of the fft.
	xcen = nxfft / 2 + 1
	ycen = nyfft / 2 + 1

	# Initialize.
	sxxxx = 0.0d0
	sxxxy = 0.0d0
	sxxyy = 0.0d0
	sxyyy = 0.0d0
	syyyy = 0.0d0
	sxxz = 0.0d0
	sxyz = 0.0d0
	syyz = 0.0d0
	sxx = 0.0d0
	sxy = 0.0d0
	syy = 0.0d0
	sxz = 0.0d0
	syz = 0.0d0

	do i = 1, nxfft {
	    x = i - xcen
	    yj = - ycen
	    do j = 2 * i - 1, 2 * nxfft * nyfft, 2 * nxfft {
		yj = yj + 1
		y = yj

		# Skip low points in the fit.
		divisor = sqrt (fftdiv[j] ** 2 + fftdiv[j+1] ** 2)
		if (divisor < divpeak)
		    next
		if (i == xcen || yj == ycen)
		    next

		# Accumulate the intermediate products.
		divisor = sqrt (fft[j] ** 2 + fft[j+1] ** 2)
		if (divisor <= 0.0)
		    next
		z = log (divisor / norm)
		x2 = x * x
		y2 = y * y
		wt = 1.0 / sqrt (x2 + y2)
		xy = x * y
		x2w = x2 * wt
		y2w = y2 * wt
		xyw = xy * wt
		zw = z * wt
		xzw = x * zw
		yzw = y * zw

		# Accumulate the sums for the Gaussian.
		sxxxx = sxxxx + x2 * x2w
		sxxxy = sxxxy + x2 * xyw
		sxxyy = sxxyy + x2 * y2w
		sxyyy = sxyyy + xy * y2w
		syyyy = syyyy + y2 * y2w
		sxxz =  sxxz + x * xzw
		sxyz = sxyz + x * yzw
		syyz = syyz + y * yzw

		# New weight and z point.
		wt = sqrt (fft[j] ** 2 + fft[j+1] ** 2) / norm
		z = atan2 (fft[j+1], fft[j])

		# Accumulate the sums for the shift determinantion.
		sxx = sxx + x2 * wt
		sxy = sxy + xy * wt
		syy = syy + y2 * wt
		sxz = sxz + x * z * wt
		syz = syz + y * z * wt
	    }
	}

	# Solve for the gaussian.
	Memd[mat] = sxxxx
	Memd[mat+1] = sxxxy
	Memd[mat+2] = sxxyy
	Memd[mat+3] = sxxz
	Memd[mat+4] = sxxxy
	Memd[mat+5] = sxxyy
	Memd[mat+6] = sxyyy
	Memd[mat+7] = sxyz
	Memd[mat+8] = sxxyy
	Memd[mat+9] = sxyyy
	Memd[mat+10] = syyyy
	Memd[mat+11] = syyz
	call rg_pgelim (Memd[mat], 3)
	param[1] = Memd[mat+3]
	param[2] = Memd[mat+7]
	param[3] = Memd[mat+11]

	# Solve for the shift.
	Memd[mat] = sxx
	Memd[mat+1] = sxy
	Memd[mat+2] = sxz
	Memd[mat+3] = sxy
	Memd[mat+4] = syy
	Memd[mat+5] = syz
	call rg_pgelim (Memd[mat], 2)
	param[4] = Memd[mat+2]
	param[5] = Memd[mat+5]

	call sfree (sp)
end


# RG_PGELIM -- Solve a matrix using Gaussian elimination.

procedure rg_pgelim (a, n)

double  a[n+1,n]                        #I/O matrix to be solved
int     n                               #I number of variables

int     i, j, k
double	den, hold

begin
        do k = 1, n {

            den = a[k,k]
            if (den == 0.0d0) {                # look for non-zero switch
                do j = k + 1, n {
                    if (a[k,k] != 0.0d0) {
                        do i = k, n + 1 {
                            hold = a[i,j]
                            a[i,j] = a[i,k]
                            a[i,k] = hold
                        }
                        den = a[k,k]
                    }
                }
                if (den == 0.0d0)              # if still zero, skip
                    next
            }

            do i = k, n + 1 
                a[i,k] = a[i,k] / den
            do j = 1, n {
                if (j != k) {
                    den = a[k,j]
                    do i = k, n + 1
                       a[i,j] = a[i,j] - a[i,k] * den
                }
	    }
        }
end


# RG_PNORMFILT -- Filter out any values greater than the normalization
# from the kernel fft.

procedure rg_pnormfilt (fft, nxfft, nyfft, norm)

real	fft[ARB]		#I/O the input fft
int	nxfft			#I the x length of the fft
int	nyfft			#I the y length of the fft
real	norm			#I the normalization factor

int	j, i_index

begin
	do j = 1, nyfft {
	    i_index = 1 + 2 * (j - 1) * nxfft
	    call rg_pnreplace (fft[i_index], nxfft, norm)
	}
end


# RG_PFOURIER -- Compute the fourier spectrum of the convolution kernel.

procedure rg_pfourier (fft, psfft, nxfft, nyfft)

real	fft[ARB]		# the input fft
real	psfft[ARB]		# fourier spectrum of the fft
int	nxfft			# the x dimension of the fft
int	nyfft			# the y dimension of the fft

int	j, i_index, o_index

begin
	do j = 1, nyfft {
	    i_index = 1 + 2 * (j - 1) * nxfft
	    o_index = 1 + (j - 1) *  nxfft
	    call rg_pvfourier (fft[i_index], psfft[o_index], nxfft)
	}
end


# RG_PVFOURIER -- Procedure to compute the fourier spectrum of a  vector.

procedure rg_pvfourier (a, b, nxfft)

real	a[ARB]		# input vector in complex storage order
real	b[ARB]		# output vector in real storage order
int	nxfft		# length of vector

int	i

begin
	do i = 1, nxfft
	    b[i] = sqrt (a[2*i-1] ** 2 + a[2*i] ** 2)
end


# RG_PNREPLACE -- Replace values whose absolute value is greater than the
# flux ratio.

procedure rg_pnreplace (a, nxfft, norm)

real	a[ARB]		#I/O ithe nput vector in complex storage order
int	nxfft		#I the length of the vector
real	norm		#I the flux ratio

int	i
real	val

begin
	do i = 1, 2 * nxfft, 2 {
	    val = sqrt (a[i] ** 2 + a[i+1] ** 2)
	    if (val > norm) {
		a[i] = a[i] / val * norm 
		a[i+1] = a[i+1] / val * norm 
	    }
	}
end
