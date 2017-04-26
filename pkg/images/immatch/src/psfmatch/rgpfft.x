
# RG_PG10F -- Fetch the 0 component of the fft.

real procedure rg_pg10f (fft, nxfft, nyfft)

real	fft[nxfft,nyfft]	#I array containing 2 real ffts
int	nxfft			#I x dimension of complex array
int	nyfft			#I y dimension of complex array

int	xcen, ycen

begin
	xcen = nxfft / 2 + 1 
	ycen = nyfft / 2 + 1

	return (fft[xcen,ycen])
end


# RG_PG1NORM -- Estimate the normalization factor by computing the amplitude
# of the best fitting Gaussian. This routine may eventually be replaced by
# on which does a complete Gaussian fit. The Gaussian is assumed to be
# of the form g = a * exp (b * r * r). The input array is a 2D real array
# storing 1  fft of dimension nxfft by nyfft in complex order with the
# zero frequency in the center.

real procedure rg_pg1norm (fft, nxfft, nyfft)

real	fft[nxfft,nyfft]	#I array containing 2 real ffts
int	nxfft			#I x dimension of complex array
int	nyfft			#I y dimension of complex array

int	xcen, ycen
real	ln1, ln2, cx, cy

begin
	xcen = nxfft / 2 + 1 
	ycen = nyfft / 2 + 1

	if (nxfft >= 8) {
	    ln1 = log (sqrt (fft[xcen-2,ycen] ** 2 + fft[xcen-1,ycen] ** 2))
	    ln2 = log (sqrt (fft[xcen-4,ycen] ** 2 + fft[xcen-3,ycen] ** 2))
	    cx = exp ((4.0 * ln1 - ln2) / 3.0)
	} else
	    cx = 0.0

	if (nyfft >= 4) {
	    ln1 = log (sqrt (fft[xcen,ycen-1] ** 2 + fft[xcen+1,ycen-1] ** 2))
	    ln2 = log (sqrt (fft[xcen,ycen-2] ** 2 + fft[xcen+1,ycen-2] ** 2))
	    cy = exp ((4.0 * ln1 - ln2) / 3.0)
	} else
	    cy = 0.0

	if (cx <= 0.0)
	    return (cy)
	else if (cy <= 0.0)
	    return (cx)
	else
	    return (0.5 * (cx + cy))
end


# RG_PG20F -- Fetch the 0 component of the fft.

real procedure rg_pg20f (fft, nxfft, nyfft)

real	fft[nxfft,nyfft]	#I array containing 2 real ffts
int	nxfft			#I x dimension of complex array
int	nyfft			#I y dimension of complex array

int	xcen, ycen

begin
	xcen = nxfft / 2 + 1 
	ycen = nyfft / 2 + 1

	return (fft[xcen,ycen] / fft[xcen+1,ycen])
end


# RG_PG2NORM -- Estimate the normalization factor by computing the amplitude
# of the best fitting Gaussian. This routine may eventually be replaced by
# on which does a complete Gaussian fit. The Gaussian is assumed to be
# of the form g = a * exp (b * r * r). The input array is a 2D real array
# storing 2 2D ffts of dimension nxfft by nyfft in complex order with the
# zero frequency in the center.

real procedure rg_pg2norm (fft, nxfft, nyfft)

real	fft[nxfft,nyfft]	#I array containing 2 real ffts
int	nxfft			#I x dimension of complex array
int	nyfft			#I y dimension of complex array

int	xcen, ycen
real	fftr, ffti, ln1r, ln2r, ln1i, ln2i, cxr, cyr, cxi, cyi, ampr, ampi

begin

	xcen = nxfft / 2 + 1 
	ycen = nyfft / 2 + 1

	# Compute the x amplitude for the first fft.
	if (nxfft >= 8) {

	    fftr = 0.5 * (fft[xcen+2,ycen] + fft[xcen-2,ycen])
	    ffti = 0.5 * (fft[xcen+3,ycen] - fft[xcen-1,ycen])
	    ln1r = log (sqrt (fftr ** 2 + ffti ** 2))
	    fftr = 0.5 * (fft[xcen+4,ycen] + fft[xcen-4,ycen])
	    ffti = 0.5 * (fft[xcen+5,ycen] - fft[xcen-3,ycen])
	    ln2r = log (sqrt (fftr ** 2 + ffti ** 2))

	    fftr = 0.5 * (fft[xcen+3,ycen] + fft[xcen-1,ycen])
	    ffti = -0.5 * (fft[xcen+2,ycen] - fft[xcen-2,ycen])
	    ln1i = log (sqrt (fftr ** 2 + ffti ** 2))
	    fftr = 0.5 * (fft[xcen+5,ycen] + fft[xcen-3,ycen])
	    ffti = -0.5 * (fft[xcen+4,ycen] - fft[xcen-4,ycen])
	    ln2i = log (sqrt (fftr ** 2 + ffti ** 2))

	    cxr = exp ((4.0 * ln1r - ln2r) / 3.0)
            cxi = exp ((4.0 * ln1i - ln2i) / 3.0)

	} else {

	    cxr = 0.0
	    cxi = 0.0

	}

	# Compute the y ratio.
	if (nyfft >= 4) {

	    fftr = 0.5 * (fft[xcen,ycen+1] + fft[xcen,ycen-1])
	    ffti = 0.5 * (fft[xcen+1,ycen+1] - fft[xcen+1,ycen-1])
	    ln1r = log (sqrt (fftr ** 2 + ffti ** 2))
	    fftr = 0.5 * (fft[xcen,ycen+2] + fft[xcen,ycen-2])
	    ffti = 0.5 * (fft[xcen+1,ycen+2] - fft[xcen+1,ycen-2])
	    ln2r = log (sqrt (fftr ** 2 + ffti ** 2))

	    fftr = 0.5 * (fft[xcen+1,ycen+1] + fft[xcen+1,ycen-1])
	    ffti = -0.5 * (fft[xcen,ycen+1] - fft[xcen,ycen-1])
	    ln1i = log (sqrt (fftr ** 2 + ffti ** 2))
	    fftr = 0.5 * (fft[xcen+1,ycen+2] + fft[xcen+1,ycen-2])
	    ffti = -0.5 * (fft[xcen,ycen+2] - fft[xcen,ycen-2])
	    ln2i = log (sqrt (fftr ** 2 + ffti ** 2))

	    cyr = exp ((4.0 * ln1r - ln2r) / 3.0)
            cyi = exp ((4.0 * ln1i - ln2i) / 3.0)

	} else {

	    cyr = 0.0
	    cyi = 0.0

	}

	if (cxr <= 0.0)
	    ampr = cyr
	else if (cyr <= 0.0)
	    ampr = cxr
	else
	    ampr = 0.5 * (cxr + cyr)

	if (cxi <= 0.0)
	    ampi = cyi
	else if (cyi <= 0.0)
	    ampi = cxi
	else
	    ampi = 0.5 * (cxi + cyi)

	if (ampi <= 0.0)
	    return (INDEFR)
	else
	    return (ampr /ampi)
end


# RG_PDIVFFT -- Unpack the two fft's, save the first fft, and compute the
# quotient of the two ffts.

procedure rg_pdivfft (fft1, fftnum, fftdenom, fft2, nxfft, nyfft)

real    fft1[nxfft,nyfft]       # array containing 2 ffts of 2 real functions
real    fftnum[nxfft,nyfft]     # the numerator fft
real    fftdenom[nxfft,nyfft]   # the denominator fft
real    fft2[nxfft,nyfft]       # fft of psf matching function
int     nxfft, nyfft            # dimensions of fft

int	i, j, xcen, ycen, nxp2, nxp3, nyp2
real    c1, c2, h1r, h1i, h2r, h2i, denom

begin
        c1 = 0.5
        c2 = -0.5
	xcen = nxfft / 2 + 1
	ycen = nyfft / 2 + 1
        nxp2 = nxfft + 2
        nxp3 = nxfft + 3
        nyp2 = nyfft + 2

        # Compute the 0 frequency point.
        h1r = fft1[xcen,ycen]
        h1i = 0.0
        h2r = fft1[xcen+1,ycen]
        h2i = 0.0
	fftnum[xcen,ycen] = h1r
	fftnum[xcen+1,ycen] = 0.0
        fftdenom[xcen,ycen] = h2r
        fftdenom[xcen+1,ycen] = 0.0
        fft2[xcen,ycen] = h1r / h2r
        fft2[xcen+1,ycen] = 0.0

	#call eprintf ("fft11=%g fft21=%g\n")
	    #call pargr (fft1[1,1])
	    #call pargr (fft1[2,1])

	# Compute the first point.
        h1r = c1 * (fft1[1,1] + fft1[1,1])
        h1i = 0.0
        h2r = -c2 * (fft1[2,1] + fft1[2,1])
        h2i = 0.0

        fftnum[1,1] = h1r
        fftnum[2,1] = h1i
        fftdenom[1,1] = h2r
        fftdenom[2,1] = h2i
        denom = h2r * h2r + h2i * h2i
        if (denom == 0.0) {
            fft2[1,1] = 1.0
            fft2[2,1] = 0.0
        } else {
            fft2[1,1] = (h1r * h2r + h1i * h2i) / denom
            fft2[2,1] = (h1i * h2r - h2i * h1r) / denom
        }

	# Compute the x symmetry axis points.
        do i = 3, xcen - 1, 2 {

            h1r = c1 * (fft1[i,ycen] + fft1[nxp2-i,ycen])
            h1i = c1 * (fft1[i+1,ycen] - fft1[nxp3-i,ycen])
            h2r = -c2 * (fft1[i+1,ycen] + fft1[nxp3-i,ycen])
            h2i = c2 * (fft1[i,ycen] - fft1[nxp2-i,ycen])

            fftnum[i,ycen] = h1r
            fftnum[i+1,ycen] = h1i
            fftnum[nxp2-i,ycen] = h1r
            fftnum[nxp3-i,ycen] = -h1i

            fftdenom[i,ycen] = h2r
            fftdenom[i+1,ycen] = h2i
            fftdenom[nxp2-i,ycen] = h2r
            fftdenom[nxp3-i,ycen] = -h2i

            denom = h2r * h2r + h2i * h2i
            if (denom == 0.0) {
                fft2[i,ycen] = 1.0
                fft2[i+1,ycen] = 0.0
            } else {
                fft2[i,ycen] = (h1r * h2r + h1i * h2i) / denom
                fft2[i+1,ycen] = (h1i * h2r - h2i * h1r) / denom
            }
            fft2[nxp2-i,ycen] = fft2[i,ycen]
            fft2[nxp3-i,ycen] = -fft2[i+1,ycen]

        }

	# Quit if the transform is 1D.
	if (nyfft < 2)
	    return

	# Compute the x axis points.
        do i = 3, xcen + 1, 2 {

            h1r = c1 * (fft1[i,1] + fft1[nxp2-i,1])
            h1i = c1 * (fft1[i+1,1] - fft1[nxp3-i,1])
            h2r = -c2 * (fft1[i+1,1] + fft1[nxp3-i,1])
            h2i = c2 * (fft1[i,1] - fft1[nxp2-i,1])

            fftnum[i,1] = h1r
            fftnum[i+1,1] = h1i
            fftnum[nxp2-i,1] = h1r
            fftnum[nxp3-i,1] = -h1i

            fftdenom[i,1] = h2r
            fftdenom[i+1,1] = h2i
            fftdenom[nxp2-i,1] = h2r
            fftdenom[nxp3-i,1] = -h2i

            denom = h2r * h2r + h2i * h2i
            if (denom == 0) {
                fft2[i,1] = 1.0
                fft2[i+1,1] = 0.0
            } else {
                fft2[i,1] = (h1r * h2r + h1i * h2i) / denom
                fft2[i+1,1] = (h1i * h2r - h2i * h1r) / denom
            }
            fft2[nxp2-i,1] = fft2[i,1]
            fft2[nxp3-i,1] = -fft2[i+1,1]
	}

	# Compute the y symmetry axis points.
        do i = 2, ycen - 1 {

            h1r = c1 * (fft1[xcen,i] + fft1[xcen, nyp2-i])
            h1i = c1 * (fft1[xcen+1,i] - fft1[xcen+1,nyp2-i])
            h2r = -c2 * (fft1[xcen+1,i] + fft1[xcen+1,nyp2-i])
            h2i = c2 * (fft1[xcen,i] - fft1[xcen,nyp2-i])

            fftnum[xcen,i] = h1r
            fftnum[xcen+1,i] = h1i
            fftnum[xcen,nyp2-i] = h1r
            fftnum[xcen+1,nyp2-i] = -h1i

            fftdenom[xcen,i] = h2r
            fftdenom[xcen+1,i] = h2i
            fftdenom[xcen,nyp2-i] = h2r
            fftdenom[xcen+1,nyp2-i] = -h2i

            denom = h2r * h2r + h2i * h2i
            if (denom == 0.0) {
                fft2[xcen,i] = 1.0
                fft2[xcen+1,i] = 0.0
            } else {
                fft2[xcen,i] = (h1r * h2r + h1i * h2i) / denom
                fft2[xcen+1,i] = (h1i * h2r - h2i * h1r) / denom
            }
            fft2[xcen,nyp2-i] = fft2[xcen,i]
            fft2[xcen+1,nyp2-i] = -fft2[xcen+1,i]

        }

	# Compute the y axis points.
        do i = 2, ycen {

            h1r = c1 * (fft1[1,i] + fft1[1,nyp2-i])
            h1i = c1 * (fft1[2,i] - fft1[2,nyp2-i])
            h2r = -c2 * (fft1[2,i] + fft1[2,nyp2-i])
            h2i = c2 * (fft1[1,i] - fft1[1,nyp2-i])

            fftnum[1,i] = h1r
            fftnum[2,i] = h1i
            fftnum[1,nyp2-i] = h1r
            fftnum[2,nyp2-i] = -h1i

            fftdenom[1,i] = h2r
            fftdenom[2,i] = h2i
            fftdenom[1,nyp2-i] = h2r
            fftdenom[2,nyp2-i] = -h2i

            denom = h2r * h2r + h2i * h2i
            if (denom == 0.0) {
                fft2[1,i] = 1.0
                fft2[2,i] = 0.0
            } else {
                fft2[1,i] = (h1r * h2r + h1i * h2i) / denom
                fft2[2,i] = (h1i * h2r - h2i * h1r) / denom
            }
            fft2[1,nyp2-i] = fft2[1,i]
            fft2[2,nyp2-i] = -fft2[2,i]
	}

        # Compute the remainder of the transform.
        do j = 2, ycen - 1 {

	    do i = 3, xcen - 1, 2 {

                h1r = c1 * (fft1[i,j] + fft1[nxp2-i, nyp2-j])
                h1i = c1 * (fft1[i+1,j] - fft1[nxp3-i,nyp2-j])
                h2r = -c2 * (fft1[i+1,j] + fft1[nxp3-i,nyp2-j])
                h2i = c2 * (fft1[i,j] - fft1[nxp2-i,nyp2-j])

                fftnum[i,j] = h1r
                fftnum[i+1,j] = h1i
                fftnum[nxp2-i,nyp2-j] = h1r
                fftnum[nxp3-i,nyp2-j] = -h1i

                fftdenom[i,j] = h2r
                fftdenom[i+1,j] = h2i
                fftdenom[nxp2-i,nyp2-j] = h2r
                fftdenom[nxp3-i,nyp2-j] = -h2i

                denom = h2r * h2r + h2i * h2i
                if (denom == 0.0) {
                    fft2[i,j] = 1.0
                    fft2[i+1,j] = 0.0
                } else {
                    fft2[i,j] = (h1r * h2r + h1i * h2i) / denom
                    fft2[i+1,j] = (h1i * h2r - h2i * h1r) / denom
                }
                fft2[nxp2-i,nyp2-j] = fft2[i,j]
                fft2[nxp3-i,nyp2-j] = - fft2[i+1,j]
	    }

            do i = xcen + 2, nxfft, 2 {

                h1r = c1 * (fft1[i,j] + fft1[nxp2-i, nyp2-j])
                h1i = c1 * (fft1[i+1,j] - fft1[nxp3-i,nyp2-j])
                h2r = -c2 * (fft1[i+1,j] + fft1[nxp3-i,nyp2-j])
                h2i = c2 * (fft1[i,j] - fft1[nxp2-i,nyp2-j])

                fftnum[i,j] = h1r
                fftnum[i+1,j] = h1i
                fftnum[nxp2-i,nyp2-j] = h1r
                fftnum[nxp3-i,nyp2-j] = -h1i

                fftdenom[i,j] = h2r
                fftdenom[i+1,j] = h2i
                fftdenom[nxp2-i,nyp2-j] = h2r
                fftdenom[nxp3-i,nyp2-j] = -h2i

                denom = h2r * h2r + h2i * h2i
                if (denom == 0.0) {
                    fft2[i,j] = 1.0
                    fft2[i+1,j] = 0.0
                } else {
                    fft2[i,j] = (h1r * h2r + h1i * h2i) / denom
                    fft2[i+1,j] = (h1i * h2r - h2i * h1r) / denom
                }
                fft2[nxp2-i,nyp2-j] = fft2[i,j]
                fft2[nxp3-i,nyp2-j] = - fft2[i+1,j]

            }
        }
end


# RG_PNORM -- Insert the normalization value into the 0 frequency of the
# fft. The fft is a 2D fft stored in a real array in complex order.
# The fft is assumed to be centered.

procedure rg_pnorm (fft, nxfft, nyfft, norm)

real    fft[ARB]        #I the input fft
int     nxfft           #I the x dimension of fft (complex storage)
int     nyfft           #I the y dimension of the fft
real    norm            #I the flux ratio

int     index

begin
        index = nxfft + 1 + 2 * (nyfft / 2) * nxfft
        fft[index] = norm
        fft[index+1] = 0.0
end
