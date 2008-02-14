# RG_FFTCOR -- Compute the FFT of the reference and image data, take their
# product,  and compute the inverse transform to get the cross-correlation
# function. The reference and input image are loaded into alternate memory
# locations.

procedure rg_fftcor (fft, nxfft nyfft)

real	fft[ARB]	#I/O array to be  fft'd
int	nxfft, nyfft	#I dimensions of the fft

pointer	sp, dim

begin
	call smark (sp)
	call salloc (dim, 2, TY_INT)

	# Fourier transform the two arrays.
	Memi[dim] = nxfft
	Memi[dim+1] = nyfft
	if (Memi[dim+1] == 1)
	    call rg_fourn (fft, Memi[dim], 1, 1)
	else
	    call rg_fourn (fft, Memi[dim], 2, 1)

	# Compute the product of the two transforms.
	call rg_mulfft (fft, fft, 2 * nxfft, nyfft)

	# Shift the array to center the transform.
	call rg_fshift (fft, fft, 2 * nxfft, nyfft)

	# Normalize the transform.
	call adivkr (fft, real (nxfft * nyfft), fft, 2 * nxfft * nyfft)

	# Compute the inverse transform.
	if (Memi[dim+1] == 1)
	    call rg_fourn (fft, Memi[dim], 1, -1)
	else
	    call rg_fourn (fft, Memi[dim], 2, -1)

	call sfree (sp)
end


# RG_MULFFT -- Unpack the two individual ffts and compute their product.

procedure rg_mulfft (fft1, fft2, nxfft, nyfft)

real	fft1[nxfft,nyfft]	#I array containing 2 ffts of 2 real functions
real	fft2[nxfft,nyfft]	#O fft of correlation function
int	nxfft, nyfft		#I dimensions of fft

int	i,j, nxd2p2, nxp2, nxp3, nyd2p1, nyp2
real	c1, c2, h1r, h1i, h2r, h2i

begin
	c1 = 0.5
	c2 = -0.5

	nxd2p2 = nxfft / 2 + 2
	nxp2 = nxfft + 2
	nxp3 = nxfft + 3
	nyd2p1 = nyfft / 2 + 1
	nyp2 = nyfft + 2

	# Compute the 0 frequency point.
	h1r = fft1[1,1]
	h1i = 0.0
	h2r = fft1[2,1]
	h2i = 0.0
	fft2[1,1] = h1r * h2r
	fft2[2,1] = 0.0

	# Compute the x axis points.
	do i = 3, nxd2p2, 2 {
	    h2r = c1 * (fft1[i,1] + fft1[nxp2-i,1])
	    h2i = c1 * (fft1[i+1,1] - fft1[nxp3-i,1])
	    h1r = -c2 * (fft1[i+1,1] + fft1[nxp3-i,1])
	    h1i = c2 * (fft1[i,1] - fft1[nxp2-i,1])
	    fft2[i,1] = (h1r * h2r + h1i * h2i)
	    fft2[i+1,1] = (h1i * h2r - h2i * h1r)
	    fft2[nxp2-i,1] = fft2[i,1]
	    fft2[nxp3-i,1] = - fft2[i+1,1]
	}

	# Quit if the transform is 1D.
	if (nyfft < 2)
	    return

	# Compute the y axis points.
	do i = 2, nyd2p1 {
	    h2r = c1 * (fft1[1,i] + fft1[1, nyp2-i])
	    h2i = c1 * (fft1[2,i] - fft1[2,nyp2-i])
	    h1r = -c2 * (fft1[2,i] + fft1[2,nyp2-i])
	    h1i = c2 * (fft1[1,i] - fft1[1,nyp2-i])
	    fft2[1,i] = (h1r * h2r + h1i * h2i)
	    fft2[2,i] = (h1i * h2r - h2i * h1r)
	    fft2[1,nyp2-i] = fft2[1,i]
	    fft2[2,nyp2-i] = - fft2[2,i]
	}

	# Compute along the axis of symmetry.
	do i = 3, nxd2p2, 2 {
	    h2r = c1 * (fft1[i,nyd2p1] + fft1[nxp2-i, nyd2p1])
	    h2i = c1 * (fft1[i+1,nyd2p1] - fft1[nxp3-i,nyd2p1])
	    h1r = -c2 * (fft1[i+1,nyd2p1] + fft1[nxp3-i,nyd2p1])
	    h1i = c2 * (fft1[i,nyd2p1] - fft1[nxp2-i,nyd2p1])
	    fft2[i,nyd2p1] = (h1r * h2r + h1i * h2i)
	    fft2[i+1,nyd2p1] = (h1i * h2r - h2i * h1r)
	    fft2[nxp2-i,nyd2p1] = fft2[i,nyd2p1]
	    fft2[nxp3-i,nyd2p1] = - fft2[i+1,nyd2p1]
	}

	# Compute the remainder of the transform.
	do j = 2, nyd2p1 - 1 {
	    do i = 3, nxfft, 2 {
	        h2r = c1 * (fft1[i,j] + fft1[nxp2-i, nyp2-j])
	        h2i = c1 * (fft1[i+1,j] - fft1[nxp3-i,nyp2-j])
	        h1r = -c2 * (fft1[i+1,j] + fft1[nxp3-i,nyp2-j])
	        h1i = c2 * (fft1[i,j] - fft1[nxp2-i,nyp2-j])
	        fft2[i,j] = (h1r * h2r + h1i * h2i)
	        fft2[i+1,j] = (h1i * h2r - h2i * h1r)
	        fft2[nxp2-i,nyp2-j] = fft2[i,j]
	        fft2[nxp3-i,nyp2-j] = - fft2[i+1,j]
	    }
	}
end


# RG_FNORM -- Normalize the reference and image data before computing
# the fft's.

procedure rg_fnorm (array, ncols, nlines, nxfft, nyfft)

real	array[ARB]			#I/O the input/output data array
int	ncols, nlines			#I dimensions of the input data array
int	nxfft, nyfft			#I dimensions of the fft

int	i, j, index
real	sumr, sumi, meanr, meani

begin
	# Compute the mean.
	sumr = 0.0
	sumi = 0.0
	index = 0
	do j = 1, nlines {
	    do i = 1, ncols {
		sumr = sumr + array[index+2*i-1]
		sumi = sumi + array[index+2*i]
	    }
	    index = index + 2 * nxfft
	}
	meanr = sumr / (ncols * nlines)
	meani = sumi / (ncols * nlines)

	# Compute the sigma.
	sumr = 0.0
	sumi = 0.0
	index = 0
	do j = 1, nlines {
	    do i = 1, ncols {
		sumr = sumr + (array[index+2*i-1] - meanr) ** 2
		sumi = sumi + (array[index+2*i] - meani) ** 2
	    }
	    index = index + 2 * nxfft
	}
	sumr = sqrt (sumr)
	sumi = sqrt (sumi)

	# Normalize the data.
	index = 0
	do j = 1, nlines {
	    do i = 1, ncols {
		array[index+2*i-1] = (array[index+2*i-1] - meanr) / sumr
		array[index+2*i] = (array[index+2*i] - meani) / sumi
	    }
	    index = index + 2 * nxfft
	}
end
