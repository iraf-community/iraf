# RG_SZFFT -- Compute the size of the required FFT given the number of
# points in the data and the fact that the FFT must be a power of 2.

int procedure rg_szfft (npts, window)

int	npts		#I the number of points in the data
int	window		#I the width of the cross-correlation function

int	nfft, pow2

begin
	nfft = npts + window / 2

	pow2 = 2
	while (pow2 < nfft)
	    pow2 = pow2 * 2

	return (pow2)

end


# RG_RLOAD -- Load a real array into the real part of a complex array.

procedure rg_rload (buf, ncols, nlines, fft, nxfft, nyfft)

real	buf[ARB]	#I the input data buffer
int	ncols, nlines	#I the size of the input data buffer
real	fft[ARB]	#O the array to be fft'd
int	nxfft, nyfft	#I the dimensions of the fft

int	i, dindex, findex

begin
	# Load the reference and image data.
	dindex = 1
	findex = 1
	do i = 1, nlines {
	    call rg_rweave (buf[dindex], fft[findex], ncols)
	    dindex = dindex + ncols
	    findex = findex + 2 * nxfft 
	}
end


# RG_ILOAD -- Load a real array into the complex part of a complex
# array.

procedure rg_iload (buf, ncols, nlines, fft, nxfft, nyfft)

real	buf[ARB]	#I the input data buffer
int	ncols, nlines	#I the size of the input data buffer
real	fft[ARB]	#O the array to be fft'd
int	nxfft, nyfft	#I the dimensions of the fft

int	i, dindex, findex

begin
	# Load the reference and image data.
	dindex = 1
	findex = 1
	do i = 1, nlines {
	    call rg_iweave (buf[dindex], fft[findex], ncols)
	    dindex = dindex + ncols
	    findex = findex + 2 * nxfft 
	}
end


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
	    call fourn (fft, Memi[dim], 1, 1)
	else
	    call fourn (fft, Memi[dim], 2, 1)

	# Compute the product of the two transforms.
	call rg_mulfft (fft, fft, 2 * nxfft, nyfft)

	# Shift the array to center the transform.
	call rg_tfshift (fft, fft, 2 * nxfft, nyfft)

	# Normalize the transform.
	call adivkr (fft, real (nxfft * nyfft), fft, 2 * nxfft * nyfft)

	# Compute the inverse transform.
	if (Memi[dim+1] == 1)
	    call fourn (fft, Memi[dim], 1, -1)
	else
	    call fourn (fft, Memi[dim], 2, -1)

	call sfree (sp)
end


# RG_MOVEXR -- Extract the portion of the FFT for which the computed lags
# are valid. The dimensions of the the FFT are a  power of two
# and the 0 frequency is in the position nxfft / 2 + 1, nyfft / 2 + 1.

procedure rg_movexr (fft, nxfft, nyfft, xcor, xwindow, ywindow)

real	fft[ARB]		#I the input fft
int	nxfft, nyfft		#I the dimensions of the input fft
real	xcor[xwindow,ARB]	#O the output cross-correlation function
int	xwindow, ywindow	#I the cross-correlation function window
	
int	j, ix, iy, index

begin
	# Compute the starting index of the extraction array.
	ix = 1 + nxfft  - 2 * (xwindow / 2)
	iy = 1 + nyfft / 2 - ywindow / 2
	index = ix + 2 * nxfft * (iy - 1)

	# Copy the real part of the Fourier transform into the
	# cross-correlation array.
	do j = 1, ywindow {
	    call rg_extract (fft[index], xcor[1,j], xwindow)
	    index = index + 2 * nxfft
	}
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


# FOURN -- Replaces datas by its n-dimensional discreter Fourier transform,
# if isign is input as 1. NN is an integer array of length ndim containing
# the lengths of each dimension (number of complex values), which must all
# be powers of 2. Data is a real array of length twice the product of these
# lengths, in which the data are stored as in a multidimensional complex
# Fortran array. If isign is input as -1, data is replaced by its inverse
# transform times the product of the lengths of all dimensions.
	
procedure fourn (data, nn, ndim, isign)

real	data[ARB]		#I/O input data and output fft
int	nn[ndim]		#I array of dimension lengths
int	ndim			#I number of dimensions
int	isign			#I forward or inverse transform

int	idim, i1, i2, i3, ip1, ip2, ip3, ifp1, ifp2, i2rev, i3rev, k1, k2
int	ntot, nprev, n, nrem, pibit
double	wr, wi, wpr, wpi, wtemp, theta
real	tempr, tempi

begin
	ntot = 1
	do idim = 1, ndim
	    ntot = ntot * nn[idim]

	nprev = 1
	do idim = 1, ndim {

	    n = nn[idim]
	    nrem = ntot / (n * nprev)
	    ip1 = 2 * nprev
	    ip2 = ip1 * n
	    ip3 = ip2 * nrem
	    i2rev = 1

	    do i2 = 1, ip2, ip1 {

	        if (i2 < i2rev) {
		    do i1 = i2, i2 + ip1 - 2, 2 {
		        do i3 = i1, ip3, ip2 {
			    i3rev = i2rev + i3 - i2
			    tempr = data [i3]
			    tempi = data[i3+1]
			    data[i3] = data[i3rev]
			    data[i3+1] = data[i3rev+1]
			    data[i3rev] = tempr
			    data[i3rev+1] = tempi
		        }
		    }
	        }

	        pibit = ip2 / 2
	        while ((pibit >= ip1) && (i2rev > pibit)) {
		    i2rev = i2rev - pibit
		    pibit = pibit / 2
	        }

	        i2rev = i2rev + pibit
	    }

	    ifp1 = ip1
	    while (ifp1 < ip2) {

		ifp2 = 2 * ifp1
		theta = isign * 6.28318530717959d0 / (ifp2 / ip1)
		wpr = - 2.0d0 * dsin (0.5d0 * theta) ** 2
		wpi = dsin (theta)
		wr = 1.0d0
		wi = 0.0d0

	        do i3 = 1, ifp1, ip1 {
		    do i1 = i3, i3 + ip1 - 2, 2 {
			do i2 = i1, ip3, ifp2 {
			    k1 = i2
			    k2 = k1 + ifp1
			    tempr = sngl (wr) * data[k2] - sngl (wi) *
			        data[k2+1]
			    tempi = sngl (wr) * data[k2+1] + sngl (wi) *
				data[k2]
			    data[k2] = data[k1] - tempr
			    data[k2+1] = data[k1+1] - tempi
			    data[k1] = data[k1] + tempr
			    data[k1+1] = data[k1+1] + tempi
			}
		    }
		    wtemp = wr
		    wr = wr * wpr - wi * wpi + wr
		    wi = wi * wpr + wtemp * wpi + wi
		}

	        ifp1 = ifp2
	    }
	    nprev = n * nprev
	}
end


# RG_TFSHIFT -- Center the array before doing the FFT.

procedure rg_tfshift (fft1, fft2, nx, ny)

real	fft1[nx,ARB]		#I input fft array
real	fft2[nx,ARB]		#O output fft array 
int	nx, ny			#I fft array dimensions

int	i, j
real	fac

begin
	fac = 1.0
	do j = 1, ny {
	    do i = 1, nx, 2 {
		fft2[i,j] = fac * fft1[i,j]
		fft2[i+1,j] = fac * fft1[i+1,j]
		fac = -fac
	    }
	    fac = -fac
	}
end


# RG_RWEAVE -- Weave a real array into the real part of a complex array.
# The output array must be twice as long as the input array.

procedure rg_rweave (a, b, npts)

real	a[ARB]		#I the input array
real	b[ARB]		#O the output array
int	npts		#I number of data points

int	i

begin
	do i = 1, npts 
	    b[2*i-1] = a[i]
end


# RG_IWEAVE -- Weave a real array into the complex part of a complex array.
# The output array must be twice as long as the input array.

procedure rg_iweave (a, b, npts)

real	a[ARB]		#I the input array
real	b[ARB]		#O the output array
int	npts		#I the of data points

int	i

begin
	do i = 1, npts 
	    b[2*i] = a[i]
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


# RG_EXTRACT -- Extract the real part of a complex array.

procedure rg_extract (a, b, npts)

real	a[ARB]		#I the input array
real	b[ARB]		#O the output array
int	npts		#I the number of data points

int	i

begin
	do i = 1, npts
	    b[i] = a[2*i-1]
end
