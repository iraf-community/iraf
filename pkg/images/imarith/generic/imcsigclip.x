# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help imcsigclip
.nf ----------------------------------------------------------------------------
           COMBINING IMAGES: SIGMA CLIPPING ALGORITHM

If there is only one input image then it is copied to the output image.
If there are two input images then it is an error.  For more than two
input images they are combined by scaling and taking a weighted average while
rejecting points which deviate from the average by more than specified
factors times the expected sigma at each point.  The exposure time of the
output image is the scaled and weighted average of the input exposure times.
The average is computed in real arithmetic with trunction on output if
the output image is an integer datatype.

The sigma clipping algorithm is applied to each image line as follows.

(1) The input image lines are scaled to account for different mean intensities.
(2) The weighted average of each point in the line is computed after rejecting
    the high and low values (the minmax combining algorithm).  This
    minimizes the influence of bad values in the initial estimate of the
    average.
(3) The sigma about the mean at each point (including the high and low values)
    is computed.  Each residual is multiplied by the square root of the
    scaling factor to compensate for the reduction in noise due to the
    intensity scaling.
(4) The most deviant point exceeding a specified factor times the estimated
    sigma is rejected.  Note that at most one value is rejected at each point.
(5) The final weighted average excluding the rejected values is computed.

PROCEDURES:

    IMC_SIGCLIP -- Entry routine to the sigma clipping algorithm.
    WTSIGCLIP   -- Sigma clipping when scales and weights are not equal.
    SIGCLIP     -- Sigma clipping when scales and weights are equal.
.endhelp -----------------------------------------------------------------------


# IMC_SIGCLIP -- Apply sigma clipping algorithm to a set of input
# images, given by an array of image pointers, to form an output image.
# The sigma clipping, scaling, and weighting factors are determined.
# The procedure gets a line from each input image and the output line
# buffer.  A procedure is called for either the weighted or unweighted
# sigma clipping routine to combine the image lines.  The output image
# header is updated to include a scaled and weighted exposure time and
# the number of images combined.

procedure imc_sigclips (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

real	low			# Low sigma cutoff
real	high			# High sigma cutoff

int	i, j, nc
pointer	sp, data, scales, zeros, wts, sigma, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	clgetr()
pointer	imgnls()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copys (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for sigma clipping")

	nc = IM_LEN(out,1)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (sigma, nc, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the sigma clipping, scaling and weighting factors.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	scale = imc_scales ("sigclip", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnls (in[i], Memi[data+i-1], Meml[v1])
		}
	        call wtsclips (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], Memr[outdata], Memr[sigma], nc, nimages,
		    low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call wtsigmas (Memi[data], Memr[scales], Memr[zeros],
			Memr[wts], nimages, Memr[outdata], Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	} else {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnls (in[i], Memi[data+i-1], Meml[v1])
		}
	        call sclips (Memi[data], Memr[outdata], Memr[sigma], nc,
		    nimages, low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmas (Memi[data], nimages, Memr[outdata],
			Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTSCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are not equal.

procedure wtsclips (data, scales, zeros, wts, output, sigma, npts, nlines,
	lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	scales[nlines]		# Scaling for data lines
real	zeros[nlines]		# Zero levels
real	wts[nlines]		# Weights
real	output[npts]		# Output line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	lowsigma		# Low sigma rejection threshold
real	highsigma		# High sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, minwt, maxwt, wt, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the scaled and weighted mean with and without rejecting
	# the minimum and maximum points.

	do i = 1, npts {
	    sum = 0.
	    low = Mems[data[1]+i-1] / scales[1] - zeros[1]
	    high = Mems[data[2]+i-1] / scales[2] - zeros[2]
	    if (low < high) {
		minwt = wts[1]
		maxwt = wts[2]
	    } else {
		minwt = low
		low = high
		high = minwt
		minwt = wts[2]
		maxwt = wts[1]
	    }
	    do j = 3, nlines {
		val = Mems[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < low) {
		    sum = sum + minwt * low
		    low = val
		    minwt = wt
		} else if (val > high) {
		    sum = sum + maxwt * high
		    high = val
		    maxwt = wt
		} else
		    sum = sum + wt * val
	    }
	    Memr[mean+i-1] = sum / (1 - maxwt - minwt)
	    sum = sum + minwt * low + maxwt * high
	    output[i] = sum
	}

	# Compute sigma at each point.  Correct individual residuals for
	# the image scaling.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = (Mems[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = (Mems[data[1]+i-1]/scales[1]-zeros[1] - val) * wts[1]
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = (Mems[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (output[i] - (Mems[data[k]+i-1] / scales[k] -
		    zeros[k]) * wts[k]) / (1 - wts[k])
		Mems[data[k]+i-1] = INDEFS
	    }
	}

	call sfree (sp)
end


# SCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are equal.

procedure sclips (data, output, sigma, npts, nlines, lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	output[npts]		# Mean line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	highsigma		# High sigma rejection threshold
real	lowsigma		# Low sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the mean with and without rejecting the extrema.
	do i = 1, npts {
	    sum = 0.
	    low = Mems[data[1]+i-1]
	    high = Mems[data[2]+i-1]
	    if (low > high) {
		val = low
		low = high
		high = val
	    }
	    do j = 3, nlines {
		val = Mems[data[j]+i-1]
		if (val < low) {
		    sum = sum + low
		    low = val
		} else if (val > high) {
		    sum = sum + high
		    high = val
		} else
		    sum = sum + val
	    }
	    Memr[mean+i-1] = sum / (nlines - 2)
	    sum = sum + low + high
	    output[i] = sum / nlines
	}

	# Compute sigma at each point.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = Mems[data[j]+i-1] - val
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = Mems[data[1]+i-1] - val
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = Mems[data[j]+i-1] - val
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (nlines*output[i] - Mems[data[k]+i-1]) / (nlines-1)
		Mems[data[k]+i-1] = INDEFS
	    }
	}

	call sfree (sp)
end

# IMC_SIGCLIP -- Apply sigma clipping algorithm to a set of input
# images, given by an array of image pointers, to form an output image.
# The sigma clipping, scaling, and weighting factors are determined.
# The procedure gets a line from each input image and the output line
# buffer.  A procedure is called for either the weighted or unweighted
# sigma clipping routine to combine the image lines.  The output image
# header is updated to include a scaled and weighted exposure time and
# the number of images combined.

procedure imc_sigclipi (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

real	low			# Low sigma cutoff
real	high			# High sigma cutoff

int	i, j, nc
pointer	sp, data, scales, zeros, wts, sigma, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	clgetr()
pointer	imgnli()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copyi (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for sigma clipping")

	nc = IM_LEN(out,1)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (sigma, nc, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the sigma clipping, scaling and weighting factors.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	scale = imc_scales ("sigclip", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnli (in[i], Memi[data+i-1], Meml[v1])
		}
	        call wtsclipi (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], Memr[outdata], Memr[sigma], nc, nimages,
		    low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call wtsigmai (Memi[data], Memr[scales], Memr[zeros],
			Memr[wts], nimages, Memr[outdata], Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	} else {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnli (in[i], Memi[data+i-1], Meml[v1])
		}
	        call sclipi (Memi[data], Memr[outdata], Memr[sigma], nc,
		    nimages, low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmai (Memi[data], nimages, Memr[outdata],
			Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTSCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are not equal.

procedure wtsclipi (data, scales, zeros, wts, output, sigma, npts, nlines,
	lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	scales[nlines]		# Scaling for data lines
real	zeros[nlines]		# Zero levels
real	wts[nlines]		# Weights
real	output[npts]		# Output line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	lowsigma		# Low sigma rejection threshold
real	highsigma		# High sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, minwt, maxwt, wt, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the scaled and weighted mean with and without rejecting
	# the minimum and maximum points.

	do i = 1, npts {
	    sum = 0.
	    low = Memi[data[1]+i-1] / scales[1] - zeros[1]
	    high = Memi[data[2]+i-1] / scales[2] - zeros[2]
	    if (low < high) {
		minwt = wts[1]
		maxwt = wts[2]
	    } else {
		minwt = low
		low = high
		high = minwt
		minwt = wts[2]
		maxwt = wts[1]
	    }
	    do j = 3, nlines {
		val = Memi[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < low) {
		    sum = sum + minwt * low
		    low = val
		    minwt = wt
		} else if (val > high) {
		    sum = sum + maxwt * high
		    high = val
		    maxwt = wt
		} else
		    sum = sum + wt * val
	    }
	    Memr[mean+i-1] = sum / (1 - maxwt - minwt)
	    sum = sum + minwt * low + maxwt * high
	    output[i] = sum
	}

	# Compute sigma at each point.  Correct individual residuals for
	# the image scaling.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = (Memi[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = (Memi[data[1]+i-1]/scales[1]-zeros[1] - val) * wts[1]
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = (Memi[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (output[i] - (Memi[data[k]+i-1] / scales[k] -
		    zeros[k]) * wts[k]) / (1 - wts[k])
		Memi[data[k]+i-1] = INDEFI
	    }
	}

	call sfree (sp)
end


# SCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are equal.

procedure sclipi (data, output, sigma, npts, nlines, lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	output[npts]		# Mean line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	highsigma		# High sigma rejection threshold
real	lowsigma		# Low sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the mean with and without rejecting the extrema.
	do i = 1, npts {
	    sum = 0.
	    low = Memi[data[1]+i-1]
	    high = Memi[data[2]+i-1]
	    if (low > high) {
		val = low
		low = high
		high = val
	    }
	    do j = 3, nlines {
		val = Memi[data[j]+i-1]
		if (val < low) {
		    sum = sum + low
		    low = val
		} else if (val > high) {
		    sum = sum + high
		    high = val
		} else
		    sum = sum + val
	    }
	    Memr[mean+i-1] = sum / (nlines - 2)
	    sum = sum + low + high
	    output[i] = sum / nlines
	}

	# Compute sigma at each point.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = Memi[data[j]+i-1] - val
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = Memi[data[1]+i-1] - val
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = Memi[data[j]+i-1] - val
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (nlines*output[i] - Memi[data[k]+i-1]) / (nlines-1)
		Memi[data[k]+i-1] = INDEFI
	    }
	}

	call sfree (sp)
end

# IMC_SIGCLIP -- Apply sigma clipping algorithm to a set of input
# images, given by an array of image pointers, to form an output image.
# The sigma clipping, scaling, and weighting factors are determined.
# The procedure gets a line from each input image and the output line
# buffer.  A procedure is called for either the weighted or unweighted
# sigma clipping routine to combine the image lines.  The output image
# header is updated to include a scaled and weighted exposure time and
# the number of images combined.

procedure imc_sigclipl (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

real	low			# Low sigma cutoff
real	high			# High sigma cutoff

int	i, j, nc
pointer	sp, data, scales, zeros, wts, sigma, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	clgetr()
pointer	imgnll()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copyl (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for sigma clipping")

	nc = IM_LEN(out,1)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (sigma, nc, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the sigma clipping, scaling and weighting factors.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	scale = imc_scales ("sigclip", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnll (in[i], Memi[data+i-1], Meml[v1])
		}
	        call wtsclipl (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], Memr[outdata], Memr[sigma], nc, nimages,
		    low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call wtsigmal (Memi[data], Memr[scales], Memr[zeros],
			Memr[wts], nimages, Memr[outdata], Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	} else {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnll (in[i], Memi[data+i-1], Meml[v1])
		}
	        call sclipl (Memi[data], Memr[outdata], Memr[sigma], nc,
		    nimages, low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmal (Memi[data], nimages, Memr[outdata],
			Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTSCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are not equal.

procedure wtsclipl (data, scales, zeros, wts, output, sigma, npts, nlines,
	lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	scales[nlines]		# Scaling for data lines
real	zeros[nlines]		# Zero levels
real	wts[nlines]		# Weights
real	output[npts]		# Output line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	lowsigma		# Low sigma rejection threshold
real	highsigma		# High sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, minwt, maxwt, wt, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the scaled and weighted mean with and without rejecting
	# the minimum and maximum points.

	do i = 1, npts {
	    sum = 0.
	    low = Meml[data[1]+i-1] / scales[1] - zeros[1]
	    high = Meml[data[2]+i-1] / scales[2] - zeros[2]
	    if (low < high) {
		minwt = wts[1]
		maxwt = wts[2]
	    } else {
		minwt = low
		low = high
		high = minwt
		minwt = wts[2]
		maxwt = wts[1]
	    }
	    do j = 3, nlines {
		val = Meml[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < low) {
		    sum = sum + minwt * low
		    low = val
		    minwt = wt
		} else if (val > high) {
		    sum = sum + maxwt * high
		    high = val
		    maxwt = wt
		} else
		    sum = sum + wt * val
	    }
	    Memr[mean+i-1] = sum / (1 - maxwt - minwt)
	    sum = sum + minwt * low + maxwt * high
	    output[i] = sum
	}

	# Compute sigma at each point.  Correct individual residuals for
	# the image scaling.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = (Meml[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = (Meml[data[1]+i-1]/scales[1]-zeros[1] - val) * wts[1]
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = (Meml[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (output[i] - (Meml[data[k]+i-1] / scales[k] -
		    zeros[k]) * wts[k]) / (1 - wts[k])
		Meml[data[k]+i-1] = INDEFL
	    }
	}

	call sfree (sp)
end


# SCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are equal.

procedure sclipl (data, output, sigma, npts, nlines, lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	output[npts]		# Mean line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	highsigma		# High sigma rejection threshold
real	lowsigma		# Low sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the mean with and without rejecting the extrema.
	do i = 1, npts {
	    sum = 0.
	    low = Meml[data[1]+i-1]
	    high = Meml[data[2]+i-1]
	    if (low > high) {
		val = low
		low = high
		high = val
	    }
	    do j = 3, nlines {
		val = Meml[data[j]+i-1]
		if (val < low) {
		    sum = sum + low
		    low = val
		} else if (val > high) {
		    sum = sum + high
		    high = val
		} else
		    sum = sum + val
	    }
	    Memr[mean+i-1] = sum / (nlines - 2)
	    sum = sum + low + high
	    output[i] = sum / nlines
	}

	# Compute sigma at each point.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = Meml[data[j]+i-1] - val
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = Meml[data[1]+i-1] - val
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = Meml[data[j]+i-1] - val
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (nlines*output[i] - Meml[data[k]+i-1]) / (nlines-1)
		Meml[data[k]+i-1] = INDEFL
	    }
	}

	call sfree (sp)
end

# IMC_SIGCLIP -- Apply sigma clipping algorithm to a set of input
# images, given by an array of image pointers, to form an output image.
# The sigma clipping, scaling, and weighting factors are determined.
# The procedure gets a line from each input image and the output line
# buffer.  A procedure is called for either the weighted or unweighted
# sigma clipping routine to combine the image lines.  The output image
# header is updated to include a scaled and weighted exposure time and
# the number of images combined.

procedure imc_sigclipr (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

real	low			# Low sigma cutoff
real	high			# High sigma cutoff

int	i, j, nc
pointer	sp, data, scales, zeros, wts, sigma, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	clgetr()
pointer	imgnlr()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copyr (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for sigma clipping")

	nc = IM_LEN(out,1)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (sigma, nc, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the sigma clipping, scaling and weighting factors.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	scale = imc_scales ("sigclip", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
		}
	        call wtsclipr (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], Memr[outdata], Memr[sigma], nc, nimages,
		    low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call wtsigmar (Memi[data], Memr[scales], Memr[zeros],
			Memr[wts], nimages, Memr[outdata], Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	} else {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
		}
	        call sclipr (Memi[data], Memr[outdata], Memr[sigma], nc,
		    nimages, low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmar (Memi[data], nimages, Memr[outdata],
			Memr[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTSCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are not equal.

procedure wtsclipr (data, scales, zeros, wts, output, sigma, npts, nlines,
	lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	scales[nlines]		# Scaling for data lines
real	zeros[nlines]		# Zero levels
real	wts[nlines]		# Weights
real	output[npts]		# Output line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	lowsigma		# Low sigma rejection threshold
real	highsigma		# High sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, minwt, maxwt, wt, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the scaled and weighted mean with and without rejecting
	# the minimum and maximum points.

	do i = 1, npts {
	    sum = 0.
	    low = Memr[data[1]+i-1] / scales[1] - zeros[1]
	    high = Memr[data[2]+i-1] / scales[2] - zeros[2]
	    if (low < high) {
		minwt = wts[1]
		maxwt = wts[2]
	    } else {
		minwt = low
		low = high
		high = minwt
		minwt = wts[2]
		maxwt = wts[1]
	    }
	    do j = 3, nlines {
		val = Memr[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < low) {
		    sum = sum + minwt * low
		    low = val
		    minwt = wt
		} else if (val > high) {
		    sum = sum + maxwt * high
		    high = val
		    maxwt = wt
		} else
		    sum = sum + wt * val
	    }
	    Memr[mean+i-1] = sum / (1 - maxwt - minwt)
	    sum = sum + minwt * low + maxwt * high
	    output[i] = sum
	}

	# Compute sigma at each point.  Correct individual residuals for
	# the image scaling.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = (Memr[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = (Memr[data[1]+i-1]/scales[1]-zeros[1] - val) * wts[1]
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = (Memr[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (output[i] - (Memr[data[k]+i-1] / scales[k] -
		    zeros[k]) * wts[k]) / (1 - wts[k])
		Memr[data[k]+i-1] = INDEFR
	    }
	}

	call sfree (sp)
end


# SCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are equal.

procedure sclipr (data, output, sigma, npts, nlines, lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	output[npts]		# Mean line (returned)
real	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	highsigma		# High sigma rejection threshold
real	lowsigma		# Low sigma rejection threshold

int	i, j, k
real	resid, resid2, maxresid, maxresid2
real	sum, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_REAL)

	# Compute the mean with and without rejecting the extrema.
	do i = 1, npts {
	    sum = 0.
	    low = Memr[data[1]+i-1]
	    high = Memr[data[2]+i-1]
	    if (low > high) {
		val = low
		low = high
		high = val
	    }
	    do j = 3, nlines {
		val = Memr[data[j]+i-1]
		if (val < low) {
		    sum = sum + low
		    low = val
		} else if (val > high) {
		    sum = sum + high
		    high = val
		} else
		    sum = sum + val
	    }
	    Memr[mean+i-1] = sum / (nlines - 2)
	    sum = sum + low + high
	    output[i] = sum / nlines
	}

	# Compute sigma at each point.

	sigall = 0.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = Memr[data[j]+i-1] - val
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memr[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = Memr[data[1]+i-1] - val
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = Memr[data[j]+i-1] - val
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (nlines*output[i] - Memr[data[k]+i-1]) / (nlines-1)
		Memr[data[k]+i-1] = INDEFR
	    }
	}

	call sfree (sp)
end

# IMC_SIGCLIP -- Apply sigma clipping algorithm to a set of input
# images, given by an array of image pointers, to form an output image.
# The sigma clipping, scaling, and weighting factors are determined.
# The procedure gets a line from each input image and the output line
# buffer.  A procedure is called for either the weighted or unweighted
# sigma clipping routine to combine the image lines.  The output image
# header is updated to include a scaled and weighted exposure time and
# the number of images combined.

procedure imc_sigclipd (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

real	low			# Low sigma cutoff
real	high			# High sigma cutoff

int	i, j, nc
pointer	sp, data, scales, zeros, wts, sigma, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	clgetr()
pointer	imgnld()
pointer	impnld()

begin
	if (nimages == 1) {
	    call imc_copyd (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for sigma clipping")

	nc = IM_LEN(out,1)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (sigma, nc, TY_DOUBLE)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the sigma clipping, scaling and weighting factors.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	scale = imc_scales ("sigclip", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	if (scale) {
	    while (impnld (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnld (in[i], Memi[data+i-1], Meml[v1])
		}
	        call wtsclipd (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], Memd[outdata], Memd[sigma], nc, nimages,
		    low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnld (sig, sigdata, Meml[v1])
		    call wtsigmad (Memi[data], Memr[scales], Memr[zeros],
			Memr[wts], nimages, Memd[outdata], Memd[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	} else {
	    while (impnld (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnld (in[i], Memi[data+i-1], Meml[v1])
		}
	        call sclipd (Memi[data], Memd[outdata], Memd[sigma], nc,
		    nimages, low, high)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnld (sig, sigdata, Meml[v1])
		    call sigmad (Memi[data], nimages, Memd[outdata],
			Memd[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTSCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are not equal.

procedure wtsclipd (data, scales, zeros, wts, output, sigma, npts, nlines,
	lowsigma, highsigma)

pointer	data[nlines]		# Data lines
real	scales[nlines]		# Scaling for data lines
real	zeros[nlines]		# Zero levels
real	wts[nlines]		# Weights
double	output[npts]		# Output line (returned)
double	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	lowsigma		# Low sigma rejection threshold
real	highsigma		# High sigma rejection threshold

int	i, j, k
double	resid, resid2, maxresid, maxresid2
double	sum, minwt, maxwt, wt, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_DOUBLE)

	# Compute the scaled and weighted mean with and without rejecting
	# the minimum and maximum points.

	do i = 1, npts {
	    sum = 0.
	    low = Memd[data[1]+i-1] / scales[1] - zeros[1]
	    high = Memd[data[2]+i-1] / scales[2] - zeros[2]
	    if (low < high) {
		minwt = wts[1]
		maxwt = wts[2]
	    } else {
		minwt = low
		low = high
		high = minwt
		minwt = wts[2]
		maxwt = wts[1]
	    }
	    do j = 3, nlines {
		val = Memd[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < low) {
		    sum = sum + minwt * low
		    low = val
		    minwt = wt
		} else if (val > high) {
		    sum = sum + maxwt * high
		    high = val
		    maxwt = wt
		} else
		    sum = sum + wt * val
	    }
	    Memd[mean+i-1] = sum / (1 - maxwt - minwt)
	    sum = sum + minwt * low + maxwt * high
	    output[i] = sum
	}

	# Compute sigma at each point.  Correct individual residuals for
	# the image scaling.

	sigall = 0.
	do i = 1, npts {
	    val = Memd[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = (Memd[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memd[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = (Memd[data[1]+i-1]/scales[1]-zeros[1] - val) * wts[1]
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = (Memd[data[j]+i-1]/scales[j]-zeros[j] - val) * wts[j]
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (output[i] - (Memd[data[k]+i-1] / scales[k] -
		    zeros[k]) * wts[k]) / (1 - wts[k])
		Memd[data[k]+i-1] = INDEFD
	    }
	}

	call sfree (sp)
end


# SCLIP -- Combine input data lines using the sigma clip algorithm
# when weights and scale factors are equal.

procedure sclipd (data, output, sigma, npts, nlines, lowsigma, highsigma)

pointer	data[nlines]		# Data lines
double	output[npts]		# Mean line (returned)
double	sigma[npts]		# Sigma line (returned)
int	npts			# Number of points per line
int	nlines			# Number of lines
real	highsigma		# High sigma rejection threshold
real	lowsigma		# Low sigma rejection threshold

int	i, j, k
double	resid, resid2, maxresid, maxresid2
double	sum, val, sig, sigall, low, high
pointer	sp, mean

begin
	call smark (sp)
	call salloc (mean, npts, TY_DOUBLE)

	# Compute the mean with and without rejecting the extrema.
	do i = 1, npts {
	    sum = 0.
	    low = Memd[data[1]+i-1]
	    high = Memd[data[2]+i-1]
	    if (low > high) {
		val = low
		low = high
		high = val
	    }
	    do j = 3, nlines {
		val = Memd[data[j]+i-1]
		if (val < low) {
		    sum = sum + low
		    low = val
		} else if (val > high) {
		    sum = sum + high
		    high = val
		} else
		    sum = sum + val
	    }
	    Memd[mean+i-1] = sum / (nlines - 2)
	    sum = sum + low + high
	    output[i] = sum / nlines
	}

	# Compute sigma at each point.

	sigall = 0.
	do i = 1, npts {
	    val = Memd[mean+i-1]
	    sig = 0.
	    do j = 1, nlines {
		resid = Memd[data[j]+i-1] - val
		sig = sig + resid ** 2
	    }
	    sigma[i] = sqrt (sig / (nlines - 1))
	}

	# Reject pixels.
	do i = 1, npts {
	    val = Memd[mean+i-1]
	    sig = sigma[i]
	    low = -lowsigma * sig
	    high = highsigma * sig

	    maxresid = Memd[data[1]+i-1] - val
	    maxresid2 = maxresid ** 2
	    k = 1
	    do j = 2, nlines {
	        resid = Memd[data[j]+i-1] - val
		resid2 = resid ** 2
		if (resid2 > maxresid2) {
		    maxresid = resid
		    maxresid2 = resid2
		    k = j
		}
	    }
	    if ((maxresid > high) || (maxresid < low)) {
		output[i] = (nlines*output[i] - Memd[data[k]+i-1]) / (nlines-1)
		Memd[data[k]+i-1] = INDEFD
	    }
	}

	call sfree (sp)
end

