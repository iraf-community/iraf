include	<imhdr.h>

.help imcthreshold
.nf ----------------------------------------------------------------------------
          COMBINING IMAGES: THRESHOLD REJECTION ALGORITHM

The input images are combined by scaling and taking a weighted
average.  Pixels having values below and above a rejection threshold
(before scaling) are excluded from the average.  The exposure time of
the output image is the scaled and weighted average of the input
exposure times.  The average is computed in real arithmetic with
trunction on output if the output image is an integer datatype.

PROCEDURES:

    IMC_THRESHOLD -- Combine the images by averaging with threshold rejection.
    THRESHOLD -- Average and reject the lines (scales and weights equal).
    WTTHRESHOLD -- Average and reject the lines (scales and weights unequal).
.endhelp -----------------------------------------------------------------------


# IMC_THRESHOLD -- Combine the input images by averaging with threshold
# rejection.  Each input image, given by an array of image pointers, is
# scaled and then a weighted average is computed rejecting values which are
# outside the lower and upper threshold.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_thresholds (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	low, high, clgetr()
pointer	imgnls()
pointer	impnlr()

begin
	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the rejection thresholds and scaling factors and weights.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	if (low >= high)
	    call error (0, "Bad threshold limits (lowreject >= highreject)")
	scale = imc_scales ("threshold", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and call a procedure
	# to perform the averging on the line.

	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnls (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtthresholds (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc, low, high)
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
		call thresholds (Memi[data], nimages, Memr[outdata], nc,
		    low, high)
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


# THRESHOLD -- Compute the average image line with threshold rejection.
# The input data is type dependent and the output is real.

procedure thresholds (data, nimages, average, npts, low, high)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold

int	i, j, nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0
	    do j = 1, nimages {
		val = Mems[data[j]+i-1]
		if ((val < low) || (val > high))
		    Mems[data[j]+i-1] = INDEFS
		else {
		   sum = sum + val
		   nsum = nsum + 1
		}
	    }
	    if (nsum > 1)
		average[i] = sum / nsum
	    else
		average[i] = sum
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholds (data, scales, zeros, wts, nimages, average, npts,
	low, high)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points per line
real	low			# Low rejection threshold
real	high			# Low rejection threshold

int	i, j
real	nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0.
	    do j = 1, nimages {
		val = Mems[data[j]+i-1]
		if ((val < low) || (val > high))
		    Mems[data[j]+i-1] = INDEFS
		else {
		    sum = sum + wts[j] * val / scales[j] - zeros[j]
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0.)
		average[i] = sum / nsum
	    else
		average[i] = sum
	}
end

# IMC_THRESHOLD -- Combine the input images by averaging with threshold
# rejection.  Each input image, given by an array of image pointers, is
# scaled and then a weighted average is computed rejecting values which are
# outside the lower and upper threshold.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_thresholdr (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	low, high, clgetr()
pointer	imgnlr()
pointer	impnlr()

begin
	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the rejection thresholds and scaling factors and weights.
	low = clgetr ("lowreject")
	high = clgetr ("highreject")
	if (low >= high)
	    call error (0, "Bad threshold limits (lowreject >= highreject)")
	scale = imc_scales ("threshold", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and call a procedure
	# to perform the averging on the line.

	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtthresholdr (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc, low, high)
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
		call thresholdr (Memi[data], nimages, Memr[outdata], nc,
		    low, high)
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


# THRESHOLD -- Compute the average image line with threshold rejection.
# The input data is type dependent and the output is real.

procedure thresholdr (data, nimages, average, npts, low, high)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold

int	i, j, nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0
	    do j = 1, nimages {
		val = Memr[data[j]+i-1]
		if ((val < low) || (val > high))
		    Memr[data[j]+i-1] = INDEFR
		else {
		   sum = sum + val
		   nsum = nsum + 1
		}
	    }
	    if (nsum > 1)
		average[i] = sum / nsum
	    else
		average[i] = sum
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholdr (data, scales, zeros, wts, nimages, average, npts,
	low, high)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points per line
real	low			# Low rejection threshold
real	high			# Low rejection threshold

int	i, j
real	nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0.
	    do j = 1, nimages {
		val = Memr[data[j]+i-1]
		if ((val < low) || (val > high))
		    Memr[data[j]+i-1] = INDEFR
		else {
		    sum = sum + wts[j] * val / scales[j] - zeros[j]
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0.)
		average[i] = sum / nsum
	    else
		average[i] = sum
	}
end
