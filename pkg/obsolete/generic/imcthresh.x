# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

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
real	low, high, blank, clgetr()
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
	blank = clgetr ("blank")
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
		    Memr[wts], nimages, Memr[outdata], nc, low, high, blank)
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
		    low, high, blank)
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

procedure thresholds (data, nimages, average, npts, low, high, blank)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

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
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholds (data, scales, zeros, wts, nimages, average, npts,
	low, high, blank)

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
real	blank			# Blank value if all points rejected

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
		    sum = sum + wts[j] * (val / scales[j] - zeros[j])
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end

# IMC_THRESHOLD -- Combine the input images by averaging with threshold
# rejection.  Each input image, given by an array of image pointers, is
# scaled and then a weighted average is computed rejecting values which are
# outside the lower and upper threshold.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_thresholdi (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	low, high, blank, clgetr()
pointer	imgnli()
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
	blank = clgetr ("blank")
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
		    j = imgnli (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtthresholdi (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc, low, high, blank)
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
		call thresholdi (Memi[data], nimages, Memr[outdata], nc,
		    low, high, blank)
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


# THRESHOLD -- Compute the average image line with threshold rejection.
# The input data is type dependent and the output is real.

procedure thresholdi (data, nimages, average, npts, low, high, blank)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

int	i, j, nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0
	    do j = 1, nimages {
		val = Memi[data[j]+i-1]
		if ((val < low) || (val > high))
		    Memi[data[j]+i-1] = INDEFI
		else {
		   sum = sum + val
		   nsum = nsum + 1
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholdi (data, scales, zeros, wts, nimages, average, npts,
	low, high, blank)

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
real	blank			# Blank value if all points rejected

int	i, j
real	nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0.
	    do j = 1, nimages {
		val = Memi[data[j]+i-1]
		if ((val < low) || (val > high))
		    Memi[data[j]+i-1] = INDEFI
		else {
		    sum = sum + wts[j] * (val / scales[j] - zeros[j])
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end

# IMC_THRESHOLD -- Combine the input images by averaging with threshold
# rejection.  Each input image, given by an array of image pointers, is
# scaled and then a weighted average is computed rejecting values which are
# outside the lower and upper threshold.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_thresholdl (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	low, high, blank, clgetr()
pointer	imgnll()
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
	blank = clgetr ("blank")
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
		    j = imgnll (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtthresholdl (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc, low, high, blank)
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
		call thresholdl (Memi[data], nimages, Memr[outdata], nc,
		    low, high, blank)
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


# THRESHOLD -- Compute the average image line with threshold rejection.
# The input data is type dependent and the output is real.

procedure thresholdl (data, nimages, average, npts, low, high, blank)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

int	i, j, nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0
	    do j = 1, nimages {
		val = Meml[data[j]+i-1]
		if ((val < low) || (val > high))
		    Meml[data[j]+i-1] = INDEFL
		else {
		   sum = sum + val
		   nsum = nsum + 1
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholdl (data, scales, zeros, wts, nimages, average, npts,
	low, high, blank)

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
real	blank			# Blank value if all points rejected

int	i, j
real	nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0.
	    do j = 1, nimages {
		val = Meml[data[j]+i-1]
		if ((val < low) || (val > high))
		    Meml[data[j]+i-1] = INDEFL
		else {
		    sum = sum + wts[j] * (val / scales[j] - zeros[j])
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
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
real	low, high, blank, clgetr()
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
	blank = clgetr ("blank")
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
		    Memr[wts], nimages, Memr[outdata], nc, low, high, blank)
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
		    low, high, blank)
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

procedure thresholdr (data, nimages, average, npts, low, high, blank)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts]		# Average line (returned)
real	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

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
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholdr (data, scales, zeros, wts, nimages, average, npts,
	low, high, blank)

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
real	blank			# Blank value if all points rejected

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
		    sum = sum + wts[j] * (val / scales[j] - zeros[j])
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end

# IMC_THRESHOLD -- Combine the input images by averaging with threshold
# rejection.  Each input image, given by an array of image pointers, is
# scaled and then a weighted average is computed rejecting values which are
# outside the lower and upper threshold.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_thresholdd (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	low, high, blank, clgetr()
pointer	imgnld()
pointer	impnld()

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
	blank = clgetr ("blank")
	if (low >= high)
	    call error (0, "Bad threshold limits (lowreject >= highreject)")
	scale = imc_scales ("threshold", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and call a procedure
	# to perform the averging on the line.

	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnld (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnld (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtthresholdd (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memd[outdata], nc, low, high, blank)
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
		call thresholdd (Memi[data], nimages, Memd[outdata], nc,
		    low, high, blank)
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


# THRESHOLD -- Compute the average image line with threshold rejection.
# The input data is type dependent and the output is real.

procedure thresholdd (data, nimages, average, npts, low, high, blank)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
double	average[npts]		# Average line (returned)
double	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

int	i, j, nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0
	    do j = 1, nimages {
		val = Memd[data[j]+i-1]
		if ((val < low) || (val > high))
		    Memd[data[j]+i-1] = INDEFD
		else {
		   sum = sum + val
		   nsum = nsum + 1
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholdd (data, scales, zeros, wts, nimages, average, npts,
	low, high, blank)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
double	average[npts]		# Average line (returned)
double	sum, val
int	npts			# Number of data points per line
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

int	i, j
real	nsum

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0.
	    do j = 1, nimages {
		val = Memd[data[j]+i-1]
		if ((val < low) || (val > high))
		    Memd[data[j]+i-1] = INDEFD
		else {
		    sum = sum + wts[j] * (val / scales[j] - zeros[j])
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end

# IMC_THRESHOLD -- Combine the input images by averaging with threshold
# rejection.  Each input image, given by an array of image pointers, is
# scaled and then a weighted average is computed rejecting values which are
# outside the lower and upper threshold.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_thresholdx (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
real	low, high, blank, clgetr()
pointer	imgnlx()
pointer	impnlx()

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
	blank = clgetr ("blank")
	if (low >= high)
	    call error (0, "Bad threshold limits (lowreject >= highreject)")
	scale = imc_scales ("threshold", log, low, high, in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and call a procedure
	# to perform the averging on the line.

	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlx (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlx (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtthresholdx (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memx[outdata], nc, low, high, blank)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlx (sig, sigdata, Meml[v1])
		    call wtsigmax (Memi[data], Memr[scales], Memr[zeros],
			Memr[wts], nimages, Memx[outdata], Memx[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	} else {
	    while (impnlx (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlx (in[i], Memi[data+i-1], Meml[v1])
		}
		call thresholdx (Memi[data], nimages, Memx[outdata], nc,
		    low, high, blank)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlx (sig, sigdata, Meml[v1])
		    call sigmax (Memi[data], nimages, Memx[outdata],
			Memx[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# THRESHOLD -- Compute the average image line with threshold rejection.
# The input data is type dependent and the output is real.

procedure thresholdx (data, nimages, average, npts, low, high, blank)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
complex	average[npts]		# Average line (returned)
complex	sum, val
int	npts			# Number of data points
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

int	i, j, nsum
real	absval

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0
	    do j = 1, nimages {
		val = Memx[data[j]+i-1]
		absval = abs (val)
		if ((absval < low) || (absval > high))
		    Memx[data[j]+i-1] = INDEFX
		else {
		   sum = sum + val
		   nsum = nsum + 1
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end


# WTTHRESHOLD -- Compute the weighted average image line with threshold
# rejection.  The input data is type dependent and the output is real.

procedure wtthresholdx (data, scales, zeros, wts, nimages, average, npts,
	low, high, blank)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
complex	average[npts]		# Average line (returned)
complex	sum, val
int	npts			# Number of data points per line
real	low			# Low rejection threshold
real	high			# Low rejection threshold
real	blank			# Blank value if all points rejected

int	i, j
real	nsum
real	absval

begin
	do i = 1, npts {
	    sum = 0.
	    nsum = 0.
	    do j = 1, nimages {
		val = Memx[data[j]+i-1]
		absval = abs (val)
		if ((absval < low) || (absval > high))
		    Memx[data[j]+i-1] = INDEFX
		else {
		    sum = sum + wts[j] * (val / scales[j] - zeros[j])
		    nsum = nsum + wts[j]
		}
	    }
	    if (nsum > 0)
		average[i] = sum / nsum
	    else
		average[i] = blank
	}
end

