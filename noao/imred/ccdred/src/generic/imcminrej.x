include	<imhdr.h>

.help imcminrej
.nf ----------------------------------------------------------------------------
          COMBINING IMAGES: MINIMUM REJECTION ALGORITHM

If there is only one input image then it is copied to the output image.
For more than one input image they are combined by scaling and taking a weighted
average excluding the minimum value.  The exposure time of
the output image is the scaled and weighted average of the input
exposure times.  The average is computed in real arithmetic with
trunction on output if the output image is an integer datatype.

PROCEDURES:

    IMC_MINREJ -- Combine the images with minimum rejection
    WTMINREJ -- Combine image lines with weighting or scaling
    MINREJ -- Combine image lines without weighting or scaling
.endhelp -----------------------------------------------------------------------


# IMC_MINREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_minrejs (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnls()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copys (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_REAL)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("minreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnls (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtminrejs (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc)
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
		call minrejs (Memi[data], nimages, Memr[outdata], nc)
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


# WTMINREJ -- Combine image lines with weighting and/or scaling.

procedure wtminrejs (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, minwt
real	mean, val, minval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Mems[data[1]+i-1] / scales[1] - zeros[1]
	    minwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Mems[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - minwt)
	    Mems[data[k]+i-1] = INDEFS
        }
end


# MINREJ -- Combine image lines without weighting or scaling.

procedure minrejs (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
real	mean, val, minval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    minval = Mems[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Mems[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Mems[data[k]+i-1] = INDEFS
        }
end

# IMC_MINREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_minrejr (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnlr()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copyr (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_REAL)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("minreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtminrejr (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc)
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
		call minrejr (Memi[data], nimages, Memr[outdata], nc)
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


# WTMINREJ -- Combine image lines with weighting and/or scaling.

procedure wtminrejr (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, minwt
real	mean, val, minval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memr[data[1]+i-1] / scales[1] - zeros[1]
	    minwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memr[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - minwt)
	    Memr[data[k]+i-1] = INDEFR
        }
end


# MINREJ -- Combine image lines without weighting or scaling.

procedure minrejr (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
real	mean, val, minval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    minval = Memr[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Memr[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memr[data[k]+i-1] = INDEFR
        }
end
