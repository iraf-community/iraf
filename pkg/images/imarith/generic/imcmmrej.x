# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help imcmmrej
.nf ----------------------------------------------------------------------------
         COMBINING IMAGES: MINMAX REJECTION ALGORITHM

If there is only one input image then it is copied to the output image.
If there are two input images then it is an error.  For more than two
input images they are combined by scaling and taking a weighted
average excluding the minimum and maximum values.  The exposure time of
the output image is the scaled and weighted average of the input
exposure times.  The average is computed in real arithmetic with
trunction on output if the output image is an integer datatype.

PROCEDURES:

    IMC_MMREJ -- Combine the images with minmax rejection
    WTMMREJ -- Combine image lines with weighting or scaling
    MMREJ -- Combine image lines without weighting or scaling
.endhelp -----------------------------------------------------------------------


# IMC_MMREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum and maximum values.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_mmrejs (log, in, out, sig, nimages)

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

	if (nimages == 2)
	    call error (0, "Too few images for minimum/maximum rejection")

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
	scale = imc_scales ("minmaxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnls (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmmrejs (Memi[data], Memr[scales], Memr[zeros],
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
		call mmrejs (Memi[data], nimages, Memr[outdata], nc)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmar (Memi[data], nimages, Memr[outdata],
			Mems[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTMMREJ -- Minmax combine image lines with weighting and/or scaling.

procedure wtmmrejs (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l
real	wt, minwt, maxwt
real	mean, val, minval, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Mems[data[1]+i-1] / scales[1] - zeros[1]
	    maxval = Mems[data[2]+i-1] / scales[2] - zeros[2]
	    if (minval < maxval) {
		minwt = wts[1]
		maxwt = wts[2]
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		minwt = wts[2]
		maxwt = wts[1]
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Mems[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    l = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt - minwt)
	    Mems[data[k]+i-1] = INDEFS
	    Mems[data[l]+i-1] = INDEFS
        }
end


# MMREJ -- Minmax combine image lines without weighting or scaling.

procedure mmrejs (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l, nims
real	mean, val, minval, maxval

begin
	nims = nimages - 2
	do i = 1, npts {
	    mean = 0.
	    minval = Mems[data[1]+i-1]
	    maxval = Mems[data[2]+i-1]
	    if (minval < maxval) {
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Mems[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    l = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Mems[data[k]+i-1] = INDEFS
	    Mems[data[l]+i-1] = INDEFS
        }
end

# IMC_MMREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum and maximum values.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_mmreji (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnli()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copyi (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for minimum/maximum rejection")

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
	scale = imc_scales ("minmaxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnli (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmmreji (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc)
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
		call mmreji (Memi[data], nimages, Memr[outdata], nc)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmar (Memi[data], nimages, Memr[outdata],
			Memi[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTMMREJ -- Minmax combine image lines with weighting and/or scaling.

procedure wtmmreji (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l
real	wt, minwt, maxwt
real	mean, val, minval, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memi[data[1]+i-1] / scales[1] - zeros[1]
	    maxval = Memi[data[2]+i-1] / scales[2] - zeros[2]
	    if (minval < maxval) {
		minwt = wts[1]
		maxwt = wts[2]
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		minwt = wts[2]
		maxwt = wts[1]
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memi[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    l = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt - minwt)
	    Memi[data[k]+i-1] = INDEFI
	    Memi[data[l]+i-1] = INDEFI
        }
end


# MMREJ -- Minmax combine image lines without weighting or scaling.

procedure mmreji (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l, nims
real	mean, val, minval, maxval

begin
	nims = nimages - 2
	do i = 1, npts {
	    mean = 0.
	    minval = Memi[data[1]+i-1]
	    maxval = Memi[data[2]+i-1]
	    if (minval < maxval) {
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memi[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    l = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memi[data[k]+i-1] = INDEFI
	    Memi[data[l]+i-1] = INDEFI
        }
end

# IMC_MMREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum and maximum values.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_mmrejl (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnll()
pointer	impnlr()

begin
	if (nimages == 1) {
	    call imc_copyl (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for minimum/maximum rejection")

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
	scale = imc_scales ("minmaxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnll (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmmrejl (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memr[outdata], nc)
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
		call mmrejl (Memi[data], nimages, Memr[outdata], nc)
		if (sig != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = impnlr (sig, sigdata, Meml[v1])
		    call sigmar (Memi[data], nimages, Memr[outdata],
			Meml[sigdata], nc)
		}
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }
	}

	call sfree (sp)
end


# WTMMREJ -- Minmax combine image lines with weighting and/or scaling.

procedure wtmmrejl (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l
real	wt, minwt, maxwt
real	mean, val, minval, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Meml[data[1]+i-1] / scales[1] - zeros[1]
	    maxval = Meml[data[2]+i-1] / scales[2] - zeros[2]
	    if (minval < maxval) {
		minwt = wts[1]
		maxwt = wts[2]
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		minwt = wts[2]
		maxwt = wts[1]
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Meml[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    l = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt - minwt)
	    Meml[data[k]+i-1] = INDEFL
	    Meml[data[l]+i-1] = INDEFL
        }
end


# MMREJ -- Minmax combine image lines without weighting or scaling.

procedure mmrejl (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l, nims
real	mean, val, minval, maxval

begin
	nims = nimages - 2
	do i = 1, npts {
	    mean = 0.
	    minval = Meml[data[1]+i-1]
	    maxval = Meml[data[2]+i-1]
	    if (minval < maxval) {
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Meml[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    l = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Meml[data[k]+i-1] = INDEFL
	    Meml[data[l]+i-1] = INDEFL
        }
end

# IMC_MMREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum and maximum values.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_mmrejr (log, in, out, sig, nimages)

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

	if (nimages == 2)
	    call error (0, "Too few images for minimum/maximum rejection")

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
	scale = imc_scales ("minmaxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmmrejr (Memi[data], Memr[scales], Memr[zeros],
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
		call mmrejr (Memi[data], nimages, Memr[outdata], nc)
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


# WTMMREJ -- Minmax combine image lines with weighting and/or scaling.

procedure wtmmrejr (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l
real	wt, minwt, maxwt
real	mean, val, minval, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memr[data[1]+i-1] / scales[1] - zeros[1]
	    maxval = Memr[data[2]+i-1] / scales[2] - zeros[2]
	    if (minval < maxval) {
		minwt = wts[1]
		maxwt = wts[2]
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		minwt = wts[2]
		maxwt = wts[1]
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memr[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    l = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt - minwt)
	    Memr[data[k]+i-1] = INDEFR
	    Memr[data[l]+i-1] = INDEFR
        }
end


# MMREJ -- Minmax combine image lines without weighting or scaling.

procedure mmrejr (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l, nims
real	mean, val, minval, maxval

begin
	nims = nimages - 2
	do i = 1, npts {
	    mean = 0.
	    minval = Memr[data[1]+i-1]
	    maxval = Memr[data[2]+i-1]
	    if (minval < maxval) {
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memr[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    l = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memr[data[k]+i-1] = INDEFR
	    Memr[data[l]+i-1] = INDEFR
        }
end

# IMC_MMREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum and maximum values.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_mmrejd (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnld()
pointer	impnld()

begin
	if (nimages == 1) {
	    call imc_copyd (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for minimum/maximum rejection")

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
	scale = imc_scales ("minmaxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnld (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnld (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmmrejd (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memd[outdata], nc)
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
		call mmrejd (Memi[data], nimages, Memd[outdata], nc)
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


# WTMMREJ -- Minmax combine image lines with weighting and/or scaling.

procedure wtmmrejd (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
double	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l
real	wt, minwt, maxwt
double	mean, val, minval, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memd[data[1]+i-1] / scales[1] - zeros[1]
	    maxval = Memd[data[2]+i-1] / scales[2] - zeros[2]
	    if (minval < maxval) {
		minwt = wts[1]
		maxwt = wts[2]
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		minwt = wts[2]
		maxwt = wts[1]
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memd[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val < minval) {
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    l = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt - minwt)
	    Memd[data[k]+i-1] = INDEFD
	    Memd[data[l]+i-1] = INDEFD
        }
end


# MMREJ -- Minmax combine image lines without weighting or scaling.

procedure mmrejd (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
double	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l, nims
double	mean, val, minval, maxval

begin
	nims = nimages - 2
	do i = 1, npts {
	    mean = 0.
	    minval = Memd[data[1]+i-1]
	    maxval = Memd[data[2]+i-1]
	    if (minval < maxval) {
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memd[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    l = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memd[data[k]+i-1] = INDEFD
	    Memd[data[l]+i-1] = INDEFD
        }
end

# IMC_MMREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum and maximum values.  The output image header
# is updated to include a scaled and weighted exposure time and the number
# of images combined.

procedure imc_mmrejx (log, in, out, sig, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input images
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnlx()
pointer	impnlx()

begin
	if (nimages == 1) {
	    call imc_copyx (in[1], out)
	    return
	}

	if (nimages == 2)
	    call error (0, "Too few images for minimum/maximum rejection")

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
	scale = imc_scales ("minmaxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlx (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlx (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmmrejx (Memi[data], Memr[scales], Memr[zeros],
		    Memr[wts], nimages, Memx[outdata], nc)
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
		call mmrejx (Memi[data], nimages, Memx[outdata], nc)
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


# WTMMREJ -- Minmax combine image lines with weighting and/or scaling.

procedure wtmmrejx (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
complex	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l
real	wt, minwt, maxwt
complex	mean, val, minval, maxval
real	absval, absminval, absmaxval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memx[data[1]+i-1] / scales[1] - zeros[1]
	    maxval = Memx[data[2]+i-1] / scales[2] - zeros[2]
	    absminval = abs (minval)
	    absmaxval = abs (maxval)
	    if (absminval < absmaxval) {
		minwt = wts[1]
		maxwt = wts[2]
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
	        absminval = abs (minval)
	        absmaxval = abs (maxval)
		minwt = wts[2]
		maxwt = wts[1]
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memx[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
	        absval = abs (val)
		if (absval < absminval) {
		    absminval = absval
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else if (absval > absmaxval) {
		    absmaxval = absval
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    l = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt - minwt)
	    Memx[data[k]+i-1] = INDEFX
	    Memx[data[l]+i-1] = INDEFX
        }
end


# MMREJ -- Minmax combine image lines without weighting or scaling.

procedure mmrejx (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
complex	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, l, nims
complex	mean, val, minval, maxval
real	absval, absminval, absmaxval

begin
	nims = nimages - 2
	do i = 1, npts {
	    mean = 0.
	    minval = Memx[data[1]+i-1]
	    maxval = Memx[data[2]+i-1]
	    absminval = abs (minval)
	    absmaxval = abs (maxval)
	    if (absminval < absmaxval) {
		k = 1
		l = 2
	    } else {
		val = minval
		minval = maxval
		maxval = val
	        absminval = abs (minval)
	        absmaxval = abs (maxval)
		k = 2
		l = 1
	    }
	    do j = 3, nimages {
		val = Memx[data[j]+i-1]
	        absval = abs (val)
		if (absval < absminval) {
		    absminval = absval
		    mean = mean + minval
		    minval = val
		    k = j
		} else if (absval > absmaxval) {
		    absmaxval = absval
		    mean = mean + maxval
		    maxval = val
		    l = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memx[data[k]+i-1] = INDEFX
	    Memx[data[l]+i-1] = INDEFX
        }
end

