# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

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

procedure imc_minreji (log, in, out, sig, nimages)

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
		    j = imgnli (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtminreji (Memi[data], Memr[scales], Memr[zeros],
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
		call minreji (Memi[data], nimages, Memr[outdata], nc)
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


# WTMINREJ -- Combine image lines with weighting and/or scaling.

procedure wtminreji (data, scales, zeros, wts, nimages, output, npts)

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
	    minval = Memi[data[1]+i-1] / scales[1] - zeros[1]
	    minwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memi[data[j]+i-1] / scales[j] - zeros[j]
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
	    Memi[data[k]+i-1] = INDEFI
        }
end


# MINREJ -- Combine image lines without weighting or scaling.

procedure minreji (data, nimages, output, npts)

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
	    minval = Memi[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Memi[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memi[data[k]+i-1] = INDEFI
        }
end

# IMC_MINREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_minrejl (log, in, out, sig, nimages)

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
		    j = imgnll (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtminrejl (Memi[data], Memr[scales], Memr[zeros],
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
		call minrejl (Memi[data], nimages, Memr[outdata], nc)
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


# WTMINREJ -- Combine image lines with weighting and/or scaling.

procedure wtminrejl (data, scales, zeros, wts, nimages, output, npts)

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
	    minval = Meml[data[1]+i-1] / scales[1] - zeros[1]
	    minwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Meml[data[j]+i-1] / scales[j] - zeros[j]
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
	    Meml[data[k]+i-1] = INDEFL
        }
end


# MINREJ -- Combine image lines without weighting or scaling.

procedure minrejl (data, nimages, output, npts)

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
	    minval = Meml[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Meml[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Meml[data[k]+i-1] = INDEFL
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

# IMC_MINREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_minrejd (log, in, out, sig, nimages)

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
	    while (impnld (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnld (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtminrejd (Memi[data], Memr[scales], Memr[zeros],
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
		call minrejd (Memi[data], nimages, Memd[outdata], nc)
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


# WTMINREJ -- Combine image lines with weighting and/or scaling.

procedure wtminrejd (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
double	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, minwt
double	mean, val, minval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memd[data[1]+i-1] / scales[1] - zeros[1]
	    minwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memd[data[j]+i-1] / scales[j] - zeros[j]
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
	    Memd[data[k]+i-1] = INDEFD
        }
end


# MINREJ -- Combine image lines without weighting or scaling.

procedure minrejd (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
double	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
double	mean, val, minval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    minval = Memd[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Memd[data[j]+i-1]
		if (val < minval) {
		    mean = mean + minval
		    minval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memd[data[k]+i-1] = INDEFD
        }
end

# IMC_MINREJ -- Combine the images by scaling and taking a weighted average
# excluding the minimum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_minrejx (log, in, out, sig, nimages)

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
	    while (impnlx (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlx (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtminrejx (Memi[data], Memr[scales], Memr[zeros],
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
		call minrejx (Memi[data], nimages, Memx[outdata], nc)
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


# WTMINREJ -- Combine image lines with weighting and/or scaling.

procedure wtminrejx (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
complex	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, minwt
complex	mean, val, minval
real	absval, absminval

begin
	do i = 1, npts {
	    mean = 0.
	    minval = Memx[data[1]+i-1] / scales[1] - zeros[1]
	    absminval = abs (minval)
	    minwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memx[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
	        absval = abs (val)
		if (absval < absminval) {
		    absminval = absval
		    mean = mean + minwt * minval
		    minval = val
		    minwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - minwt)
	    Memx[data[k]+i-1] = INDEFX
        }
end


# MINREJ -- Combine image lines without weighting or scaling.

procedure minrejx (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
complex	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
complex	mean, val, minval
real	absval, absminval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    minval = Memx[data[1]+i-1]
	    absminval = abs (minval)
	    k = 1
	    do j = 2, nimages {
		val = Memx[data[j]+i-1]
	        absval = abs (val)
		if (absval < absminval) {
		    absminval = absval
		    mean = mean + minval
		    minval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memx[data[k]+i-1] = INDEFX
        }
end

