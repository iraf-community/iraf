# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help imcmaxrej
.nf ----------------------------------------------------------------------------
           COMBINING IMAGES: MAXIMUM REJECTION ALGORITHM

If there is only one input image then it is copied to the output image.
For more than one image they are combined by scaling and taking a weighted
average excluding the maximum value.  The exposure time of
the output image is the scaled and weighted average of the input
exposure times.  The average is computed in real arithmetic with
trunction on output if the output image is an integer datatype.

PROCEDURES:

    IMC_MAXREJ -- Combine the images with maximum rejection
    WTMAXREJ -- Combine image lines with weighting or scaling
    MAXREJ -- Combine image lines without weighting or scaling
.endhelp -----------------------------------------------------------------------


# IMC_MAXREJ -- Combine the images by scaling and taking a weighted average
# excluding the maximum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_maxrejs (log, in, out, sig, nimages)

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
	scale = imc_scales ("maxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnls (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmaxrejs (Memi[data], Memr[scales], Memr[zeros],
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
		call maxrejs (Memi[data], nimages, Memr[outdata], nc)
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


# WTMAXREJ -- Combine lines with scaling and/or weighting.

procedure wtmaxrejs (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, maxwt
real	mean, val, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    maxval = Mems[data[1]+i-1] / scales[1] - zeros[1]
	    maxwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Mems[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt)
	    Mems[data[k]+i-1] = INDEFS
        }
end


# MAXREJ -- Combine image lines without weighting or scaling.

procedure maxrejs (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
real	mean, val, maxval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    maxval = Mems[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Mems[data[j]+i-1]
		if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Mems[data[k]+i-1] = INDEFS
        }
end

# IMC_MAXREJ -- Combine the images by scaling and taking a weighted average
# excluding the maximum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_maxreji (log, in, out, sig, nimages)

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
	scale = imc_scales ("maxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnli (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmaxreji (Memi[data], Memr[scales], Memr[zeros],
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
		call maxreji (Memi[data], nimages, Memr[outdata], nc)
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


# WTMAXREJ -- Combine lines with scaling and/or weighting.

procedure wtmaxreji (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, maxwt
real	mean, val, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    maxval = Memi[data[1]+i-1] / scales[1] - zeros[1]
	    maxwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memi[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt)
	    Memi[data[k]+i-1] = INDEFI
        }
end


# MAXREJ -- Combine image lines without weighting or scaling.

procedure maxreji (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
real	mean, val, maxval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    maxval = Memi[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Memi[data[j]+i-1]
		if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memi[data[k]+i-1] = INDEFI
        }
end

# IMC_MAXREJ -- Combine the images by scaling and taking a weighted average
# excluding the maximum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_maxrejl (log, in, out, sig, nimages)

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
	scale = imc_scales ("maxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnll (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmaxrejl (Memi[data], Memr[scales], Memr[zeros],
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
		call maxrejl (Memi[data], nimages, Memr[outdata], nc)
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


# WTMAXREJ -- Combine lines with scaling and/or weighting.

procedure wtmaxrejl (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, maxwt
real	mean, val, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    maxval = Meml[data[1]+i-1] / scales[1] - zeros[1]
	    maxwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Meml[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt)
	    Meml[data[k]+i-1] = INDEFL
        }
end


# MAXREJ -- Combine image lines without weighting or scaling.

procedure maxrejl (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
real	mean, val, maxval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    maxval = Meml[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Meml[data[j]+i-1]
		if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Meml[data[k]+i-1] = INDEFL
        }
end

# IMC_MAXREJ -- Combine the images by scaling and taking a weighted average
# excluding the maximum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_maxrejr (log, in, out, sig, nimages)

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
	scale = imc_scales ("maxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlr (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmaxrejr (Memi[data], Memr[scales], Memr[zeros],
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
		call maxrejr (Memi[data], nimages, Memr[outdata], nc)
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


# WTMAXREJ -- Combine lines with scaling and/or weighting.

procedure wtmaxrejr (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, maxwt
real	mean, val, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    maxval = Memr[data[1]+i-1] / scales[1] - zeros[1]
	    maxwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memr[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt)
	    Memr[data[k]+i-1] = INDEFR
        }
end


# MAXREJ -- Combine image lines without weighting or scaling.

procedure maxrejr (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
real	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
real	mean, val, maxval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    maxval = Memr[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Memr[data[j]+i-1]
		if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memr[data[k]+i-1] = INDEFR
        }
end

# IMC_MAXREJ -- Combine the images by scaling and taking a weighted average
# excluding the maximum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_maxrejd (log, in, out, sig, nimages)

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
	scale = imc_scales ("maxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnld (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnld (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmaxrejd (Memi[data], Memr[scales], Memr[zeros],
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
		call maxrejd (Memi[data], nimages, Memd[outdata], nc)
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


# WTMAXREJ -- Combine lines with scaling and/or weighting.

procedure wtmaxrejd (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
double	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, maxwt
double	mean, val, maxval

begin
	do i = 1, npts {
	    mean = 0.
	    maxval = Memd[data[1]+i-1] / scales[1] - zeros[1]
	    maxwt = wts[1]
	    k = 1
	    do j = 2, nimages {
		val = Memd[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
		if (val > maxval) {
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt)
	    Memd[data[k]+i-1] = INDEFD
        }
end


# MAXREJ -- Combine image lines without weighting or scaling.

procedure maxrejd (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
double	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
double	mean, val, maxval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    maxval = Memd[data[1]+i-1]
	    k = 1
	    do j = 2, nimages {
		val = Memd[data[j]+i-1]
		if (val > maxval) {
		    mean = mean + maxval
		    maxval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memd[data[k]+i-1] = INDEFD
        }
end

# IMC_MAXREJ -- Combine the images by scaling and taking a weighted average
# excluding the maximum value.  The output image header is updated to include
# a scaled and weighted exposure time and the number of images combined.

procedure imc_maxrejx (log, in, out, sig, nimages)

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
	scale = imc_scales ("maxreject", log, 0., 0., in, out, Memr[scales],
	    Memr[zeros], Memr[wts], nimages)

	# For each line get input and ouput image lines and combine them.
	nc = IM_LEN(out,1)
	if (scale) {
	    while (impnlx (out, outdata, Meml[v1]) != EOF) {
		do i = 1, nimages {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    j = imgnlx (in[i], Memi[data+i-1], Meml[v1])
		}
		call wtmaxrejx (Memi[data], Memr[scales], Memr[zeros],
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
		call maxrejx (Memi[data], nimages, Memx[outdata], nc)
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


# WTMAXREJ -- Combine lines with scaling and/or weighting.

procedure wtmaxrejx (data, scales, zeros, wts, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
complex	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k
real	wt, maxwt
complex	mean, val, maxval
real	absval, absmaxval

begin
	do i = 1, npts {
	    mean = 0.
	    maxval = Memx[data[1]+i-1] / scales[1] - zeros[1]
	    maxwt = wts[1]
	    absmaxval = abs (maxval)
	    k = 1
	    do j = 2, nimages {
		val = Memx[data[j]+i-1] / scales[j] - zeros[j]
		wt = wts[j]
	        absval = abs (val)
		if (absval > absmaxval) {
		    absmaxval = absval
		    mean = mean + maxwt * maxval
		    maxval = val
		    maxwt = wt
		    k = j
		} else
		    mean = mean + wt * val
	    }
	    output[i] = mean / (1. - maxwt)
	    Memx[data[k]+i-1] = INDEFX
        }
end


# MAXREJ -- Combine image lines without weighting or scaling.

procedure maxrejx (data, nimages, output, npts)

pointer	data[nimages]		# IMIO data pointers
int	nimages			# Number of data lines
complex	output[npts]		# Output line (returned)
int	npts			# Number of data points per line

int	i, j, k, nims
complex	mean, val, maxval
real	absval, absmaxval

begin
	nims = nimages - 1
	do i = 1, npts {
	    mean = 0.
	    maxval = Memx[data[1]+i-1]
	    absmaxval = abs (maxval)
	    k = 1
	    do j = 2, nimages {
		val = Memx[data[j]+i-1]
	        absval = abs (val)
		if (absval > absmaxval) {
		    absmaxval = absval
		    mean = mean + maxval
		    maxval = val
		    k = j
		} else
		    mean = mean + val
	    }
	    output[i] = mean / nims
	    Memx[data[k]+i-1] = INDEFX
        }
end

