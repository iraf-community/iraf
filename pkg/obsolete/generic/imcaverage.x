# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help imcaverage
.nf ----------------------------------------------------------------------------
                  COMBINING IMAGES: AVERAGING ALGORITHM

The input images are combined by scaling and taking a weighted average.  The
exposure time of the output image is the scaled and weighted average of the
input exposure times.

PROCEDURES:

    IMC_AVERAGE -- Combine the input images by averaging.
    AVERAGE -- Average the lines when the scales and weights are equal.
    WTAVERAGE -- Average the lines when the scales and weights are not equal.
.endhelp -----------------------------------------------------------------------


# IMC_AVERAGE -- Combine the input images by averaging.
# Each input image, given by an array of image pointers, is scaled and
# then a weighted average is computed.  The output image header is updated
# to include a scaled and weighted exposure time and the number of images
# combined.

procedure imc_averages (log, in, out, sig, nimages)

int	log			# Log FD
pointer	in[nimages]		# Input image
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnls()
pointer	impnlr()
errchk	imc_scales, imgnls

begin
	if (nimages == 1) {
	    call imc_copys (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("average", log, 0., 0., in, out, Memr[scales],
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
		call wtaverages (Memi[data], Memr[scales], Memr[zeros],
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
		call averages (Memi[data], nimages, Memr[outdata], nc)
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


# AVERAGE -- Compute the average image line.

procedure averages (data, nimages, average, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts], nsum	# Average line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aclrr (average, npts)
	i = 1

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        average[j] = average[j] + Mems[p1] + Mems[p2] + Mems[p3] +
		    Mems[p4]
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
	        average[j] = average[j] + Mems[p1] + Mems[p2] + Mems[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        average[j] = average[j] + Mems[p1] + Mems[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    p1 = data[i]
	    do j = 1, npts {
	        average[j] = average[j] + Mems[p1]
	        p1 = p1 + 1
	    }
	}

	nsum = nimages
	call adivkr (average, nsum, average, npts)
end


# WTAVERAGE -- Compute the weighted average image line.  The input data is
# type dependent and the output is real.

procedure wtaverages (data, scales, zeros, wts, nimages, average, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	average[npts]		# Average line (returned)
int	npts			# Number of data points per line

int	i, j, nleft
real	s1, s2, s3, s4, z1, z2, z3, z4, w1, w2, w3, w4
pointer	p1, p2, p3, p4

begin
	s1 = scales[1]
	s2 = scales[2]
	z1 = zeros[1]
	z2 = zeros[2]
	w1 = wts[1]
	w2 = wts[2]
	p1 = data[1]
	p2 = data[2]
	do j = 1, npts {
	    average[j] = w1*(Mems[p1]/s1-z1) + w2*(Mems[p2]/s2-z2)
	    p1 = p1 + 1
	    p2 = p2 + 1
	}
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    s4 = scales[i+3]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    z4 = zeros[i+3]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    w4 = wts[i+3]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Mems[p1]/s1-z1) +
		    w2*(Mems[p2]/s2-z2) + w3*(Mems[p3]/s3-z3) +
		    w4*(Mems[p4]/s4-z4)
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Mems[p1]/s1-z1) +
		    w2*(Mems[p2]/s2-z2) + w3*(Mems[p3]/s3-z3)
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Mems[p1]/s1-z1) +
		    w2*(Mems[p2]/s2-z2)
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    s1 = scales[i]
	    z1 = zeros[i]
	    w1 = wts[i]
	    p1 = data[i]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Mems[p1]/s1-z1)
	        p1 = p1 + 1
	    }
	}
end

# IMC_AVERAGE -- Combine the input images by averaging.
# Each input image, given by an array of image pointers, is scaled and
# then a weighted average is computed.  The output image header is updated
# to include a scaled and weighted exposure time and the number of images
# combined.

procedure imc_averagei (log, in, out, sig, nimages)

int	log			# Log FD
pointer	in[nimages]		# Input image
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnli()
pointer	impnlr()
errchk	imc_scales, imgnli

begin
	if (nimages == 1) {
	    call imc_copyi (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("average", log, 0., 0., in, out, Memr[scales],
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
		call wtaveragei (Memi[data], Memr[scales], Memr[zeros],
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
		call averagei (Memi[data], nimages, Memr[outdata], nc)
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


# AVERAGE -- Compute the average image line.

procedure averagei (data, nimages, average, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts], nsum	# Average line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aclrr (average, npts)
	i = 1

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        average[j] = average[j] + Memi[p1] + Memi[p2] + Memi[p3] +
		    Memi[p4]
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
	        average[j] = average[j] + Memi[p1] + Memi[p2] + Memi[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        average[j] = average[j] + Memi[p1] + Memi[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    p1 = data[i]
	    do j = 1, npts {
	        average[j] = average[j] + Memi[p1]
	        p1 = p1 + 1
	    }
	}

	nsum = nimages
	call adivkr (average, nsum, average, npts)
end


# WTAVERAGE -- Compute the weighted average image line.  The input data is
# type dependent and the output is real.

procedure wtaveragei (data, scales, zeros, wts, nimages, average, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	average[npts]		# Average line (returned)
int	npts			# Number of data points per line

int	i, j, nleft
real	s1, s2, s3, s4, z1, z2, z3, z4, w1, w2, w3, w4
pointer	p1, p2, p3, p4

begin
	s1 = scales[1]
	s2 = scales[2]
	z1 = zeros[1]
	z2 = zeros[2]
	w1 = wts[1]
	w2 = wts[2]
	p1 = data[1]
	p2 = data[2]
	do j = 1, npts {
	    average[j] = w1*(Memi[p1]/s1-z1) + w2*(Memi[p2]/s2-z2)
	    p1 = p1 + 1
	    p2 = p2 + 1
	}
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    s4 = scales[i+3]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    z4 = zeros[i+3]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    w4 = wts[i+3]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memi[p1]/s1-z1) +
		    w2*(Memi[p2]/s2-z2) + w3*(Memi[p3]/s3-z3) +
		    w4*(Memi[p4]/s4-z4)
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memi[p1]/s1-z1) +
		    w2*(Memi[p2]/s2-z2) + w3*(Memi[p3]/s3-z3)
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memi[p1]/s1-z1) +
		    w2*(Memi[p2]/s2-z2)
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    s1 = scales[i]
	    z1 = zeros[i]
	    w1 = wts[i]
	    p1 = data[i]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memi[p1]/s1-z1)
	        p1 = p1 + 1
	    }
	}
end

# IMC_AVERAGE -- Combine the input images by averaging.
# Each input image, given by an array of image pointers, is scaled and
# then a weighted average is computed.  The output image header is updated
# to include a scaled and weighted exposure time and the number of images
# combined.

procedure imc_averagel (log, in, out, sig, nimages)

int	log			# Log FD
pointer	in[nimages]		# Input image
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnll()
pointer	impnlr()
errchk	imc_scales, imgnll

begin
	if (nimages == 1) {
	    call imc_copyl (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("average", log, 0., 0., in, out, Memr[scales],
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
		call wtaveragel (Memi[data], Memr[scales], Memr[zeros],
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
		call averagel (Memi[data], nimages, Memr[outdata], nc)
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


# AVERAGE -- Compute the average image line.

procedure averagel (data, nimages, average, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts], nsum	# Average line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aclrr (average, npts)
	i = 1

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        average[j] = average[j] + Meml[p1] + Meml[p2] + Meml[p3] +
		    Meml[p4]
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
	        average[j] = average[j] + Meml[p1] + Meml[p2] + Meml[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        average[j] = average[j] + Meml[p1] + Meml[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    p1 = data[i]
	    do j = 1, npts {
	        average[j] = average[j] + Meml[p1]
	        p1 = p1 + 1
	    }
	}

	nsum = nimages
	call adivkr (average, nsum, average, npts)
end


# WTAVERAGE -- Compute the weighted average image line.  The input data is
# type dependent and the output is real.

procedure wtaveragel (data, scales, zeros, wts, nimages, average, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	average[npts]		# Average line (returned)
int	npts			# Number of data points per line

int	i, j, nleft
real	s1, s2, s3, s4, z1, z2, z3, z4, w1, w2, w3, w4
pointer	p1, p2, p3, p4

begin
	s1 = scales[1]
	s2 = scales[2]
	z1 = zeros[1]
	z2 = zeros[2]
	w1 = wts[1]
	w2 = wts[2]
	p1 = data[1]
	p2 = data[2]
	do j = 1, npts {
	    average[j] = w1*(Meml[p1]/s1-z1) + w2*(Meml[p2]/s2-z2)
	    p1 = p1 + 1
	    p2 = p2 + 1
	}
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    s4 = scales[i+3]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    z4 = zeros[i+3]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    w4 = wts[i+3]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Meml[p1]/s1-z1) +
		    w2*(Meml[p2]/s2-z2) + w3*(Meml[p3]/s3-z3) +
		    w4*(Meml[p4]/s4-z4)
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Meml[p1]/s1-z1) +
		    w2*(Meml[p2]/s2-z2) + w3*(Meml[p3]/s3-z3)
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Meml[p1]/s1-z1) +
		    w2*(Meml[p2]/s2-z2)
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    s1 = scales[i]
	    z1 = zeros[i]
	    w1 = wts[i]
	    p1 = data[i]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Meml[p1]/s1-z1)
	        p1 = p1 + 1
	    }
	}
end

# IMC_AVERAGE -- Combine the input images by averaging.
# Each input image, given by an array of image pointers, is scaled and
# then a weighted average is computed.  The output image header is updated
# to include a scaled and weighted exposure time and the number of images
# combined.

procedure imc_averager (log, in, out, sig, nimages)

int	log			# Log FD
pointer	in[nimages]		# Input image
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnlr()
pointer	impnlr()
errchk	imc_scales, imgnlr

begin
	if (nimages == 1) {
	    call imc_copyr (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("average", log, 0., 0., in, out, Memr[scales],
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
		call wtaverager (Memi[data], Memr[scales], Memr[zeros],
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
		call averager (Memi[data], nimages, Memr[outdata], nc)
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


# AVERAGE -- Compute the average image line.

procedure averager (data, nimages, average, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	average[npts], nsum	# Average line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aaddr (Memr[data[1]], Memr[data[2]], average, npts)
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        average[j] = average[j] + Memr[p1] + Memr[p2] + Memr[p3] +
		    Memr[p4]
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
	        average[j] = average[j] + Memr[p1] + Memr[p2] + Memr[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        average[j] = average[j] + Memr[p1] + Memr[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    call aaddr (Memr[data[i]], average, average, npts)
	}

	nsum = nimages
	call adivkr (average, nsum, average, npts)
end


# WTAVERAGE -- Compute the weighted average image line.  The input data is
# type dependent and the output is real.

procedure wtaverager (data, scales, zeros, wts, nimages, average, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
real	average[npts]		# Average line (returned)
int	npts			# Number of data points per line

int	i, j, nleft
real	s1, s2, s3, s4, z1, z2, z3, z4, w1, w2, w3, w4
pointer	p1, p2, p3, p4

begin
	s1 = scales[1]
	s2 = scales[2]
	z1 = zeros[1]
	z2 = zeros[2]
	w1 = wts[1]
	w2 = wts[2]
	p1 = data[1]
	p2 = data[2]
	do j = 1, npts {
	    average[j] = w1*(Memr[p1]/s1-z1) + w2*(Memr[p2]/s2-z2)
	    p1 = p1 + 1
	    p2 = p2 + 1
	}
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    s4 = scales[i+3]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    z4 = zeros[i+3]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    w4 = wts[i+3]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memr[p1]/s1-z1) +
		    w2*(Memr[p2]/s2-z2) + w3*(Memr[p3]/s3-z3) +
		    w4*(Memr[p4]/s4-z4)
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memr[p1]/s1-z1) +
		    w2*(Memr[p2]/s2-z2) + w3*(Memr[p3]/s3-z3)
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memr[p1]/s1-z1) +
		    w2*(Memr[p2]/s2-z2)
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    s1 = scales[i]
	    z1 = zeros[i]
	    w1 = wts[i]
	    p1 = data[i]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memr[p1]/s1-z1)
	        p1 = p1 + 1
	    }
	}
end

# IMC_AVERAGE -- Combine the input images by averaging.
# Each input image, given by an array of image pointers, is scaled and
# then a weighted average is computed.  The output image header is updated
# to include a scaled and weighted exposure time and the number of images
# combined.

procedure imc_averaged (log, in, out, sig, nimages)

int	log			# Log FD
pointer	in[nimages]		# Input image
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnld()
pointer	impnld()
errchk	imc_scales, imgnld

begin
	if (nimages == 1) {
	    call imc_copyd (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("average", log, 0., 0., in, out, Memr[scales],
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
		call wtaveraged (Memi[data], Memr[scales], Memr[zeros],
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
		call averaged (Memi[data], nimages, Memd[outdata], nc)
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


# AVERAGE -- Compute the average image line.

procedure averaged (data, nimages, average, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
double	average[npts], nsum	# Average line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aaddd (Memd[data[1]], Memd[data[2]], average, npts)
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        average[j] = average[j] + Memd[p1] + Memd[p2] + Memd[p3] +
		    Memd[p4]
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
	        average[j] = average[j] + Memd[p1] + Memd[p2] + Memd[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        average[j] = average[j] + Memd[p1] + Memd[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    call aaddd (Memd[data[i]], average, average, npts)
	}

	nsum = nimages
	call adivkd (average, nsum, average, npts)
end


# WTAVERAGE -- Compute the weighted average image line.  The input data is
# type dependent and the output is real.

procedure wtaveraged (data, scales, zeros, wts, nimages, average, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
double	average[npts]		# Average line (returned)
int	npts			# Number of data points per line

int	i, j, nleft
real	s1, s2, s3, s4, z1, z2, z3, z4, w1, w2, w3, w4
pointer	p1, p2, p3, p4

begin
	s1 = scales[1]
	s2 = scales[2]
	z1 = zeros[1]
	z2 = zeros[2]
	w1 = wts[1]
	w2 = wts[2]
	p1 = data[1]
	p2 = data[2]
	do j = 1, npts {
	    average[j] = w1*(Memd[p1]/s1-z1) + w2*(Memd[p2]/s2-z2)
	    p1 = p1 + 1
	    p2 = p2 + 1
	}
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    s4 = scales[i+3]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    z4 = zeros[i+3]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    w4 = wts[i+3]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memd[p1]/s1-z1) +
		    w2*(Memd[p2]/s2-z2) + w3*(Memd[p3]/s3-z3) +
		    w4*(Memd[p4]/s4-z4)
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memd[p1]/s1-z1) +
		    w2*(Memd[p2]/s2-z2) + w3*(Memd[p3]/s3-z3)
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memd[p1]/s1-z1) +
		    w2*(Memd[p2]/s2-z2)
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    s1 = scales[i]
	    z1 = zeros[i]
	    w1 = wts[i]
	    p1 = data[i]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memd[p1]/s1-z1)
	        p1 = p1 + 1
	    }
	}
end

# IMC_AVERAGE -- Combine the input images by averaging.
# Each input image, given by an array of image pointers, is scaled and
# then a weighted average is computed.  The output image header is updated
# to include a scaled and weighted exposure time and the number of images
# combined.

procedure imc_averagex (log, in, out, sig, nimages)

int	log			# Log FD
pointer	in[nimages]		# Input image
pointer	out			# Output image
pointer	sig			# Sigma image
int	nimages			# Number of input images

int	i, j, nc
pointer	sp, data, scales, zeros, wts, outdata, sigdata, v1, v2
bool	scale, imc_scales()
pointer	imgnlx()
pointer	impnlx()
errchk	imc_scales, imgnlx

begin
	if (nimages == 1) {
	    call imc_copyx (in[1], out)
	    return
	}

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Get the scaling factors and weights.
	scale = imc_scales ("average", log, 0., 0., in, out, Memr[scales],
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
		call wtaveragex (Memi[data], Memr[scales], Memr[zeros],
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
		call averagex (Memi[data], nimages, Memx[outdata], nc)
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


# AVERAGE -- Compute the average image line.

procedure averagex (data, nimages, average, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
complex	average[npts], nsum	# Average line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aaddx (Memx[data[1]], Memx[data[2]], average, npts)
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        average[j] = average[j] + Memx[p1] + Memx[p2] + Memx[p3] +
		    Memx[p4]
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
	        average[j] = average[j] + Memx[p1] + Memx[p2] + Memx[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        average[j] = average[j] + Memx[p1] + Memx[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    call aaddx (Memx[data[i]], average, average, npts)
	}

	nsum = nimages
	call adivkx (average, nsum, average, npts)
end


# WTAVERAGE -- Compute the weighted average image line.  The input data is
# type dependent and the output is real.

procedure wtaveragex (data, scales, zeros, wts, nimages, average, npts)

pointer	data[nimages]		# IMIO data pointers
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero levels
real	wts[nimages]		# Weights
int	nimages			# Number of data lines
complex	average[npts]		# Average line (returned)
int	npts			# Number of data points per line

int	i, j, nleft
real	s1, s2, s3, s4, z1, z2, z3, z4, w1, w2, w3, w4
pointer	p1, p2, p3, p4

begin
	s1 = scales[1]
	s2 = scales[2]
	z1 = zeros[1]
	z2 = zeros[2]
	w1 = wts[1]
	w2 = wts[2]
	p1 = data[1]
	p2 = data[2]
	do j = 1, npts {
	    average[j] = w1*(Memx[p1]/s1-z1) + w2*(Memx[p2]/s2-z2)
	    p1 = p1 + 1
	    p2 = p2 + 1
	}
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    s4 = scales[i+3]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    z4 = zeros[i+3]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    w4 = wts[i+3]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memx[p1]/s1-z1) +
		    w2*(Memx[p2]/s2-z2) + w3*(Memx[p3]/s3-z3) +
		    w4*(Memx[p4]/s4-z4)
	        p1 = p1 + 1
	        p2 = p2 + 1
	        p3 = p3 + 1
	        p4 = p4 + 1
	    }
	    i = i + 4
	    nleft = nleft - 4
	}
	if (nleft == 3) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    s3 = scales[i+2]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    z3 = zeros[i+2]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    w3 = wts[i+2]
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memx[p1]/s1-z1) +
		    w2*(Memx[p2]/s2-z2) + w3*(Memx[p3]/s3-z3)
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    s1 = scales[i]
	    s2 = scales[i+1]
	    z1 = zeros[i]
	    z2 = zeros[i+1]
	    w1 = wts[i]
	    w2 = wts[i+1]
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memx[p1]/s1-z1) +
		    w2*(Memx[p2]/s2-z2)
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    s1 = scales[i]
	    z1 = zeros[i]
	    w1 = wts[i]
	    p1 = data[i]
	    do j = 1, npts {
		average[j] = average[j] + w1*(Memx[p1]/s1-z1)
	        p1 = p1 + 1
	    }
	}
end

