# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help imcsum
.nf ----------------------------------------------------------------------------
                COMBINING IMAGES: SUMMING ALGORITHM

The input images are summed.  The exposure time of the output image is the
sum of the input exposure times.  There is no checking for overflow.
.endhelp -----------------------------------------------------------------------


# IMC_SUM -- Sum the input images.


procedure imc_sums (log, in, out, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
pointer	out			# Output IMIO pointer
int	nimages			# Number of input images

int	i, j, npts
pointer	sp, data, outdata, v1, v2, imgnls()
pointer	impnlr()

begin
	call imc_logsum ("sum", log, in, out, nimages)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	npts = IM_LEN(out,1)
	while (impnlr (out, outdata, Meml[v1]) != EOF) {
	    do i = 1, nimages {
	        call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	        j = imgnls (in[i], Memi[data+i-1], Meml[v1])
	    }
	    call sums (Memi[data], nimages, Memr[outdata], npts)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end


# SUM -- Compute the summed image line.

procedure sums (data, nimages, sum, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	sum[npts]		# Summed line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aclrr (sum, npts)
	i = 1

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        sum[j] = sum[j] + Mems[p1] + Mems[p2] + Mems[p3] + Mems[p4]
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
	        sum[j] = sum[j] + Mems[p1] + Mems[p2] + Mems[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        sum[j] = sum[j] + Mems[p1] + Mems[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    p1 = data[i]
	    do j = 1, npts {
	        sum[j] = sum[j] + Mems[p1]
	        p1 = p1 + 1
	    }
	}
end

procedure imc_sumi (log, in, out, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
pointer	out			# Output IMIO pointer
int	nimages			# Number of input images

int	i, j, npts
pointer	sp, data, outdata, v1, v2, imgnli()
pointer	impnlr()

begin
	call imc_logsum ("sum", log, in, out, nimages)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	npts = IM_LEN(out,1)
	while (impnlr (out, outdata, Meml[v1]) != EOF) {
	    do i = 1, nimages {
	        call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	        j = imgnli (in[i], Memi[data+i-1], Meml[v1])
	    }
	    call sumi (Memi[data], nimages, Memr[outdata], npts)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end


# SUM -- Compute the summed image line.

procedure sumi (data, nimages, sum, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	sum[npts]		# Summed line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aclrr (sum, npts)
	i = 1

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memi[p1] + Memi[p2] + Memi[p3] + Memi[p4]
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
	        sum[j] = sum[j] + Memi[p1] + Memi[p2] + Memi[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memi[p1] + Memi[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    p1 = data[i]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memi[p1]
	        p1 = p1 + 1
	    }
	}
end

procedure imc_suml (log, in, out, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
pointer	out			# Output IMIO pointer
int	nimages			# Number of input images

int	i, j, npts
pointer	sp, data, outdata, v1, v2, imgnll()
pointer	impnlr()

begin
	call imc_logsum ("sum", log, in, out, nimages)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	npts = IM_LEN(out,1)
	while (impnlr (out, outdata, Meml[v1]) != EOF) {
	    do i = 1, nimages {
	        call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	        j = imgnll (in[i], Memi[data+i-1], Meml[v1])
	    }
	    call suml (Memi[data], nimages, Memr[outdata], npts)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end


# SUM -- Compute the summed image line.

procedure suml (data, nimages, sum, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	sum[npts]		# Summed line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aclrr (sum, npts)
	i = 1

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        sum[j] = sum[j] + Meml[p1] + Meml[p2] + Meml[p3] + Meml[p4]
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
	        sum[j] = sum[j] + Meml[p1] + Meml[p2] + Meml[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        sum[j] = sum[j] + Meml[p1] + Meml[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    p1 = data[i]
	    do j = 1, npts {
	        sum[j] = sum[j] + Meml[p1]
	        p1 = p1 + 1
	    }
	}
end

procedure imc_sumr (log, in, out, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
pointer	out			# Output IMIO pointer
int	nimages			# Number of input images

int	i, j, npts
pointer	sp, data, outdata, v1, v2, imgnlr()
pointer	impnlr()

begin
	call imc_logsum ("sum", log, in, out, nimages)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	npts = IM_LEN(out,1)
	while (impnlr (out, outdata, Meml[v1]) != EOF) {
	    do i = 1, nimages {
	        call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	        j = imgnlr (in[i], Memi[data+i-1], Meml[v1])
	    }
	    call sumr (Memi[data], nimages, Memr[outdata], npts)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end


# SUM -- Compute the summed image line.

procedure sumr (data, nimages, sum, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
real	sum[npts]		# Summed line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aaddr (Memr[data[1]], Memr[data[2]], sum, npts)
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memr[p1] + Memr[p2] + Memr[p3] + Memr[p4]
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
	        sum[j] = sum[j] + Memr[p1] + Memr[p2] + Memr[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memr[p1] + Memr[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    call aaddr (Memr[data[i]], sum, sum, npts)
	}
end

procedure imc_sumd (log, in, out, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
pointer	out			# Output IMIO pointer
int	nimages			# Number of input images

int	i, j, npts
pointer	sp, data, outdata, v1, v2, imgnld()
pointer	impnld()

begin
	call imc_logsum ("sum", log, in, out, nimages)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	npts = IM_LEN(out,1)
	while (impnld (out, outdata, Meml[v1]) != EOF) {
	    do i = 1, nimages {
	        call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	        j = imgnld (in[i], Memi[data+i-1], Meml[v1])
	    }
	    call sumd (Memi[data], nimages, Memd[outdata], npts)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end


# SUM -- Compute the summed image line.

procedure sumd (data, nimages, sum, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
double	sum[npts]		# Summed line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aaddd (Memd[data[1]], Memd[data[2]], sum, npts)
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memd[p1] + Memd[p2] + Memd[p3] + Memd[p4]
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
	        sum[j] = sum[j] + Memd[p1] + Memd[p2] + Memd[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memd[p1] + Memd[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    call aaddd (Memd[data[i]], sum, sum, npts)
	}
end

procedure imc_sumx (log, in, out, nimages)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
pointer	out			# Output IMIO pointer
int	nimages			# Number of input images

int	i, j, npts
pointer	sp, data, outdata, v1, v2, imgnlx()
pointer	impnlx()

begin
	call imc_logsum ("sum", log, in, out, nimages)

	call smark (sp)
	call salloc (data, nimages, TY_INT)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	npts = IM_LEN(out,1)
	while (impnlx (out, outdata, Meml[v1]) != EOF) {
	    do i = 1, nimages {
	        call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	        j = imgnlx (in[i], Memi[data+i-1], Meml[v1])
	    }
	    call sumx (Memi[data], nimages, Memx[outdata], npts)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end


# SUM -- Compute the summed image line.

procedure sumx (data, nimages, sum, npts)

pointer	data[nimages]		# Data pointers
int	nimages			# Number of images
complex	sum[npts]		# Summed line (returned)
int	npts			# Number of data points

int	i, j, nleft
pointer	p1, p2, p3, p4

begin
	call aaddx (Memx[data[1]], Memx[data[2]], sum, npts)
	i = 3

	nleft = nimages - i + 1
	while (nleft > 3) {
	    p1 = data[i]
	    p2 = data[i+1]
	    p3 = data[i+2]
	    p4 = data[i+3]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memx[p1] + Memx[p2] + Memx[p3] + Memx[p4]
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
	        sum[j] = sum[j] + Memx[p1] + Memx[p2] + Memx[p3]
		p1 = p1 + 1
		p2 = p2 + 1
		p3 = p3 + 1
	    }
	} else if (nleft == 2) {
	    p1 = data[i]
	    p2 = data[i+1]
	    do j = 1, npts {
	        sum[j] = sum[j] + Memx[p1] + Memx[p2]
	        p1 = p1 + 1
	        p2 = p2 + 1
	    }
	} else if (nleft == 1) {
	    call aaddx (Memx[data[i]], sum, sum, npts)
	}
end

