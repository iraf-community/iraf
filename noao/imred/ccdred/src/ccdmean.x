include	<imhdr.h>


# CCDMEAN -- Compute mean and add to header if needed.

procedure ccdmean (input)

char	input[ARB]		# Input image

int	i, nc, nl, hdmgeti()
long	time, clktime()
bool	clgetb()
real	mean, hdmgetr(), asumr()
pointer	in, immap(), imgl2r()
errchk	immap

begin
	# Check if this operation has been done.

	in = immap (input, READ_WRITE, 0)
	ifnoerr (mean = hdmgetr (in, "ccdmean")) {
	    iferr (time = hdmgeti (in, "ccdmeant"))
		time = IM_MTIME(in)
	    if (time >= IM_MTIME(in)) {
		call imunmap (in)
		return
	    }
	}

	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Compute mean of image\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	# Compute and record the mean.
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	mean = 0.
	do i = 1, nl
	    mean = mean + asumr (Memr[imgl2r(in,i)], nc)
	mean = mean / (nc * nl)
	time = clktime (long(0))
	call hdmputr (in, "ccdmean", mean)
	call hdmputi (in, "ccdmeant", int (time))

	call imunmap (in)
end
