include	<imhdr.h>
include	<imset.h>
include	<time.h>

# IMC_SCALES -- Get the scale factors for the images.
# 1. This procedure does CLIO to determine the type of scaling desired.
# 2. The output header parameters for exposure time, dark time, and
#    NCOMBINE are set.
# 3. The scaling and weighting factors are logged.  The logging is done
#    here because some of the information is only available here.
# 4. It is an error if unable to write to a specified log file.

bool procedure imc_scales (str, log, low, high, in, out, scales, zeros, wts,
	nimages)

char	str[ARB]		# Log string
int	log			# Log file descriptor
real	low, high		# Rejection limits
pointer	in[nimages]		# Input images
pointer	out			# Output image
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero or sky levels
real	wts[nimages]		# Weights
int	nimages			# Number of images

int	i, nout
real	mean, exposure, darktime, dark
pointer	sp, ncombine, exptime, modes, time, fname, x1, x2, xs
bool	scale, expscale, modescale, modeoffset, weight

bool	clgetb()
long	clktime()
int	hdmgeti(), open()
short	imc_modes()
real	hdmgetr(), imc_moder(), asumr(), asumi()
errchk	open

begin
	call smark (sp)
	call salloc (ncombine, nimages, TY_INT)
	call salloc (exptime, nimages, TY_REAL)
	call salloc (modes, nimages, TY_REAL)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (x1, IM_MAXDIM, TY_INT)
	call salloc (x2, IM_MAXDIM, TY_INT)
	call salloc (xs, IM_MAXDIM, TY_INT)

	# Determine type of scaling desired.
	expscale = clgetb ("exposure")
	modescale = clgetb ("scale")
	modeoffset = clgetb ("offset")
	weight = clgetb ("weight")
	call clgstr ("modesec", Memc[fname], SZ_FNAME)

	# Get the number of images previously combined and the exposure times.
	# The default combine number is 1 and the default exposure is 0.

	do i = 1, nimages {
	    iferr (Memi[ncombine+i-1] = hdmgeti (in[i], "ncombine"))
		Memi[ncombine+i-1] = 1
	    iferr (Memr[exptime+i-1] = hdmgetr (in[i], "exptime"))
		Memr[exptime+i-1] = 0.
	}

	# Set the default scaling factors.
	call amovkr (INDEF, Memr[modes], nimages)
	call amovkr (1., scales, nimages)
	call amovkr (0., zeros, nimages)
	call amovkr (1., wts, nimages)

	# Set scaling factors.  Mode scaling overrides exposure scaline and
	# offset scaling.

	if (modescale) {
	    call amovki (1, Memi[x1], IM_NDIM(in[1]))
	    call amovi (IM_LEN(in[1],1), Memi[x2], IM_NDIM(in[1]))
	    call amovki (1, Memi[xs], IM_NDIM(in[1]))
	    call imc_section (Memc[fname], Memi[x1], Memi[x2], Memi[xs],
		IM_NDIM(in[1]))
	    do i = 1, nimages {
		switch (IM_PIXTYPE(in[i])) {
		case TY_SHORT:
		    Memr[modes+i-1] = imc_modes (in[i], Memi[x1], Memi[x2],
			Memi[xs])
		default:
		    Memr[modes+i-1] = imc_moder (in[i], Memi[x1], Memi[x2],
			Memi[xs])
		}
		scales[i] = Memr[modes+i-1]
		if (scales[i] <= 0.)
		    call error (0, "Mode must be positive for scaling")
		if (weight)
	            wts[i] = sqrt (Memi[ncombine+i-1] * scales[i])
	    }
	} else {
	    if (expscale)
	        do i = 1, nimages {
		    scales[i] =  max (0.001, Memr[exptime+i-1])
		    if (weight)
	                wts[i] = sqrt (Memi[ncombine+i-1] * scales[i])
		}
	    if (modeoffset) {
	        call amovki (1, Memi[x1], IM_NDIM(in[1]))
	        call amovi (IM_LEN(in[1],1), Memi[x2], IM_NDIM(in[1]))
	        call amovki (1, Memi[xs], IM_NDIM(in[1]))
	        call imc_section (Memc[fname], Memi[x1], Memi[x2], Memi[xs],
		    IM_NDIM(in[1]))
	        do i = 1, nimages {
		    switch (IM_PIXTYPE(in[i])) {
		    case TY_SHORT:
		        Memr[modes+i-1] = imc_modes (in[i], Memi[x1], Memi[x2],
			    Memi[xs])
		    default:
		        Memr[modes+i-1] = imc_moder (in[i], Memi[x1], Memi[x2],
			    Memi[xs])
		    }
		    zeros[i] = Memr[modes+i-1] / scales[i]
		    if (weight) {
			if (zeros[i] <= 0.)
		            call error (0,
				"Mode must be positive for weighting")
		        wts[i] = sqrt (Memi[ncombine+i-1]*scales[i]/zeros[i])
		    }
	        }
	    }
	}

	# Change to relative scaling factors.
	mean = asumr (zeros, nimages) / nimages
	call asubkr (zeros, mean, zeros, nimages)
	mean = asumr (scales, nimages) / nimages
	call adivkr (scales, mean, scales, nimages)
	call amulkr (zeros, mean, zeros, nimages)
	mean = asumr (wts, nimages)
	call adivkr (wts, mean, wts, nimages)

	# Because of finite arithmetic it is possible for the offsets to
	# be nonzero even when they are all equal.  Just for the sake of
	# a nice log set the offsets in this case.

	for (i=2; (i<=nimages)&&(zeros[i]==zeros[1]); i=i+1)
	    ;
	if (i > nimages)
	    call aclrr (zeros, nimages)

	# If all scale factors, offsets, and weights are equal then
	# don't actually scale.

	for (i=2; (i<=nimages)&&(scales[i]==scales[1]); i=i+1)
	    if ((zeros[i] != zeros[1]) || (wts[i] != wts[1]))
		break
	if (i > nimages)
	    scale = false
	else
	    scale = true

	# Set the output header parameters.
	nout = asumi (Memi[ncombine], nimages)
	exposure = 0.
	darktime = 0.
	do i = 1, nimages {
	    exposure = exposure + wts[i] * Memr[exptime+i-1] / scales[i]
	    ifnoerr (dark = hdmgetr (in[i], "darktime"))
		darktime = darktime + wts[i] * dark / scales[i]
	    else
		darktime = darktime + wts[i] * Memr[exptime+i-1] / scales[i]
	}

	call hdmputi (out, "ncombine", nout)
	call hdmputr (out, "exptime", exposure)
	call hdmputr (out, "darktime", darktime)

	# Log the scaling.  If verbose log to the standard output.  If a
	# logfile is specified log to the file.

	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	if (clgetb ("verbose")) {
	    if ((low > 0.) || (high > 0.)) {
	        call printf ("%s combine: %s, lowreject=%g, highreject=%g\n")
		    call pargstr (Memc[time])
		    call pargstr (str)
		    call pargr (low)
		    call pargr (high)
	    } else {
	        call printf ("%s combine: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (str)
	    }
	    call printf ("  %20s %6s %6s %6s %6s %6s %6s\n")
	        call pargstr ("Images")
	        call pargstr ("N")
	        call pargstr ("Exp")
	        call pargstr ("Mode")
	        call pargstr ("Scale")
	        call pargstr ("Offset")
	        call pargstr ("Weight")
	    do i = 1, nimages {
		call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call printf ("  %20s %6d %6.1f %6g %6.3f %6g %6.3f\n")
		    call pargstr (Memc[fname])
		    call pargi (Memi[ncombine+i-1])
		    call pargr (Memr[exptime+i-1])
		    call pargr (Memr[modes+i-1])
		    call pargr (1./scales[i])
		    call pargr (-zeros[i])
		    call pargr (wts[i])
	    }
	    call printf ("  -------------------- ------ ------\n")
	    call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	    call printf ("  %20s %6d %6.1f\n")
		call pargstr (Memc[fname])
	        call pargi (nout)
	        call pargr (exposure)
	    call flush (STDOUT)
	}
		    
	# Append to the "logfile" if not null.
	call clgstr ("logfile", Memc[fname], SZ_FNAME)
	call xt_stripwhite (Memc[fname])
	if (Memc[fname] != EOS) {
	    log = open (Memc[fname], APPEND, TEXT_FILE)
	    if ((low > 0.) || (high > 0.)) {
	        call fprintf (log,
		    "%s combine: %s, lowreject=%g, highreject=%g\n")
		    call pargstr (Memc[time])
		    call pargstr (str)
		    call pargr (low)
		    call pargr (high)
	    } else {
	        call fprintf (log, "%s combine: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (str)
	    }
	    call fprintf (log, "  %20s %6s %6s %6s %6s %6s %6s\n")
	        call pargstr ("Images")
	        call pargstr ("N")
	        call pargstr ("Exp")
	        call pargstr ("Mode")
	        call pargstr ("Scale")
	        call pargstr ("Offset")
	        call pargstr ("Weight")
	    do i = 1, nimages {
		call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s %6d %6.1f %6g %6.3f %6g %6.3f\n")
		    call pargstr (Memc[fname])
		    call pargi (Memi[ncombine+i-1])
		    call pargr (Memr[exptime+i-1])
		    call pargr (Memr[modes+i-1])
		    call pargr (1./scales[i])
		    call pargr (-zeros[i])
		    call pargr (wts[i])
	    }
	    call fprintf (log, "  -------------------- ------ ------\n")
	    call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	    call fprintf (log, "  %20s %6d %6.1f\n")
		call pargstr (Memc[fname])
	        call pargi (nout)
	        call pargr (exposure)
	    call flush (log)
	    call close (log)
	}

	call sfree (sp)
	return (scale)
end
