# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"../shdr.h"
include	"icombine.h"

# IC_LOG -- Output log information is a log file has been specfied.

procedure ic_log (sh, shout, ncombine, exptime, mode, median, mean, scales,
	zeros, wts, nimages, dozero, nout, expname, exposure)

pointer	sh[nimages]		# Input spectra
pointer	shout			# Output spectrum
int	ncombine[nimages]	# Number of previous combined images
real	exptime[nimages]	# Exposure times
real	mode[nimages]		# Modes
real	median[nimages]		# Medians
real	mean[nimages]		# Means
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero or sky levels
real	wts[nimages]		# Weights
int	nimages			# Number of images
bool	dozero			# Zero flag
int	nout			# Number of images combined in output
char	expname[ARB]		# Exposure name
real	exposure		# Output exposure

int	i, j, ctor()
real	rval
long	clktime()
bool	prncombine, prexptime, prmode, prmedian, prmean, prrdn, prgain
pointer	sp, fname

include	"icombine.com"

begin
	if (logfd == NULL)
	    return

	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)

	# Time stamp the log and print parameter information.

	call cnvdate (clktime(0), Memc[fname], SZ_LINE)
	call fprintf (logfd, "\n%s: SCOMBINE\n")
	    call pargstr (Memc[fname])
	switch (combine) {
	case AVERAGE:
	    call fprintf (logfd, "  combine = average\n")
	case MEDIAN:
	    call fprintf (logfd, "  combine = median\n")
	case SUM:
	    call fprintf (logfd, "  combine = sum\n")
	}
	switch (reject) {
	case MINMAX:
	    call fprintf (logfd, "  reject = minmax, nlow = %d, nhigh = %d\n")
		call pargi (nint (flow * nimages))
		call pargi (nint (fhigh * nimages))
	case CCDCLIP:
	    call fprintf (logfd,
		"  reject = ccdclip, mclip = %b, rdnoise = %s, gain = %s\n")
		call pargb (mclip)
		call pargstr (Memc[rdnoise])
		call pargstr (Memc[gain])
	    call fprintf (logfd, "  lsigma = %g, hsigma = %g\n")
		call pargr (lsigma)
		call pargr (hsigma)
	case CRREJECT:
	    call fprintf (logfd,
	    "  reject = crreject, mclip = %b, rdnoise = %s, gain = %s, hsigma = %g\n")
		call pargb (mclip)
		call pargstr (Memc[rdnoise])
		call pargstr (Memc[gain])
		call pargr (hsigma)
	case PCLIP:
	    call fprintf (logfd,
	    "  reject = pclip, pclip = %g, lsigma = %g, hsigma = %g\n")
		call pargr (pclip)
		call pargr (lsigma)
		call pargr (hsigma)
	case SIGCLIP:
	    call fprintf (logfd,
		"  reject = sigclip, mclip = %b, lsigma = %g, hsigma = %g\n")
		call pargb (mclip)
		call pargr (lsigma)
		call pargr (hsigma)
	case AVSIGCLIP:
	    call fprintf (logfd,
		"  reject = avsigclip, mclip = %b, lsigma = %g, hsigma = %g\n")
		call pargb (mclip)
		call pargr (lsigma)
		call pargr (hsigma)
	}
	if (reject != NONE && grow > 0) {
	    call fprintf (logfd, "  grow = %d\n")
		call pargi (grow)
	}
	if (dothresh) {
	    if (lthresh > -MAX_REAL && hthresh < MAX_REAL) {
		call fprintf (logfd, "  lthreshold = %g, hthreshold = %g\n")
		    call pargr (lthresh)
		    call pargr (hthresh)
	    } else if (lthresh > -MAX_REAL) {
		call fprintf (logfd, "  lthreshold = %g\n")
		    call pargr (lthresh)
	    } else {
		call fprintf (logfd, "  hthreshold = %g\n")
		    call pargr (hthresh)
	    }
	}
	call fprintf (logfd, "  blank = %g\n")
	    call pargr (blank)
	call clgstr ("sample", Memc[fname], SZ_LINE)
	if (Memc[fname] != EOS) {
	    call fprintf (logfd, "  sample = %s\n")
		call pargstr (Memc[fname])
	}

	# Print information pertaining to individual images as a set of
	# columns with the image name being the first column.  Determine
	# what information is relevant and print the appropriate header.

	prncombine = false
	prexptime = false
	prmode = false
	prmedian = false
	prmean = false
	prrdn = false
	prgain = false
	do i = 1, nimages {
	    if (ncombine[i] != ncombine[1])
		prncombine = true
	    if (exptime[i] != exptime[1])
		prexptime = true
	    if (mode[i] != mode[1])
		prmode = true
	    if (median[i] != median[1])
		prmedian = true
	    if (mean[i] != mean[1])
		prmean = true
	    if (reject == CCDCLIP || reject == CRREJECT) {
		j = 1
		if (ctor (Memc[rdnoise], j, rval) == 0)
		    prrdn = true
		j = 1
		if (ctor (Memc[gain], j, rval) == 0)
		    prgain = true
	    }
	}

	call fprintf (logfd, "  %20s ")
	    call pargstr ("Images")
	if (prncombine) {
	    call fprintf (logfd, " %6s")
		call pargstr ("N")
	}
	if (prexptime) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Exp")
	}
	if (prmode) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Mode")
	}
	if (prmedian) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Median")
	}
	if (prmean) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Mean")
	}
	if (prrdn) {
	    call fprintf (logfd, " %7s")
		call pargstr ("Rdnoise")
	}
	if (prgain) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Gain")
	}
	if (doscale) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Scale")
	}
	if (dozero) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Zero")
	}
	if (dowts) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Weight")
	}
	call fprintf (logfd, "\n")

	do i = 1, nimages {
	    call fprintf (logfd, "  %16s[%3d]")
		call pargstr (SPECTRUM(sh[i]))
		call pargi (AP(sh[i]))
	    if (prncombine) {
		call fprintf (logfd, " %6d")
		    call pargi (ncombine[i])
	    }
	    if (prexptime) {
		call fprintf (logfd, " %6.1f")
		    call pargr (exptime[i])
	    }
	    if (prmode) {
		call fprintf (logfd, " %6g")
		    call pargr (mode[i])
	    }
	    if (prmedian) {
		call fprintf (logfd, " %6g")
		    call pargr (median[i])
	    }
	    if (prmean) {
		call fprintf (logfd, " %6g")
		    call pargr (mean[i])
	    }
	    if (prrdn) {
		call fprintf (logfd, " %7g")
		    call pargr (RA(sh[i]))
	    }
	    if (prgain) {
		call fprintf (logfd, " %6g")
		    call pargr (DEC(sh[i]))
	    }
	    if (doscale) {
		call fprintf (logfd, " %6.3f")
		    call pargr (1./scales[i])
	    }
	    if (dozero) {
		call fprintf (logfd, " %6g")
		    call pargr (-zeros[i])
	    }
	    if (dowts) {
		call fprintf (logfd, " %6.3f")
		    call pargr (wts[i])
	    }
	    call fprintf (logfd, "\n")
	}

	# Log information about the output images.
	call fprintf (logfd, "\n  Output image = %s, ncombine = %d")
	    call pargstr (SPECTRUM(shout))
	    call pargi (nout)
	if (expname[1] != EOS) {
	    call fprintf (logfd, ", %s = %g")
		call pargstr (expname)
		call pargr (exposure)
	}
	call fprintf (logfd, "\n")
	call fprintf (logfd,
	    "  w1 = %g, w2 = %g, dw = %g, nw = %g, dtype = %d\n")
	    call pargr (W0(shout))
	    call pargr (W1(shout))
	    call pargr (WP(shout))
	    call pargi (SN(shout))
	    call pargi (DC(shout))

	call flush (logfd)
	call sfree (sp)
end
