# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<mach.h>
include	"icombine.h"
include	"icmask.h"

# IC_LOG -- Output log information is a log file has been specfied.

procedure ic_log (in, out, ncombine, exptime, sname, zname, wname,
	mode, median, mean, scales, zeros, wts, offsets, nimages,
	dozero, nout)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
int	ncombine[nimages]	# Number of previous combined images
real	exptime[nimages]	# Exposure times
char	sname[ARB]		# Scale name
char	zname[ARB]		# Zero name
char	wname[ARB]		# Weight name
real	mode[nimages]		# Modes
real	median[nimages]		# Medians
real	mean[nimages]		# Means
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero or sky levels
real	wts[nimages]		# Weights
int	offsets[nimages,ARB]	# Image offsets
int	nimages			# Number of images
bool	dozero			# Zero flag
int	nout			# Number of images combined in output

int	i, j, stack, ctor()
real	rval, imgetr()
long	clktime()
bool	prncombine, prexptime, prmode, prmedian, prmean, prmask
bool	prrdn, prgain, prsn
pointer	sp, fname, bpname, key
errchk	imgetr

include	"icombine.com"

begin
	if (logfd == NULL)
	    return

	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (bpname, SZ_LINE, TY_CHAR)

	stack = NO
	if (project) {
	    ifnoerr (call imgstr (in[1], "stck0001", Memc[fname], SZ_LINE))
	        stack = YES
	}
	if (stack == YES)
	    call salloc (key, SZ_FNAME, TY_CHAR)

	# Time stamp the log and print parameter information.

	call cnvdate (clktime(0), Memc[fname], SZ_LINE)
	call fprintf (logfd, "\n%s: IMCOMBINE\n")
	    call pargstr (Memc[fname])
	switch (combine) {
	case  AVERAGE:
	    call fprintf (logfd, "  combine = average, ")
	case  MEDIAN:
	    call fprintf (logfd, "  combine = median, ")
	case  SUM:
	    call fprintf (logfd, "  combine = sum, ")
	}
	call fprintf (logfd, "scale = %s, zero = %s, weight = %s\n")
	    call pargstr (sname)
	    call pargstr (zname)
	    call pargstr (wname)
	if (combine == NMODEL && reject!=CCDCLIP && reject!=CRREJECT) {
	    call fprintf (logfd,
	    "  rdnoise = %s, gain = %s, snoise = %s\n")
		call pargstr (Memc[rdnoise])
		call pargstr (Memc[gain])
		call pargstr (Memc[snoise])
	}

	switch (reject) {
	case MINMAX:
	    call fprintf (logfd, "  reject = minmax, nlow = %d, nhigh = %d\n")
		call pargi (nint (flow * nimages))
		call pargi (nint (fhigh * nimages))
	case CCDCLIP:
	    call fprintf (logfd, "  reject = ccdclip, mclip = %b, nkeep = %d\n")
		call pargb (mclip)
		call pargi (nkeep)
	    call fprintf (logfd,
	    "  rdnoise = %s, gain = %s, snoise = %s, sigma = %g, hsigma = %g\n")
		call pargstr (Memc[rdnoise])
		call pargstr (Memc[gain])
		call pargstr (Memc[snoise])
		call pargr (lsigma)
		call pargr (hsigma)
	case CRREJECT:
	    call fprintf (logfd,
		"  reject = crreject, mclip = %b, nkeep = %d\n")
		call pargb (mclip)
		call pargi (nkeep)
	    call fprintf (logfd,
	    "  rdnoise = %s, gain = %s, snoise = %s, hsigma = %g\n")
		call pargstr (Memc[rdnoise])
		call pargstr (Memc[gain])
		call pargstr (Memc[snoise])
		call pargr (hsigma)
	case PCLIP:
	    call fprintf (logfd, "  reject = pclip, nkeep = %d\n")
		call pargi (nkeep)
	    call fprintf (logfd, "  pclip = %g, lsigma = %g, hsigma = %g\n")
		call pargr (pclip)
		call pargr (lsigma)
		call pargr (hsigma)
	case SIGCLIP:
	    call fprintf (logfd, "  reject = sigclip, mclip = %b, nkeep = %d\n")
		call pargb (mclip)
		call pargi (nkeep)
	    call fprintf (logfd, "  lsigma = %g, hsigma = %g\n")
		call pargr (lsigma)
		call pargr (hsigma)
	case AVSIGCLIP:
	    call fprintf (logfd,
		"  reject = avsigclip, mclip = %b, nkeep = %d\n")
		call pargb (mclip)
		call pargi (nkeep)
	    call fprintf (logfd, "  lsigma = %g, hsigma = %g\n")
		call pargr (lsigma)
		call pargr (hsigma)
	}
	if (reject != NONE && grow >= 1.) {
	    call fprintf (logfd, "  grow = %g\n")
		call pargr (grow)
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
	if (Memc[statsec] != EOS) {
	    call fprintf (logfd, "  statsec = %s\n")
		call pargstr (Memc[fname])
	}

	if (ICM_TYPE(icm) != M_NONE) {
	    switch (ICM_TYPE(icm)) {
	    case M_BOOLEAN, M_GOODVAL:
		call fprintf (logfd, "  masktype = goodval, maskval = %d\n")
		    call pargi (ICM_VALUE(icm))
	    case M_BADVAL:
		call fprintf (logfd, "  masktype = badval, maskval = %d\n")
		    call pargi (ICM_VALUE(icm))
	    case M_NOVAL:
		call fprintf (logfd, "  masktype = noval, maskval = %d\n")
		    call pargi (ICM_VALUE(icm))
	    case M_GOODBITS:
		call fprintf (logfd, "  masktype = goodbits, maskval = %d\n")
		    call pargi (ICM_VALUE(icm))
	    case M_BADBITS:
		call fprintf (logfd, "  masktype = badbits, maskval = %d\n")
		    call pargi (ICM_VALUE(icm))
	    case M_LTVAL:
		call fprintf (logfd, "  masktype = goodval, maskval < %d\n")
		    call pargi (ICM_VALUE(icm))
	    case M_GTVAL:
		call fprintf (logfd, "  masktype = goodval, maskval > %d\n")
		    call pargi (ICM_VALUE(icm))
	    }
	}

	# Print information pertaining to individual images as a set of
	# columns with the image name being the first column.  Determine
	# what information is relevant and print the appropriate header.

	prncombine = false
	prexptime = false
	prmode = false
	prmedian = false
	prmean = false
	prmask = false
	prrdn = false
	prgain = false
	prsn = false
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
	    if (ICM_TYPE(icm) != M_NONE) {
		if (project)
		    bpname = Memi[ICM_NAMES(icm)]
		else
		    bpname = Memi[ICM_NAMES(icm)+i-1]
		if (Memc[bpname] != EOS)
		    prmask = true
	    }
	    if (combine == NMODEL || reject == CCDCLIP || reject == CRREJECT) {
		j = 1
		if (ctor (Memc[rdnoise], j, rval) == 0)
		    prrdn = true
		j = 1
		if (ctor (Memc[gain], j, rval) == 0)
		    prgain = true
		j = 1
		if (ctor (Memc[snoise], j, rval) == 0)
		    prsn = true
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
	    call fprintf (logfd, " %7s")
		call pargstr ("Mode")
	}
	if (prmedian) {
	    call fprintf (logfd, " %7s")
		call pargstr ("Median")
	}
	if (prmean) {
	    call fprintf (logfd, " %7s")
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
	if (prsn) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Snoise")
	}
	if (doscale) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Scale")
	}
	if (dozero) {
	    call fprintf (logfd, " %7s")
		call pargstr ("Zero")
	}
	if (dowts) {
	    call fprintf (logfd, " %6s")
		call pargstr ("Weight")
	}
	if (!aligned) {
	    call fprintf (logfd, " %9s")
		call pargstr ("Offsets")
	}
	if (prmask) {
	    call fprintf (logfd, " %s")
		call pargstr ("Maskfile")
	}
	call fprintf (logfd, "\n")

	do i = 1, nimages {
	    if (stack == YES) {
		call sprintf (Memc[key], SZ_FNAME, "stck%04d")
		    call pargi (i)
		ifnoerr (call imgstr (in[i], Memc[key], Memc[fname], SZ_LINE)) {
		    call fprintf (logfd, "  %21s")
			call pargstr (Memc[fname])
		} else {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_LINE)
		    call fprintf (logfd, "  %16s[%3d]")
			call pargstr (Memc[fname])
			call pargi (i)
		}
	    } else if (project) {
		call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_LINE)
		call fprintf (logfd, "  %16s[%3d]")
		    call pargstr (Memc[fname])
		    call pargi (i)
	    } else  {
		call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_LINE)
		call fprintf (logfd, "  %21s")
		    call pargstr (Memc[fname])
	    }
	    if (prncombine) {
		call fprintf (logfd, " %6d")
		    call pargi (ncombine[i])
	    }
	    if (prexptime) {
		call fprintf (logfd, " %6.1f")
		    call pargr (exptime[i])
	    }
	    if (prmode) {
		call fprintf (logfd, " %7.5g")
		    call pargr (mode[i])
	    }
	    if (prmedian) {
		call fprintf (logfd, " %7.5g")
		    call pargr (median[i])
	    }
	    if (prmean) {
		call fprintf (logfd, " %7.5g")
		    call pargr (mean[i])
	    }
	    if (prrdn) {
		rval = imgetr (in[i], Memc[rdnoise])
		call fprintf (logfd, " %7g")
		    call pargr (rval)
	    }
	    if (prgain) {
		rval = imgetr (in[i], Memc[gain])
		call fprintf (logfd, " %6g")
		    call pargr (rval)
	    }
	    if (prsn) {
		rval = imgetr (in[i], Memc[snoise])
		call fprintf (logfd, " %6g")
		    call pargr (rval)
	    }
	    if (doscale) {
		call fprintf (logfd, " %6.3f")
		    call pargr (1./scales[i])
	    }
	    if (dozero) {
		call fprintf (logfd, " %7.5g")
		    call pargr (-zeros[i])
	    }
	    if (dowts) {
		call fprintf (logfd, " %6.3f")
		    call pargr (wts[i])
	    }
	    if (!aligned) {
		if (IM_NDIM(out[1]) == 1) {
		    call fprintf (logfd, " %9d")
			call pargi (offsets[i,1])
		} else {
		    do  j = 1, IM_NDIM(out[1]) {
			call fprintf (logfd, " %4d")
			    call pargi (offsets[i,j])
		    }
		}
	    }
	    if (prmask) {
		if (stack == YES) {
		    call sprintf (Memc[key], SZ_FNAME, "bpm%04d")
			call pargi (i)
		    ifnoerr (call imgstr (in[i], Memc[key], Memc[fname],
		        SZ_LINE)) {
			call fprintf (logfd, " %s")
			    call pargstr (Memc[fname])
		    } else {
			call fprintf (logfd, " %s")
			    call pargstr (Memc[bpname])
		    }
		} else if (ICM_TYPE(icm) != M_NONE) {
		    if (project)
			bpname = Memi[ICM_NAMES(icm)]
		    else
			bpname = Memi[ICM_NAMES(icm)+i-1]
		    if (Memc[bpname] != EOS) {
			call fprintf (logfd, " %s")
			    call pargstr (Memc[bpname])
		    }
		}
	    }
	    call fprintf (logfd, "\n")
	}

	# Log information about the output images.
	call imstats (out[1], IM_IMAGENAME, Memc[fname], SZ_LINE)
	call fprintf (logfd, "\n  Output image = %s, ncombine = %d")
	    call pargstr (Memc[fname])
	    call pargi (nout)
	call fprintf (logfd, "\n")

	if (out[2] != NULL) {
	    call imstats (out[2], IM_IMAGENAME, Memc[fname], SZ_LINE)
	    call fprintf (logfd, "  Bad pixel mask = %s\n")
		call pargstr (Memc[fname])
	}

	if (out[4] != NULL) {
	    call imstats (out[4], IM_IMAGENAME, Memc[fname], SZ_LINE)
	    call fprintf (logfd, "  Rejection mask = %s\n")
		call pargstr (Memc[fname])
	}

	if (out[5] != NULL) {
	    call imstats (out[5], IM_IMAGENAME, Memc[fname], SZ_LINE)
	    call fprintf (logfd, "  Number rejected mask = %s\n")
		call pargstr (Memc[fname])
	}

	if (out[6] != NULL) {
	    call imstats (out[6], IM_IMAGENAME, Memc[fname], SZ_LINE)
	    call fprintf (logfd, "  Exposure mask = %s\n")
		call pargstr (Memc[fname])
	}

	if (out[3] != NULL) {
	    call imstats (out[3], IM_IMAGENAME, Memc[fname], SZ_LINE)
	    call fprintf (logfd, "  Sigma image = %s\n")
		call pargstr (Memc[fname])
	}

	call flush (logfd)
	call sfree (sp)
end
