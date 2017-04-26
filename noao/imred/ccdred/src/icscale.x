# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<error.h>
include	"icombine.h"

# IC_SCALE -- Get the scale factors for the images.
# 1. This procedure does CLIO to determine the type of scaling desired.
# 2. The output header parameters for exposure time and NCOMBINE are set.

procedure ic_scale (in, out, offsets, scales, zeros, wts, nimages)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
int	offsets[nimages,ARB]	# Image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero or sky levels
real	wts[nimages]		# Weights
int	nimages			# Number of images

int	stype, ztype, wtype
int	i, j, k, l, nout
real	mode, median, mean, exposure, zmean, darktime, dark
pointer	sp, ncombine, exptime, modes, medians, means
pointer	section, str, sname, zname, wname, imref
bool	domode, domedian, domean, dozero, snorm, znorm, wflag

bool	clgetb()
int	hdmgeti(), strdic(), ic_gscale()
real	hdmgetr(), asumr(), asumi()
errchk	ic_gscale, ic_statr

include	"icombine.com"

begin
	call smark (sp)
	call salloc (ncombine, nimages, TY_INT)
	call salloc (exptime, nimages, TY_REAL)
	call salloc (modes, nimages, TY_REAL)
	call salloc (medians, nimages, TY_REAL)
	call salloc (means, nimages, TY_REAL)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (sname, SZ_FNAME, TY_CHAR)
	call salloc (zname, SZ_FNAME, TY_CHAR)
	call salloc (wname, SZ_FNAME, TY_CHAR)

	# Set the defaults.
	call amovki (1, Memi[ncombine], nimages)
	call amovkr (0., Memr[exptime], nimages)
	call amovkr (INDEF, Memr[modes], nimages)
	call amovkr (INDEF, Memr[medians], nimages)
	call amovkr (INDEF, Memr[means], nimages)
	call amovkr (1., scales, nimages)
	call amovkr (0., zeros, nimages)
	call amovkr (1., wts, nimages)

	# Get the number of images previously combined and the exposure times.
	# The default combine number is 1 and the default exposure is 0.

	do i = 1, nimages {
	    iferr (Memi[ncombine+i-1] = hdmgeti (in[i], "ncombine"))
		Memi[ncombine+i-1] = 1
	    iferr (Memr[exptime+i-1] = hdmgetr (in[i], "exptime"))
		Memr[exptime+i-1] = 0.
	    if (project) {
		call amovki (Memi[ncombine], Memi[ncombine], nimages)
		call amovkr (Memr[exptime], Memr[exptime], nimages)
		break
	    }
	}

	# Set scaling factors.

	stype = ic_gscale ("scale", Memc[sname], STYPES, in, Memr[exptime],
	    scales, nimages)
	ztype = ic_gscale ("zero", Memc[zname], ZTYPES, in, Memr[exptime],
	    zeros, nimages)
	wtype = ic_gscale ("weight", Memc[wname], WTYPES, in, Memr[exptime],
	    wts, nimages)

	# Get image statistics only if needed.
	domode = ((stype==S_MODE)||(ztype==S_MODE)||(wtype==S_MODE))
	domedian = ((stype==S_MEDIAN)||(ztype==S_MEDIAN)||(wtype==S_MEDIAN))
	domean = ((stype==S_MEAN)||(ztype==S_MEAN)||(wtype==S_MEAN))
	if (domode || domedian || domean) {
	    Memc[section] = EOS
	    Memc[str] = EOS
	    call clgstr ("statsec", Memc[section], SZ_FNAME)
	    call sscan (Memc[section])
	    call gargwrd (Memc[section], SZ_FNAME)
	    call  gargwrd (Memc[str], SZ_LINE)

	    i = strdic (Memc[section], Memc[section], SZ_FNAME, S_SECTION)
	    switch (i) {
	    case S_INPUT:
		call strcpy (Memc[str], Memc[section], SZ_FNAME)
		imref = NULL
	    case S_OUTPUT:
		call strcpy (Memc[str], Memc[section], SZ_FNAME)
		imref = out[1]
	    case S_OVERLAP:
		call strcpy ("[", Memc[section], SZ_FNAME)
		do i = 1, IM_NDIM(out[1]) {
		    k = offsets[1,i] + 1
		    l = offsets[1,i] + IM_LEN(in[1],i)
		    do j = 2, nimages {
			k = max (k, offsets[j,i]+1)
			l = min (l, offsets[j,i]+IM_LEN(in[j],i))
		    }
		    if (i < IM_NDIM(out[1]))
			call sprintf (Memc[str], SZ_LINE, "%d:%d,")
		    else
			call sprintf (Memc[str], SZ_LINE, "%d:%d]")
			    call pargi (k)
			    call pargi (l)
		    call strcat (Memc[str], Memc[section], SZ_FNAME)
		}
		imref = out[1]
	    default:
		imref = NULL
	    }

	    do i = 1, nimages {
		if (imref != out[1])
		    imref = in[i]
		call ic_statr (in[i], imref, Memc[section], offsets,
		    i, nimages, domode, domedian, domean, mode, median, mean)
		if (domode) {
		    Memr[modes+i-1] = mode
		    if (stype == S_MODE)
			scales[i] = mode
		    if (ztype == S_MODE)
			zeros[i] = mode
		    if (wtype == S_MODE)
			wts[i] = mode
		}
		if (domedian) {
		    Memr[medians+i-1] = median
		    if (stype == S_MEDIAN)
			scales[i] = median
		    if (ztype == S_MEDIAN)
			zeros[i] = median
		    if (wtype == S_MEDIAN)
			wts[i] = median
		}
		if (domean) {
		    Memr[means+i-1] = mean
		    if (stype == S_MEAN)
			scales[i] = mean
		    if (ztype == S_MEAN)
			zeros[i] = mean
		    if (wtype == S_MEAN)
			wts[i] = mean
		}
	    }
	}

	do i = 1, nimages
	    if (scales[i] <= 0.) {
		call eprintf ("WARNING: Negative scale factors")
		call eprintf (" -- ignoring scaling\n")
		call amovkr (1., scales, nimages)
		break
	    }

	# Convert to relative factors if needed.
	snorm = (stype == S_FILE || stype == S_KEYWORD)
	znorm = (ztype == S_FILE || ztype == S_KEYWORD)
	wflag = (wtype == S_FILE || wtype == S_KEYWORD)
	if (snorm)
	    call arcpr (1., scales, scales, nimages)
	else {
	    mean = asumr (scales, nimages) / nimages
	    call adivkr (scales, mean, scales, nimages)
	}
	call adivr (zeros, scales, zeros, nimages)
	zmean = asumr (zeros, nimages) / nimages

	if (wtype != S_NONE) {
	    do i = 1, nimages {
		if (wts[i] <= 0.) {
		    call eprintf ("WARNING: Negative weights")
		    call eprintf (" -- using only NCOMBINE weights\n")
		    do j = 1, nimages
			wts[j] = Memi[ncombine+j-1]
		    break
		}
		if (ztype == S_NONE || znorm || wflag)
	            wts[i] = Memi[ncombine+i-1] * wts[i]
		else {
		    if (zeros[i] <= 0.) {
			call eprintf ("WARNING: Negative zero offsets")
			call eprintf (" -- ignoring zero weight adjustments\n")
			do j = 1, nimages
			    wts[j] = Memi[ncombine+j-1] * wts[j]
			break
		    }
		    wts[i] = Memi[ncombine+i-1] * wts[i] * zmean / zeros[i]
		}
	    }
	}

	if (znorm)
	    call anegr (zeros, zeros, nimages)
	else {
	    # Because of finite arithmetic it is possible for the zero offsets
	    # to be nonzero even when they are all equal.  Just for the sake of
	    # a nice log set the zero offsets in this case.

	    call asubkr (zeros, zmean, zeros, nimages)
	    for (i=2; (i<=nimages)&&(zeros[i]==zeros[1]); i=i+1)
		;
	    if (i > nimages)
		call aclrr (zeros, nimages)
	}
	mean = asumr (wts, nimages)
	call adivkr (wts, mean, wts, nimages)

	# Set flags for scaling, zero offsets, sigma scaling, weights.
	# Sigma scaling may be suppressed if the scales or zeros are
	# different by a specified tolerance.

	doscale = false
	dozero = false
	doscale1 = false
	dowts = false
	do i = 2, nimages {
	    if (snorm || scales[i] != scales[1])
		doscale = true
	    if (znorm || zeros[i] != zeros[1])
		dozero = true
	    if (wts[i] != wts[1])
		dowts = true
	}
	if (doscale && sigscale != 0.) {
	    do i = 1, nimages {
		if (abs (scales[i] - 1) > sigscale) {
		    doscale1 = true
		    break
		}
	    }
	    if (!doscale1 && zmean > 0.) {
		do i = 1, nimages {
		    if (abs (zeros[i] / zmean) > sigscale) {
			doscale1 = true
			break
		    }
		}
	    }
	}
		    
	# Set the output header parameters.
	nout = asumi (Memi[ncombine], nimages)
	call hdmputi (out[1], "ncombine", nout)
	exposure = 0.
	darktime = 0.
	mean = 0.
	do i = 1, nimages {
	    exposure = exposure + wts[i] * Memr[exptime+i-1] / scales[i]
	    ifnoerr (dark = hdmgetr (in[i], "darktime"))
		darktime = darktime + wts[i] * dark / scales[i]
	    else
		darktime = darktime + wts[i] * Memr[exptime+i-1] / scales[i]
	    ifnoerr (mode = hdmgetr (in[i], "ccdmean"))
		mean = mean + wts[i] * mode / scales[i]
	}
	call hdmputr (out[1], "exptime", exposure)
	call hdmputr (out[1], "darktime", darktime)
	ifnoerr (mode = hdmgetr (out[1], "ccdmean")) {
	    call hdmputr (out[1], "ccdmean", mean)
	    iferr (call imdelf (out[1], "ccdmeant"))
		;
	}
	if (out[2] != NULL) {
	    call imstats (out[2], IM_IMAGENAME, Memc[str], SZ_FNAME)
	    call imastr (out[1], "BPM", Memc[str])
	}

	# Start the log here since much of the info is only available here.
	if (clgetb ("verbose")) {
	    i = logfd
	    logfd = STDOUT
	    call ic_log (in, out, Memi[ncombine], Memr[exptime], Memc[sname],
		Memc[zname], Memc[wname], Memr[modes], Memr[medians],
		Memr[means], scales, zeros, wts, offsets, nimages, dozero,
		nout, "", exposure)

	    logfd = i
	}
	call ic_log (in, out, Memi[ncombine], Memr[exptime], Memc[sname],
	    Memc[zname], Memc[wname], Memr[modes], Memr[medians], Memr[means],
	    scales, zeros, wts, offsets, nimages, dozero, nout,
	    "", exposure)

	doscale = (doscale || dozero)

	call sfree (sp)
end


# IC_GSCALE -- Get scale values as directed by CL parameter
# The values can be one of those in the dictionary, from a file specified
# with a @ prefix, or from an image header keyword specified by a ! prefix.

int procedure ic_gscale (param, name, dic, in, exptime, values, nimages)

char	param[ARB]		#I CL parameter name
char	name[SZ_FNAME]		#O Parameter value
char	dic[ARB]		#I Dictionary string
pointer	in[nimages]		#I IMIO pointers
real	exptime[nimages]	#I Exposure times
real	values[nimages]		#O Values
int	nimages			#I Number of images

int	type			#O Type of value

int	fd, i, nowhite(), open(), fscan(), nscan(), strdic()
real	rval, hdmgetr()
pointer	errstr
errchk	open, hdmgetr()

include	"icombine.com"

begin
	call clgstr (param, name, SZ_FNAME)
	if (nowhite (name, name, SZ_FNAME) == 0)
	    type = S_NONE
	else if (name[1] == '@') {
	    type = S_FILE
	    fd = open (name[2], READ_ONLY, TEXT_FILE)
	    i = 0
	    while (fscan (fd) != EOF) {
		call gargr (rval)
		if (nscan() != 1)
		    next
		if (i == nimages) {
		   call eprintf (
		       "Warning: Ignoring additional %s values in %s\n")
		       call pargstr (param)
		       call pargstr (name[2])
		   break
		}
		i = i + 1
		values[i] = rval
	    }
	    call close (fd)
	    if (i < nimages) {
		call salloc (errstr, SZ_LINE, TY_CHAR)
		call sprintf (Memc[errstr], SZ_FNAME,
		    "Insufficient %s values in %s")
		    call pargstr (param)
		    call pargstr (name[2])
		call error (1, Memc[errstr])
	    }
	} else if (name[1] == '!') {
	    type = S_KEYWORD
	    do i = 1, nimages {
		values[i] = hdmgetr (in[i], name[2])
		if (project) {
		    call amovkr (values, values, nimages)
		    break
		}
	    }
	} else {
	    type = strdic (name, name, SZ_FNAME, dic)
	    if (type == 0)
		call error (1, "Unknown scale, zero, or weight type")
	    if (type==S_EXPOSURE)
		do i = 1, nimages
		    values[i] = max (0.001, exptime[i])
	}

	return (type)
end
