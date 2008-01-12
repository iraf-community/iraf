# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<error.h>
include	<ctype.h>
include	<smw.h>
include	"icombine.h"

# IC_SCALE -- Get the scale factors for the spectra.
# 1. This procedure does CLIO to determine the type of scaling desired.
# 2. The output header parameters for exposure time and NCOMBINE are set.

procedure ic_scale (sh, shout, lflags, scales, zeros, wts, nimages)

pointer	sh[nimages]		# Input spectra
pointer	shout			# Output spectrum
int	lflags[nimages]		# Data flags
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero or sky levels
real	wts[nimages]		# Weights
int	nimages			# Number of images

int	stype, ztype, wtype
int	i, j, nout
real	mode, median, mean, exposure, zmean
pointer	sp, ncombine, exptime, modes, medians, means, expname
pointer	str, sname, zname, wname, rg
bool	domode, domedian, domean, dozero

int	ic_gscale()
real	asumr(), asumi()
pointer	ic_wranges()
errchk	ic_gscale, ic_statr

include	"icombine.com"

begin
	call smark (sp)
	call salloc (ncombine, nimages, TY_INT)
	call salloc (exptime, nimages, TY_REAL)
	call salloc (modes, nimages, TY_REAL)
	call salloc (medians, nimages, TY_REAL)
	call salloc (means, nimages, TY_REAL)
	call salloc (expname, SZ_FNAME, TY_CHAR)
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

	# Set scaling factors.
	if (combine == SUM) {
	    stype = S_NONE
	    ztype = S_NONE
	    wtype = S_NONE
	    do i = 1, nimages
		Memr[exptime+i-1] = IT(sh[i])
	} else {
	    stype = ic_gscale ("scale", Memc[sname], STYPES, sh, Memr[exptime],
		scales, nimages)
	    ztype = ic_gscale ("zero", Memc[zname], ZTYPES, sh, Memr[exptime],
		zeros, nimages)
	    wtype = ic_gscale ("weight", Memc[wname], WTYPES, sh, Memr[exptime],
		wts, nimages)
	}

	Memc[expname] = EOS
	if (combine == SUM || stype == S_EXPOSURE || wtype == S_EXPOSURE) {
	    call strcpy ("exptime", Memc[expname], SZ_FNAME)
	    do i = 1, nimages
		if (IS_INDEFR(Memr[exptime+i-1]))
		    Memc[expname] = EOS
	}

	# Get image statistics only if needed.
	domode = ((stype==S_MODE)||(ztype==S_MODE)||(wtype==S_MODE))
	domedian = ((stype==S_MEDIAN)||(ztype==S_MEDIAN)||(wtype==S_MEDIAN))
	domean = ((stype==S_MEAN)||(ztype==S_MEAN)||(wtype==S_MEAN))
	if (domode || domedian || domean) {
	    call clgstr ("sample", Memc[str], SZ_LINE)
	    rg = ic_wranges (Memc[str])
	    do i = 1, nimages {
		call ic_statr (sh[i], lflags[i], rg, domode, domedian, domean,
		    mode, median, mean)
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
	    call mfree (rg, TY_REAL)
	}

	do i = 1, nimages
	    if (scales[i] <= 0.) {
		call eprintf ("WARNING: Negative scale factors")
		call eprintf (" -- ignoring scaling\n")
		call amovkr (1., scales, nimages)
		break
	    }

	# Convert to relative factors.
	mean = asumr (scales, nimages) / nimages
	call adivkr (scales, mean, scales, nimages)
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
		if (ztype == S_NONE)
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

	call asubkr (zeros, zmean, zeros, nimages)
	mean = asumr (wts, nimages)
	call adivkr (wts, mean, wts, nimages)

	# Because of finite arithmetic it is possible for the zero offsets to
	# be nonzero even when they are all equal.  Just for the sake of
	# a nice log set the zero offsets in this case.

	for (i=2; (i<=nimages)&&(zeros[i]==zeros[1]); i=i+1)
	    ;
	if (i > nimages)
	    call aclrr (zeros, nimages)

	# Set flags for scaling, zero offsets, sigma scaling, weights.
	# Sigma scaling may be suppressed if the scales or zeros are
	# different by a specified tolerance.

	doscale = false
	dozero = false
	doscale1 = false
	dowts = false
	do i = 2, nimages {
	    if (scales[i] != scales[1])
		doscale = true
	    if (zeros[i] != zeros[1])
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
	call imaddi (IM(shout), "ncombine", nout)
	if (Memc[expname] != EOS) {
	    exposure = 0.
	    if (combine == SUM) {
		do i = 1, nimages
		    exposure = exposure + Memr[exptime+i-1]
	    } else {
		do i = 1, nimages
		    exposure = exposure + wts[i] * Memr[exptime+i-1] / scales[i]
	    }
	    call imaddr (IM(shout), Memc[expname], exposure)
	} else
	    exposure = INDEF

	# Start the log here since much of the info is only available here.
	call ic_log (sh, shout, Memi[ncombine], Memr[exptime], Memc[sname],
	    Memc[zname], Memc[wname], Memr[modes], Memr[medians], Memr[means],
	    scales, zeros, wts, nimages, dozero, nout, Memc[expname], exposure)

	doscale = (doscale || dozero)

	call sfree (sp)
end


# IC_GSCALE -- Get scale values as directed by CL parameter
# The values can be one of those in the dictionary, from a file specified
# with a @ prefix, or from an image header keyword specified by a ! prefix.

int procedure ic_gscale (param, name, dic, sh, exptime, values, nimages)

char	param[ARB]		#I CL parameter name
char	name[SZ_FNAME]		#O Parameter value
char	dic[ARB]		#I Dictionary string
pointer	sh[nimages]		#I SHDR pointers
real	exptime[nimages]	#I Exposure times
real	values[nimages]		#O Values
int	nimages			#I Number of images

int	type			#O Type of value

int	fd, i, nowhite(), open(), fscan(), nscan(), strdic()
real	rval
pointer	errstr
errchk	open

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
		switch (param[1]) {
		case 's':
		    values[i] = ST(sh[i])
		case 'z':
		    values[i] = HA(sh[i])
		case 'w':
		    values[i] = AM(sh[i])
		}
	    }
	} else {
	    type = strdic (name, name, SZ_FNAME, dic)
	    if (type == 0)
		call error (1, "Unknown scale, zero, or weight type")
	    if (type==S_EXPOSURE) {
		do i = 1, nimages {
		    if (IS_INDEF(IT(sh[i])))
			call error (1, "Exposure time not defined")
		    exptime[i] = IT(sh[i])
		    values[i] = max (0.001, exptime[i])
		}
	    }
	}

	return (type)
end


# IC_WRANGES -- Parse wavelength range string.
# A wavelength range string  consists of colon delimited ranges with
# multiple ranges separated by comma and/or whitespace.

pointer procedure ic_wranges (rstr)

char	rstr[ARB]		# Range string
pointer	rg			# Range pointer

int	i, fd, strlen(), open(), getline()
pointer	sp, str, ptr
errchk	open, ic_wadd

begin
	call smark (sp)
	call salloc (str, max (strlen (rstr), SZ_LINE), TY_CHAR)
	call calloc (rg, 1, TY_REAL)

	i = 1
	while (rstr[i] != EOS) {

	    # Find beginning and end of a range and copy it to the work string
	    while (IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n')
	        i = i + 1
	    if (rstr[i] == EOS)
		break

	    ptr = str
	    while (!(IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n' ||
		rstr[i]==EOS)) {
		Memc[ptr] = rstr[i]
	        i = i + 1
		ptr = ptr + 1
	    }
	    Memc[ptr] = EOS

	    # Add range(s)
	    iferr {
		if (Memc[str] == '@') {
		    fd = open (Memc[str+1], READ_ONLY, TEXT_FILE)
		    while (getline (fd, Memc[str]) != EOF) {
			iferr (call ic_wadd (rg, Memc[str]))
			    call erract (EA_WARN)
		    }
		    call close (fd)
		} else
		    call ic_wadd (rg, Memc[str])
	    } then
		call erract (EA_WARN)
	}

	call sfree (sp)

	# Set final structure
	i = Memr[rg]
	if (i == 0)
	    call mfree (rg, TY_REAL)
	else
	    call realloc (rg, 1 + 2 * i, TY_REAL)
	return (rg)
end


# IC_WADD -- Add a range

procedure ic_wadd (rg, rstr)

pointer	rg			# Range descriptor
char	rstr[ARB]		# Range string

int	i, j, n, strlen(), ctor()
real	w1, w2
pointer	sp, str, ptr

begin
	call smark (sp)
	call salloc (str, strlen (rstr), TY_CHAR)

	i = 1
	n = Memr[rg]
	while (rstr[i] != EOS) {

	    # Find beginning and end of a range and copy it to the work string
	    while (IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n')
	        i = i + 1
	    if (rstr[i] == EOS)
		break

	    ptr = str
	    while (!(IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n' ||
		rstr[i]==EOS)) {
		if (rstr[i] == ':')
		    Memc[ptr] = ' '
		else
		    Memc[ptr] = rstr[i]
	        i = i + 1
		ptr = ptr + 1
	    }
	    Memc[ptr] = EOS

	    # Parse range
	    if (Memc[str] == '@')
		call error (1, "Cannot nest @files")
	    else {
		# Get range
		j = 1
		if (ctor (Memc[str], j, w1) == 0)
		    call error (1, "Range syntax error")
		if (ctor (Memc[str], j, w2) == 0)
		    call error (1, "Range syntax error")
	    }

	    if (mod (n, 10) == 0)
		call realloc (rg, 1+2*(n+10), TY_REAL)
	    n = n + 1
	    Memr[rg+2*n-1] = min (w1, w2)
	    Memr[rg+2*n] = max (w1, w2)
	}
	Memr[rg] = n

	call sfree (sp)
end


# IC_WISINRANGE -- Is wavelength in range?

bool procedure ic_wisinrange (rg, w)

pointer	rg		# Wavelength range array
real	w		# Wavelength

int	i, n

begin
	if (rg == NULL)
	    return (true)

	n = nint (Memr[rg])
	do i = 1, 2*n, 2
	    if (w >= Memr[rg+i] && w <= Memr[rg+i+1])
		return (true)
	return (false)
end
