# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<error.h>
include	<ctype.h>
include	"../shdr.h"
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
pointer	sp, ncombine, exptime, modes, medians, means, expname, fname, rg
bool	domode, domedian, domean, dozero

int	clgwrd()
real	asumr(), asumi()
pointer	ic_wranges()
errchk	ic_statr

include	"icombine.com"

begin
	call smark (sp)
	call salloc (ncombine, nimages, TY_INT)
	call salloc (exptime, nimages, TY_REAL)
	call salloc (modes, nimages, TY_REAL)
	call salloc (medians, nimages, TY_REAL)
	call salloc (means, nimages, TY_REAL)
	call salloc (expname, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_LINE, TY_CHAR)

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
	} else {
	    stype = clgwrd ("scale", Memc[fname], SZ_LINE, STYPES)
	    ztype = clgwrd ("zero", Memc[fname], SZ_LINE, ZTYPES)
	    wtype = clgwrd ("weight", Memc[fname], SZ_LINE, WTYPES)
	}

	# Get the exposure times.
	if (stype == S_EXPOSURE || wtype == S_EXPOSURE) {
	    call strcpy ("exptime", Memc[expname], SZ_FNAME)
	    do i = 1, nimages {
		if (IS_INDEF(IT(sh[i])))
		    call error (1, "Exposure time not defined")
		Memr[exptime+i-1] = IT(sh[i])
	    }
	} else
	    Memc[expname] = EOS

	if (stype==S_EXPOSURE)
	    do i = 1, nimages
		scales[i] = max (0.001, Memr[exptime+i-1])
	if (wtype==S_EXPOSURE)
	    do i = 1, nimages
		wts[i] = max (0.001, Memr[exptime+i-1])

	# Get image statistics only if needed.
	domode = ((stype==S_MODE)||(ztype==S_MODE)||(wtype==S_MODE))
	domedian = ((stype==S_MEDIAN)||(ztype==S_MEDIAN)||(wtype==S_MEDIAN))
	domean = ((stype==S_MEAN)||(ztype==S_MEAN)||(wtype==S_MEAN))
	if (domode || domedian || domean) {
	    call clgstr ("sample", Memc[fname], SZ_LINE)
	    rg = ic_wranges (Memc[fname])
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
		call eprintf (
		    "WARNING: Negative scale factors -- ignoring scaling\n")
		call amovkr (1., scales, nimages)
		break
	    }

	do i = 1, nimages
	    zeros[i] = zeros[i] / scales[i]

	if (wtype != S_NONE) {
	    do i = 1, nimages {
		if (wts[i] <= 0.) {
		    call eprintf (
			"WARNING: Negative weights -- ignoring weighting\n")
		    do j = 1, nimages
			wts[j] = sqrt (real (Memi[ncombine+j-1]))
		    break
		}
		if (ztype == S_NONE)
	            wts[i] = sqrt (Memi[ncombine+i-1] * wts[i])
		else {
		    if (zeros[i] <= 0.) {
			call eprintf (
	"WARNING: Negative zero offsets -- ignoring zero weight adjustments\n")
			do j = 1, nimages
			    wts[j] = sqrt (Memi[ncombine+j-1]*wts[j])
			break
		    }
		    wts[i] = sqrt (Memi[ncombine+i-1]*wts[i]/zeros[i])
		}
	    }
	}

	# Convert to relative scaling factors.
	mean = asumr (scales, nimages) / nimages
	call adivkr (scales, mean, scales, nimages)
	call adivr (zeros, scales, zeros, nimages)
	zmean = asumr (zeros, nimages) / nimages
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
	if (doscale && zmean > 0. && sigscale != 0.) {
	    do i = 1, nimages {
		if (abs (scales[i] - 1) > sigscale ||
		    abs (zeros[i] / zmean) > sigscale) {
		    doscale1 = true
		    break
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
	call ic_log (sh, shout, Memi[ncombine], Memr[exptime], Memr[modes],
	    Memr[medians], Memr[means], scales, zeros, wts, nimages,
	    dozero, nout, Memc[expname], exposure)

	doscale = (doscale || dozero)

	call sfree (sp)
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
