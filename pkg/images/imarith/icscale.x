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
pointer	out[3]			# Output images
int	offsets[nimages,ARB]	# Image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero or sky levels
real	wts[nimages]		# Weights
int	nimages			# Number of images

int	stype, ztype, wtype
int	i, j, k, l, nout
real	mode, median, mean, exposure, zmean
pointer	sp, ncombine, exptime, modes, medians, means, expname
pointer	section, fname, imref
bool	domode, domedian, domean, dozero

int	clgwrd(), imgeti(), strdic()
real	imgetr(), asumr(), asumi()
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
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

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

	call clgstr ("expname", Memc[expname], SZ_FNAME)
	do i = 1, nimages {
	    iferr (Memi[ncombine+i-1] = imgeti (in[i], "ncombine"))
		Memi[ncombine+i-1] = 1
	    if (Memc[expname] != EOS) {
	        iferr (Memr[exptime+i-1] = imgetr (in[i], Memc[expname]))
		    Memr[exptime+i-1] = 0.
	    }
	    if (project) {
		call amovki (Memi[ncombine], Memi[ncombine], nimages)
		call amovkr (Memr[exptime], Memr[exptime], nimages)
		break
	    }
	}

	# Set scaling factors.

	stype = clgwrd ("scale", Memc[fname], SZ_FNAME, STYPES)
	ztype = clgwrd ("zero", Memc[fname], SZ_FNAME, ZTYPES)
	wtype = clgwrd ("weight", Memc[fname], SZ_FNAME, WTYPES)

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
	    Memc[section] = EOS
	    Memc[fname] = EOS
	    call clgstr ("statsec", Memc[section], SZ_FNAME)
	    call sscan (Memc[section])
	    call gargwrd (Memc[section], SZ_FNAME)
	    call  gargwrd (Memc[fname], SZ_FNAME)

	    i = strdic (Memc[section], Memc[section], SZ_FNAME, S_SECTION)
	    switch (i) {
	    case S_INPUT:
		call strcpy (Memc[fname], Memc[section], SZ_FNAME)
		imref = NULL
	    case S_OUTPUT:
		call strcpy (Memc[fname], Memc[section], SZ_FNAME)
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
			call sprintf (Memc[fname], SZ_FNAME, "%d:%d,")
		    else
			call sprintf (Memc[fname], SZ_FNAME, "%d:%d]")
			    call pargi (k)
			    call pargi (l)
		    call strcat (Memc[fname], Memc[section], SZ_FNAME)
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

	do i = 1, nimages {
	    if (scales[i] <= 0.)
		call error (1, "Scaling factors must be postive")
	    zeros[i] = zeros[i] / scales[i]
	    if (wtype != S_NONE) {
		if (ztype == S_NONE)
	            wts[i] = sqrt (Memi[ncombine+i-1] * wts[i])
		else {
		    if (zeros[i] <= 0.)
			call error (1,
			   "Zero offset factors must be positive for weighting")
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
	call imaddi (out[1], "ncombine", nout)
	if (Memc[expname] != EOS) {
	    exposure = 0.
	    do i = 1, nimages
	        exposure = exposure + wts[i] * Memr[exptime+i-1] / scales[i]
	    call imaddr (out[1], Memc[expname], exposure)
	} else
	    exposure = INDEF
	if (out[2] != NULL) {
	    call imstats (out[2], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	    call imastr (out[1], "BPM", Memc[fname])
	}
	ifnoerr (mode = imgetr (out[1], "CCDMEAN"))
	    call imdelf (out[1], "CCDMEAN")

	# Start the log here since much of the info is only available here.
	call ic_log (in, out, Memi[ncombine], Memr[exptime], Memr[modes],
	    Memr[medians], Memr[means], scales, zeros, wts, offsets, nimages,
	    dozero, nout, Memc[expname], exposure)

	doscale = (doscale || dozero)

	call sfree (sp)
end
