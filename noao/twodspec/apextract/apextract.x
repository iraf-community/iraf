include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	<math/iminterp.h>
include	<pkg/gtools.h>
include	"apertures.h"

# Background fitting types
define	BACKGROUND	"|none|average|median|minimum|fit|"
define	B_NONE		1
define	B_AVERAGE	2
define	B_MEDIAN	3
define	B_MINIMUM	4
define	B_FIT		5

# Weight types
define	WEIGHTS		"|none|variance|"
define	W_NONE		1
define	W_VARIANCE	2

# Profile fitting algorithms
define	P_FIT		"|fit1d|fit2d|"
define	P_FIT1D		1
define	P_FIT2D		2

# Output formats
define	FORMATS		"|onedspec|multispec|echelle|strip|normalize|flatten\
			|ratio|difference|fit|noise|"
define	ONEDSPEC	1	# Individual 1D spectra
define	MULTISPEC	2	# Multiple spectra
define	ECHELLE		3	# Echelle spectra
define	STRIP		4	# Strip spectra
define	NORM		5	# Normalized spectra
define	FLAT		6	# Flat spectra
define	RATIO		7	# Ratio of data to model
define	DIFF		8	# Difference of data and model
define	FIT		9	# Model
define	NOISE		10	# Noise calculation


# AP_EXTRACT -- Extract spectra by a weighted sum across the apertures.
# 
# This routine does clobber checks on the output images, manages the I/O
# from the input image in as big of pieces as possible, and loops through
# each aperture calling routines to determine the sky, do any fitting and
# extraction, and output the spectra.
# The extraction may be either a simple, unweighted extraction
# which is very fast or a weighted extraction using CCD noise
# parameters.  The weights require dividing out the basic spectrum and
# smoothing the 2D spectral profile.  The general approach of variance
# weighting is described by K. Horne (PASP V98, P609, 1986).  The
# smoothing has two algorithms, fitting columns or lines parallel to the
# dispersion axis for nearly aligned spectra or fitting a 2D function
# using a method given by T. Marsh (PASP V101, P1032, 1989).  The profile
# may also be used to reject cosmic rays by iteration.
#
# The extractions require enough memory to get at least one aperture plus
# background (if needed) into memory.  If possible the region containing
# all the apertures is read into memory.  The target maximum amount of
# memory is set by the maxmimum size returned by BEGMEM and the
# appropriate working set size is requested.  The optimal size can be
# tuned through BEGMEM, which references a machine dependent include
# file, if needed.  The algorithm should work well (minimize I/O as well
# as paging) in all cases but very large image formats with highly tilted
# spectra (where aperture extraction along the image axes is not really
# appropriate).  These memory requirements were chosen to minimize image
# I/O and because the variance weighted algorithms need to make multiple
# passes through the image.  In principle simple, unweighted extractions
# with no sky smoothing can be done sequentially but this was not done in
# order to use nearly the same code for both weighted and unweighted
# cases.
#
# If using variance weighting and a profile image is given then it is used
# to determine the profile which is then applied to the target image
# during the final extraction.  If the same profile image is used multiple
# times it would be more efficient to store the profile but then issues
# of consistency arise.  For now this possible feature is not implemented.

procedure ap_extract (input, output, format, profiles, aps, naps)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image (optional root name)
char	format[SZ_LINE]		# Output format
char	profiles[SZ_FNAME]	# Profile filename (optional)
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

# CL parameters
int	fmt			# Output format
int	bkg			# Background type
int	weights			# Extraction weights
int	pfit			# Profile fitting algorithm
bool	clean			# Reject cosmic rays?
real	gain			# Photon/DN gain
real	rdnoise			# Read out noise
int	nsubaps			# Number of subapertures
int	interptype		# Edge interpolation type

int	i, j, k, napsex, aaxis, baxis, namax, na, nb, na1, interpbuf
int	amin, amax, bmin, bmax
int	new_size, old_size, max_size, best_size
real	cmin, cmax, xmin, xmax, shift
pointer	sp, str, bkgstr, wtstr, cleanstr, apsex
pointer	a, b, c, astart, spec, specsky, specsig, raw, profile
pointer	a1, a2, b1, b2, c1, c2, im, pim, ap, cv, ic, dbuf, pbuf, sbuf, svar, ptr
pointer	asi

bool	clgetb(), apgetb(), strne()
int	apgeti(), apgwrd(), begmem(), ap_check()
real	apgimr(), ap_cveval(), ic_getr()
pointer	ap_immap(), imgs2r(), imgl2r()
errchk	salloc, malloc, ap_immap, imgs2r, imgl2r, asiinit
errchk	ap_check, ap_skyeval, ap_profile, ap_variance, ap_output, apgimr

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	napsex = 0
	do i = 1, naps
	    if (AP_SELECT(aps[i]) == YES)
		napsex = napsex + 1
	if (napsex == 0) {
	    call sprintf (Memc[str], SZ_LINE,
	        "EXTRACT - No apertures defined for %s")
		call pargstr (input)
	    call ap_log (Memc[str], YES, NO, YES)
	    call sfree (sp)
	    return
	}

	call salloc (bkgstr, SZ_FNAME, TY_CHAR)
	call salloc (wtstr, SZ_FNAME, TY_CHAR)
	call salloc (cleanstr, SZ_FNAME, TY_CHAR)
	call salloc (apsex, napsex, TY_POINTER)

	# Select apertures to extract and fix possible limit error.
	napsex = 0
	do i = 1, naps {
	    if (AP_LOW(aps[i],1) > AP_HIGH(aps[i],1)) {
		xmax = AP_LOW(aps[i],1)
		AP_LOW(aps[i],1) = AP_HIGH(aps[i],1)
		AP_HIGH(aps[i],1) = xmax
	    }
	    if (AP_LOW(aps[i],2) > AP_HIGH(aps[i],2)) {
		xmax = AP_LOW(aps[i],2)
		AP_LOW(aps[i],2) = AP_HIGH(aps[i],2)
		AP_HIGH(aps[i],2) = xmax
	    }
	    if (AP_SELECT(aps[i]) == NO)
		next
	    Memi[apsex+napsex] = aps[i]
	    napsex = napsex + 1
	}

	# Get CL parameters
	bkg = apgwrd ("background", Memc[bkgstr], SZ_FNAME, BACKGROUND)
	pfit = apgwrd ("pfit", Memc[str], SZ_LINE, P_FIT)
	clean = apgetb ("clean")
	if (clean)
	    call strcpy ("yes", Memc[cleanstr], SZ_FNAME)
	else
	    call strcpy ("no", Memc[cleanstr], SZ_FNAME)
	nsubaps = apgeti ("nsubaps")
	interptype = II_LINEAR

	# Do clobber checking.  Return if output exists and not clobbering.
	call apgstr ("ansclobber", Memc[str], SZ_LINE)
	call appstr ("ansclobber1", Memc[str])
	fmt = ap_check (input, output, format, Memi[apsex], napsex, nsubaps)
	if (fmt == 0) {
	    call sfree (sp)
	    return
	}

	# Force weights depending on format or cleaning.
	switch (fmt) {
	case FLAT, RATIO, DIFF, FIT, NOISE:
	    weights = W_VARIANCE
	default:
	    if (clean) {
		call strcpy ("variance", Memc[wtstr], SZ_FNAME)
		weights = W_VARIANCE
	    } else
	        weights = apgwrd ("weights", Memc[wtstr], SZ_FNAME, WEIGHTS)
	}

	if (clgetb ("verbose")) {
	    call printf ("Extracting apertures ...\n")
	    call flush (STDOUT)
	}
	    
	# Open input image and profile image if given.  Set axis parameters
	# where 'a' is the aperture axis across the dispersion and 'b' is
	# along the dispersion.

	im = ap_immap (input, aaxis, baxis)
	namax = IM_LEN(im, aaxis)
	nb = IM_LEN(im, baxis)

	pim = NULL
	if (strne(profiles,input) && weights==W_VARIANCE && profiles[1]!=EOS) {
	    pim = ap_immap (profiles, i, j)
	    if (i!=aaxis||j!=baxis||IM_LEN(pim,i)!=namax||IM_LEN(pim,j)!=nb) {
		call imunmap (pim)
		call imunmap (im)
		call sfree (sp)
		call error (1,
		    "Input image and profile image are not compatible")
	    }
	    call sprintf (Memc[str], SZ_LINE,
	        "EXTRACT - Using profile image %s for %s")
		call pargstr (profiles)
		call pargstr (input)
	    call ap_log (Memc[str], YES, YES, NO)
	}

	# Determine limits of apertures for use in defining memory requirements
	# and I/O.

	call salloc (a, 2 * napsex, TY_INT)
	call salloc (b, 2 * napsex, TY_INT)
	call salloc (c, 2 * napsex, TY_REAL)
	a1 = a - 1
	a2 = a1 + napsex
	b1 = b - 1
	b2 = b1 + napsex
	c1 = c - 1
	c2 = c1 + napsex

	# Initialize image interpolator for edge pixel weighting.
	switch (interptype) {
	case II_LINEAR:
	    interpbuf = 2
	case II_POLY3:
	    interpbuf = 3
	case II_SINC:
	    interpbuf = 16
	default:
	    interpbuf = 0
	}
	if (interptype > 0)
	    call asiinit (asi, interptype)
	else
	    asi = NULL

	na1 = 0
	do i = 1, napsex {
	    ap = Memi[apsex+i-1]
	    cv = AP_CV(ap)
	    ic = AP_IC(ap)

	    # Dispersion axis limits
	    bmin = min (nb, max (1, nint (AP_CEN(ap,baxis)+AP_LOW(ap,baxis))))
	    bmax = max (1, min (nb, nint (AP_CEN(ap,baxis)+AP_HIGH(ap,baxis))))

	    # Aperture axis shifts
	    if (cv != NULL) {
		cmin = MAX_REAL
	        cmax = -MAX_REAL
	        do j = bmin, bmax {
	           shift = ap_cveval (cv, real (j))
	           cmin = min (cmin, shift)
	           cmax = max (cmax, shift)
	        }
	    } else {
		cmin = 0.
		cmax = 0.
	    }

	    # Background region limits.
	    xmin = AP_LOW(ap,aaxis)
	    xmax = AP_HIGH(ap,aaxis)
	    if (weights == W_VARIANCE) {
		xmin = xmin - 2
		xmax = xmax + 2
	    }
	    xmin = xmin - interpbuf
	    xmax = xmax + interpbuf
	    if (bkg != B_NONE && AP_IC(ap) != NULL) {
		xmin = min (xmin, ic_getr (ic, "xmin"))
		xmax = max (xmax, ic_getr (ic, "xmax"))
	    }

	    Memi[a1+i] = min (namax, max (1, nint (AP_CEN(ap,aaxis)+xmin+cmin)))
	    Memi[a2+i] = max (1, min (namax, nint (AP_CEN(ap,aaxis)+xmax+cmax)))
	    Memi[b1+i] = bmin
	    Memi[b2+i] = bmax
	    Memr[c1+i] = cmin
	    Memr[c2+i] = cmax
	}
	call alimi (Memi[a], 2*napsex, amin, amax)
	call alimi (Memi[b], 2*napsex, bmin, bmax)

	# The maximum size of the image in memory is 80% of the maximum
	# working set size returned by begmem or 40% if a profile image
	# is used.  Later I/O may exceed this since at least one
	# aperture + background is needed in memory.

	new_size = begmem (0, old_size, max_size)
	namax = (amax - amin + 1)
	nb = (bmax - bmin + 1)
	if (pim == NULL)
	    namax = min (namax, int (0.8 * max_size / SZ_REAL / nb))
	else
	    namax = min (namax, int (0.8 * max_size / SZ_REAL / nb / 2))
	best_size = 1.2 * namax * nb * SZ_REAL
	new_size = begmem (best_size, old_size, max_size)

	# Allocate auxilary memory.  Some memory is only dependent on the
	# number of dispersion points and subapertures and is the same for
	# all apertures.  Other memory, such as the sky and profile depend on
	# the aperture widths and tilts which may vary.  The input data is
	# expected to have the aperture axis along the first dimension.  If
	# the image is in this orientation then the IMIO buffer is used.
	# Otherwise sequential I/O is used and transposed into the allocated
	# memory.

	iferr {
	    call salloc (astart, nb, TY_INT)
	    call salloc (spec, nsubaps * nb, TY_REAL)
	    if (weights == W_VARIANCE) {
		call salloc (raw, nsubaps * nb, TY_REAL)
		call salloc (specsig, nsubaps * nb, TY_REAL)
	    } else {
		raw = NULL
		specsig = NULL
	    }
	    profile = NULL
	    if (aaxis == 2) {
		call calloc (dbuf, namax * nb, TY_REAL)
		if (pim != NULL)
		    call calloc (pbuf, namax * nb, TY_REAL)
	    }

	    # For variance weighting the computations are done in photon units.
	    if (weights == W_VARIANCE) {
		gain = apgimr ("gain", im)
		rdnoise = apgimr ("readnoise", im)
	    } else {
		gain = 1
		rdnoise = 0
	    }

	    # Loop through each aperture doing the extractions.
	    amax = 0
	    do i = 1, napsex {
		ap = Memi[apsex+i-1]

		# Check if a new input data buffer is needed.  As many apertures
		# as possible are read at once within the given memory limits
		# though at least one aperture must be read.  Do a transpose if
		# needed.

		if (Memi[a1+i] < amin || Memi[a2+i] > amax) {
		    amin = Memi[a1+i]
		    amax = Memi[a2+i]
		    do j = i,  napsex {
			amin = min (amin, Memi[a1+j]) 
			amax = max (amax, Memi[a2+j]) 
			na = amax - amin + 1
			if (na > namax)
			    break
		    }

		    if (aaxis == 1) {
			if (fmt == DIFF) {
			    call mfree (dbuf, TY_REAL)
			    call malloc (dbuf, na*nb, TY_REAL)
			    call amovr (Memr[imgs2r(im,amin,amax,bmin,bmax)],
				Memr[dbuf], na*nb)
			} else
			    dbuf = imgs2r (im, amin, amax, bmin, bmax)
		    } else {
			if (na > namax) {
			    call mfree (dbuf, TY_REAL)
			    namax = na
			    call calloc (dbuf, namax * nb, TY_REAL)
			}
			do j = amin, amax {
			    sbuf = imgl2r (im, j)
			    sbuf = sbuf + bmin - 1
			    ptr = dbuf + j - amin
			    do k = bmin, bmax {
				Memr[ptr] = Memr[sbuf]
				sbuf = sbuf + 1
				ptr = ptr + na
			    }
			}
		    }
		    if (pim != NULL) {
			if (aaxis == 1)
			    pbuf = imgs2r (pim, amin, amax, bmin, bmax)
			else {
			    if (na > namax) {
				call mfree (pbuf, TY_REAL)
				namax = na
				call calloc (pbuf, namax * nb, TY_REAL)
			    }
			    do j = amin, amax {
				sbuf = imgl2r (pim, j)
				sbuf = sbuf + bmin - 1
				ptr = pbuf + j - amin
				do k = bmin, bmax {
				    Memr[ptr] = Memr[sbuf]
				    sbuf = sbuf + 1
				    ptr = ptr + na
				}
			    }
			}
		    }
		    if (weights == W_VARIANCE && gain != 1.) {
			j = na * nb
			call amulkr (Memr[dbuf], gain, Memr[dbuf], j)
			if (pim != NULL)
			    call amulkr (Memr[pbuf], gain, Memr[pbuf], j)
		    }
		}

		# To minimize memory a variable integer offset is used to
		# accomodate the aperture tilts.  The offsets are stored in
		# the astart array and the width of any one line determined.
		# If a stored profile is used it is read and it is ASSUMED to
		# be valid for the input aperture with the same ID.  If no
		# stored profile is found the profile fitting algorithm
		# parameter determines whether to fit 1D function along the
		# image axes (in which case all the profile offsets are the
		# same) or if the Marsh algorithm for tilted spectra is
		# used.  In the latter the offsets can be adjusted to mimize
		# memory and a buffer of two pixels around the aperture is
		# required by the algorithm.

		if (weights == W_NONE) {
		    xmin = AP_CEN(ap,aaxis) + AP_LOW(ap,aaxis)
		    xmax = AP_CEN(ap,aaxis) + AP_HIGH(ap,aaxis)
		    xmin = xmin - interpbuf
		    xmax = xmax + interpbuf
		    na1 = nint (xmax) - nint (xmin) + 1
		    cv = AP_CV(ap)
		    do j = bmin, bmax {
			shift = ap_cveval (cv, real (j))
			Memi[astart+j-bmin] = nint (xmin + shift)
		    }
		} else {
		    if (pfit == P_FIT1D) {
			xmin = AP_CEN(ap,aaxis) + AP_LOW(ap,aaxis) + Memr[c1+i]
			xmax = AP_CEN(ap,aaxis) + AP_HIGH(ap,aaxis) + Memr[c2+i]
			xmin = xmin - interpbuf
			xmax = xmax + interpbuf
			na1 = nint (xmax) - nint (xmin) + 1
			call amovki (nint (xmin), Memi[astart], nb)
		    } else if (pfit == P_FIT2D) {
			xmin = AP_CEN(ap,aaxis) + AP_LOW(ap,aaxis) - 2
			xmax = AP_CEN(ap,aaxis) + AP_HIGH(ap,aaxis) + 2
			xmin = xmin - interpbuf
			xmax = xmax + interpbuf
			na1 = nint (xmax) - nint (xmin) + 1
			cv = AP_CV(ap)
			do j = bmin, bmax {
			    shift = ap_cveval (cv, real (j))
			    Memi[astart+j-bmin] = nint (xmin + shift)
			}
		    }
		}

		# Do the sky or background determination if needed.  An array
		# of the same size as the 2D aperture is returned as well as
		# a single estimate of the variance in the sky value at each
		# line based on the fit.  If a profile image is used then the
		# sky is for the profile image and the object sky is
		# determined later in order to reuse the sky buffers.

		if (bkg != B_NONE && AP_IC(ap) != NULL) {
		    call malloc (sbuf, na1 * nb, TY_REAL)
		    call malloc (svar, nb, TY_REAL)
		    call malloc (specsky, nsubaps * nb, TY_REAL)
		    if (pim == NULL)
			call ap_skyeval (im, ap, dbuf, na, nb, amin, 1,
			    Memr[sbuf], Memr[svar], Memr[specsky], na1, nb,
			    Memi[astart], 1, nsubaps, rdnoise)
		    else
			call ap_skyeval (pim, ap, pbuf, na, nb, amin, 1,
			    Memr[sbuf], Memr[svar], Memr[specsky], na1, nb,
			    Memi[astart], 1, nsubaps, rdnoise)
		} else {
		    sbuf = NULL
		    svar = NULL
		    specsky = NULL 
		}

		# Use a quick sum for unweighted extraction.  For weighed
		# extractions we use either a previously determined profile
		# or call the profile routine.  If desired the profile is
		# stored for later use.  Then the variance weighted
		# extraction routine is called.

		if (weights == W_NONE)
		    call ap_sum (ap, dbuf, na, nb, amin, 1, sbuf, na1, nb,
			Memi[astart], 1, Memr[spec], nsubaps, asi)
		else {
		    call malloc (profile, na1 * nb, TY_REAL)
		    if (pim == NULL)
			call ap_profile (im, ap, dbuf, na, nb, amin, 1, sbuf,
			    svar, Memr[profile], na1, nb, Memi[astart], 1,
			    asi)
		    else {
			call ap_profile (pim, ap, pbuf, na, nb, amin, 1, sbuf,
			    svar, Memr[profile], na1, nb, Memi[astart], 1,
			    asi)
			if (sbuf != NULL)
			    call ap_skyeval (im, ap, dbuf, na, nb, amin, 1,
				Memr[sbuf], Memr[svar], Memr[specsky], na1, nb,
				Memi[astart], 1, nsubaps, rdnoise)
		    }

		    call ap_variance (im, ap, dbuf, na, nb, amin, 1, sbuf, svar,
			Memr[profile], na1, nb, Memi[astart], 1, Memr[spec],
			Memr[raw], Memr[specsig], nsubaps, asi)
		}

		# Output the extracted spectrum.  The extras of sky, sigma,
		# and unweighted spectrum may also be stored.  If the extra
		# information is not available the pointers will be NULL.

		if (weights == W_VARIANCE && gain != 1.) {
		    call adivkr (Memr[spec], gain, Memr[spec], nb)
		    if (raw != NULL)
			call adivkr (Memr[raw], gain, Memr[raw], nb)
		    if (specsky != NULL)
			call adivkr (Memr[specsky], gain, Memr[specsky], nb)
		    if (specsig != NULL)
			call adivkr (Memr[specsig], gain, Memr[specsig], nb)
		    call amulkr (Memr[profile], gain, Memr[profile], nb*na1)
		}

		call ap_output (input, output, format, Memc[bkgstr],
		    Memc[wtstr], Memc[cleanstr], gain, im, Memi[apsex], napsex,
		    i, nsubaps, spec, raw, specsky, specsig, dbuf, na, nb, amin,
		    1, sbuf, profile, na1, nb, Memi[astart], 1)

		call mfree (profile, TY_REAL)
		call mfree (sbuf, TY_REAL)
		call mfree (svar, TY_REAL)
		call mfree (specsky, TY_REAL)
	    }

	    # Finish up and restore the working set size.
	    if (asi != NULL)
		call asifree (asi)
	    if (pim != NULL) {
		if (aaxis == 2)
		    call mfree (pbuf, TY_REAL)
		call imunmap (pim)
	    }
	    if (aaxis == 2)
		call mfree (dbuf, TY_REAL)
	    call imunmap (im)
	    call fixmem (old_size)
	    call sfree (sp)

	} then {
	    call mfree (profile, TY_REAL)
	    call mfree (sbuf, TY_REAL)
	    call mfree (svar, TY_REAL)
	    call mfree (specsky, TY_REAL)

	    if (asi != NULL)
		call asifree (asi)
	    if (pim != NULL) {
		if (aaxis == 2)
		    call mfree (pbuf, TY_REAL)
		call imunmap (pim)
	    }
	    if (aaxis == 2)
		call mfree (dbuf, TY_REAL)
	    call imunmap (im)
	    call fixmem (old_size)
	    call sfree (sp)

	    call erract (EA_ERROR)
	}
end


# AP_CHECK -- Check if output spectra exist.  If the user allows clobbering,
# delete the spectra.  Return the format.

int procedure ap_check (input, output, format, aps, naps, nsubaps)

char	input[ARB]			# Input image name
char	output[ARB]			# Output root name
char	format[ARB]			# Output format
pointer	aps[naps]			# Apertures
int	naps				# Number of apertures
int	nsubaps				# Number of subapertures

int	i, j, fmt
pointer	sp, name, name1, input1, ksection, ans

int	strdic(), imaccess(), stridxs()
bool	streq(), ap_answer()

begin
	call smark (sp)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (name1, SZ_LINE, TY_CHAR)
	call salloc (input1, SZ_LINE, TY_CHAR)
	call salloc (ksection, SZ_LINE, TY_CHAR)
	call salloc (ans, SZ_LINE, TY_CHAR)

	fmt = strdic (format, format, SZ_LINE, FORMATS)
	call imgimage (input, Memc[input1], SZ_LINE)

	switch (fmt) {
	case MULTISPEC, NORM, FLAT, RATIO, DIFF, FIT:
	    i = stridxs ("[", Memc[input1])
	    if (i > 0) {
		call strcpy (Memc[input1+i-1], Memc[ksection], SZ_LINE)
		Memc[input1+i-1] = EOS
	    } else
		Memc[ksection] = EOS
	    if (output[1] == EOS)
	        call strcpy (Memc[input1], Memc[name], SZ_LINE)
	    else
	        call strcpy (output, Memc[name], SZ_LINE)

	    switch (fmt) {
	    case MULTISPEC:
		if (streq (Memc[input1], Memc[name])) {
	            call strcat (".ms", Memc[name], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name], SZ_LINE)
		}
	    case NORM:
		if (streq (Memc[input1], Memc[name])) {
	            call strcat (".norm", Memc[name], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name], SZ_LINE)
		}
	    case FLAT:
		if (streq (Memc[input1], Memc[name])) {
	            call strcat (".flat", Memc[name], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name], SZ_LINE)
		}
	    case RATIO:
		if (streq (Memc[input1], Memc[name])) {
	            call strcat (".ratio", Memc[name], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name], SZ_LINE)
		}
	    case DIFF:
		if (streq (Memc[input1], Memc[name])) {
	            call strcat (".diff", Memc[name], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name], SZ_LINE)
		}
	    case FIT:
		if (streq (Memc[input1], Memc[name])) {
	            call strcat (".fit", Memc[name], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name], SZ_LINE)
		}
	    }
	    if (imaccess (Memc[name], 0) == YES) {
		call sprintf (Memc[ans], SZ_LINE,
		    "Clobber existing output image %s?")
		    call pargstr (Memc[name])
		if (ap_answer ("ansclobber1", Memc[ans]))
		    call imdelete (Memc[name])
		else {
		    call sprintf (Memc[ans], SZ_LINE,
			"EXTRACT - Output spectrum %s already exists")
			call pargstr (Memc[name])
		    call ap_log (Memc[ans], YES, NO, YES)
		    fmt = 0
		}
	    }
	case ECHELLE:
	    if (output[1] == EOS)
	        call strcpy (Memc[input1], Memc[name], SZ_LINE)
	    else
	        call strcpy (output, Memc[name], SZ_LINE)

	    do i = 1, nsubaps {
		if (nsubaps == 1)
		    call strcpy (Memc[name], Memc[name1], SZ_LINE)
		else {
		    call sprintf (Memc[name1], SZ_LINE, "%s%0*d")
			call pargstr (Memc[name])
			call pargi (int(log10(real(nsubaps)))+1)
			call pargi (i)
		}
		if (streq (Memc[input1], Memc[name])) {
		    call strcat (".ec", Memc[name1], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name1], SZ_LINE)
		}

		if (imaccess (Memc[name1], 0) == YES) {
		    call sprintf (Memc[ans], SZ_LINE,
			"Clobber existing output image %s?")
			call pargstr (Memc[name1])
		    if (ap_answer ("ansclobber1", Memc[ans]))
			call imdelete (Memc[name1])
		    else {
			call sprintf (Memc[ans], SZ_LINE,
			    "EXTRACT - Output spectrum %s already exists")
			    call pargstr (Memc[name1])
			call ap_log (Memc[ans], YES, NO, YES)
			fmt = 0
		    }
		}
	    }
	case ONEDSPEC, STRIP:
	    do i = 1, naps {
		do j = 1, nsubaps {
		    call sprintf (Memc[name], SZ_LINE, "%s.%0*d")
			if (output[1] == EOS)
			    call pargstr (Memc[input1])
			else
			    call pargstr (output)
			call pargi (int(log10(real(nsubaps)))+4)
			call pargi (AP_ID(aps[i])+(j-1)*1000)
		    if (imaccess (Memc[name], 0) == YES) {
			call sprintf (Memc[ans], SZ_LINE,
			    "Clobber existing output image %s?")
			    call pargstr (Memc[name])
			if (ap_answer ("ansclobber1", Memc[ans]))
			    call imdelete (Memc[name])
			else {
			    call sprintf (Memc[ans], SZ_LINE,
				"EXTRACT - Output spectrum %s already exists")
				call pargstr (Memc[name])
			    call ap_log (Memc[ans], YES, NO, YES)
			    fmt = 0
			}
		    }
		}
	    }
	case NOISE:
	    ;
	default:
	    call sfree (sp)
	    call error (1, "EXTRACT - Unknown output format")
	}

	call sfree (sp)
	return (fmt)
end


# AP_OUTPUT -- Review the extracted spectra and write them to an image.
# This routine determines the output format and whether to also output sky
# unweighted, and sigma spectra.  The appropriate header keywords have
# to be added.

procedure ap_output (image, output, format, bkg, wt, clean, gain, in, aps,
	naps, iap, nsubaps, spec, raw sky, sig, dbuf, nc, nl, c1, l1, sbuf,
	profile, nx, ny, xs, ys)

char	image[ARB]		# Input image name
char	output[ARB]		# Output root name
char	format[ARB]		# Output format
char	bkg[ARB]		# Background type
char	wt[ARB]			# Weight type
char	clean[ARB]		# Clean?
real	gain			# Gain
pointer	in			# Input IMIO pointer
pointer	aps[naps]		# Apertures
int	naps			# Number of apertures
int	iap			# Aperture
int	nsubaps			# Number of subapertures
pointer	spec			# Output spectrum
pointer	raw			# Output raw spectrum
pointer	sky			# Output sky
pointer	sig			# Output sigma
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
pointer	sbuf			# Sky values (NULL if none)
pointer	profile			# Profile (NULL if none)
int	nx, ny			# Size of sky and profile array
int	xs[ny], ys		# Origin of sky and profile array

int	fmt			# Output format
bool	extras			# Include raw spectrum, sky, and sigma

real	low, high, step
int	i, k, l, m, apid, apaxis, dispaxis
pointer	sp, str, str1, name, name1, input, ksection
pointer	ap, out, outsave, gt, apmw, buf
pointer	sum2, sum4, nsum

real	clgetr()
int	scan(), strdic(), imaccf(), stridxs()
bool	streq(), ap_answer(), apgetb()
pointer	immap(), imgl2r(), impl2r(), impl3r()
pointer	gt_init(), apmw_open()
errchk	immap, impl2r, impl3r, imps2r, ap_strip, ap_pstrip, apmw_open
errchk	ap_fitspec, ap_lnorm, ap_cnorm, ap_lflat, ap_cflat

begin
	# Allocate string and file name arrays.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (name1, SZ_LINE, TY_CHAR)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (ksection, SZ_LINE, TY_CHAR)

	fmt = strdic (format, format, SZ_LINE, FORMATS)
	extras = apgetb ("extras")

	ap = aps[iap]
	apaxis = AP_AXIS(ap)
	dispaxis = mod (apaxis, 2) + 1

	# Set output name.
	call imgimage (image, Memc[input], SZ_LINE)
	i = stridxs ("[", Memc[input])
	if (i > 0) {
	    call strcpy (Memc[input+i-1], Memc[ksection], SZ_LINE)
	    Memc[input+i-1] = EOS
	    i = stridxs ("]", Memc[ksection])
	    call strcpy (",append]", Memc[ksection+i-1], SZ_LINE)
	} else
	    Memc[ksection] = EOS
	if (output[1] == EOS)
	    call strcpy (Memc[input], Memc[name], SZ_LINE)
	else
	    call strcpy (output, Memc[name], SZ_LINE)

	switch (fmt) {
	case ECHELLE:
	    ;
	case MULTISPEC:
	    if (streq (Memc[input], Memc[name])) {
		call strcat (".ms", Memc[name], SZ_LINE)
		call strcat (Memc[ksection], Memc[name], SZ_LINE)
	    }
	case NORM:
	    if (streq (Memc[input], Memc[name])) {
	        call strcat (".norm", Memc[name], SZ_LINE)
		call strcat (Memc[ksection], Memc[name], SZ_LINE)
	    }
	case FLAT:
	    if (streq (Memc[input], Memc[name])) {
	        call strcat (".flat", Memc[name], SZ_LINE)
		call strcat (Memc[ksection], Memc[name], SZ_LINE)
	    }
	case RATIO:
	    if (streq (Memc[input], Memc[name])) {
	        call strcat (".ratio", Memc[name], SZ_LINE)
		call strcat (Memc[ksection], Memc[name], SZ_LINE)
	    }
	case DIFF:
	    if (streq (Memc[input], Memc[name])) {
	        call strcat (".diff", Memc[name], SZ_LINE)
		call strcat (Memc[ksection], Memc[name], SZ_LINE)
	    }
	case FIT:
	    if (streq (Memc[input], Memc[name])) {
	        call strcat (".fit", Memc[name], SZ_LINE)
		call strcat (Memc[ksection], Memc[name], SZ_LINE)
	    }
	case NOISE:
	    Memc[name] = EOS
	}


	# Set the review graph title.
	call sprintf (Memc[str], SZ_LINE, "%s: %s - Aperture %s")
	    call pargstr (image)
	    call pargstr (IM_TITLE(in))
	    call pargi (AP_ID(ap))

	gt = gt_init ()
	call gt_sets (gt, GTTITLE, Memc[str])

	# Query the user whether to review the extraction.
	call sprintf (Memc[str], SZ_LINE,
	    "Review extracted spectrum for aperture %d from %s?")
	    call pargi (AP_ID(ap))
	    call pargstr (image)

	# If reviewing graph the spectrum, do a cursor loop, and allow
	# the user to skip the output or define a new output image.
	if (ap_answer ("ansreview1", Memc[str])) {
	    call ap_graph1 (gt, Memr[spec], ny, nsubaps)
	    
	    if (fmt == ONEDSPEC && nsubaps == 1) {
	        call printf (
		    "Output image name [use # to skip output] (%s): ")
		    call pargstr (Memc[name])
		call flush (STDOUT)
	        if (scan() != EOF) {
	            call gargwrd (Memc[str], SZ_LINE)
	            if (Memc[str] == '#') {
			call gt_free (gt)
			call sfree (sp)
		        return
		    }
	            if (Memc[str] != EOS)
		        call strcpy (Memc[str], Memc[name], SZ_LINE)
		}
	    }
	}

	# Output the image.
	switch (fmt) {
	case MULTISPEC:
	    if (iap == 1) {
		out = immap (Memc[name], NEW_COPY, in)

		IM_PIXTYPE(out) = TY_REAL
		IM_NDIM(out) = 1
		IM_LEN(out, 1) = ny
		IM_LEN(out, 2) = nsubaps * naps
		IM_LEN(out, 3) = 1
		if (extras) {
		    if (sky != NULL)
		        IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		    if (raw != NULL)
		        IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		    if (sig != NULL)
		        IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		}
		if (IM_LEN(out, 2) > 1)
		    IM_NDIM(out) = 2
		if (IM_LEN(out, 3) > 1)
		    IM_NDIM(out) = 3

		apmw = apmw_open (in, out, dispaxis, nsubaps*naps, ny)

		# Write BAND IDs.
		k = 1
		call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
		    call pargi (k)
		call sprintf (Memc[str], SZ_LINE,
		    "spectrum - background %s, weights %s, clean %s")
		    call pargstr (bkg)
		    call pargstr (wt)
		    call pargstr (clean)
		call imastr (out, Memc[str1], Memc[str])
		k = k + 1
		if (extras) {
		    if (raw != NULL) {
			call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			    call pargi (k)
			call sprintf (Memc[str], SZ_LINE,
			    "raw - background %s, weights none, clean no")
			    call pargstr (bkg)
			call imastr (out, Memc[str1], Memc[str])
			k = k + 1
		    }
		    if (sky != NULL) {
			call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			    call pargi (k)
			call sprintf (Memc[str], SZ_LINE,
			    "background - background %s")
			    call pargstr (bkg)
			call imastr (out, Memc[str1], Memc[str])
			k = k + 1
		    }
		    if (sig != NULL) {
			call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			    call pargi (k)
			call sprintf (Memc[str], SZ_LINE,
			    "sigma - background %s, weights %s, clean %s")
			    call pargstr (bkg)
			    call pargstr (wt)
			    call pargstr (clean)
			call imastr (out, Memc[str1], Memc[str])
		    }
		}

		do k = 1, naps {
		    low = AP_CEN(aps[k],apaxis) + AP_LOW(aps[k],apaxis)
		    high = AP_CEN(aps[k],apaxis) + AP_HIGH(aps[k],apaxis)
		    step = (high - low) / nsubaps
		    low = low - step
		    do l = 1, nsubaps {
			apid = AP_ID(aps[k]) + (l - 1) * 1000
			low = low + step
			high = low + step
			call apmw_setap (apmw, (k-1)*nsubaps+l,
			    apid, AP_BEAM(aps[k]), low, high)
		    }
		}
		do k = 1, naps {
		    if (AP_TITLE(aps[k]) != NULL) {
			do l = 1, nsubaps {
			    call sprintf (Memc[str], SZ_LINE, "APID%d")
				call pargi ((k-1)*nsubaps+l)
		    	    call imastr (out, Memc[str],
				Memc[AP_TITLE(aps[k])])
			}
		    }
		}
	    }
	
	    do l = 1, nsubaps {
		k = (iap - 1) * nsubaps + l
		buf = impl2r (out, k)
	        call amovr (Memr[spec+(l-1)*ny], Memr[buf], ny)
		if (extras) {
		    m = 2
		    if (raw != NULL) {
			buf = impl3r (out, k, m)
	                call amovr (Memr[raw+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		    if (sky != NULL) {
			buf = impl3r (out, k, m)
	                call amovr (Memr[sky+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		    if (sig != NULL) {
			buf = impl3r (out, k, m)
	                call amovr (Memr[sig+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		}
	    }
	    if (iap == naps) {
		call apmw_saveim (apmw, out, fmt)
		call apmw_close (apmw)
	        call imunmap (out)
	    }

	    if (Memc[name] != EOS) {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s")
		    call pargi (AP_ID(ap))
		    call pargstr (image)
		    call pargstr (Memc[name])
		call ap_log (Memc[str], YES, YES, NO)
		call ap_plot1 (gt, Memr[spec], ny, nsubaps)
	    }

	case ECHELLE:
	    do l = 1, nsubaps {
		if (nsubaps == 1)
		    call strcpy (Memc[name], Memc[name1], SZ_LINE)
		else {
		    call sprintf (Memc[name1], SZ_LINE, "%s%0*d")
			call pargstr (Memc[name])
			call pargi (int(log10(real(nsubaps)))+1)
			call pargi (l)
		}
		if (streq (Memc[input], Memc[name])) {
		    call strcat (".ec", Memc[name1], SZ_LINE)
		    call strcat (Memc[ksection], Memc[name1], SZ_LINE)
		}

		if (iap == 1) {
		    out = immap (Memc[name1], NEW_COPY, in)

		    IM_PIXTYPE(out) = TY_REAL
		    IM_NDIM(out) = 1
		    IM_LEN(out, 1) = ny
		    IM_LEN(out, 2) = naps
		    IM_LEN(out, 3) = 1
		    if (extras) {
			if (sky != NULL)
			    IM_LEN(out, 3) = IM_LEN(out, 3) + 1
			if (raw != NULL)
			    IM_LEN(out, 3) = IM_LEN(out, 3) + 1
			if (sig != NULL)
			    IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		    }
		    if (IM_LEN(out, 2) > 1)
			IM_NDIM(out) = 2
		    if (IM_LEN(out, 3) > 1)
			IM_NDIM(out) = 3

		    apmw = apmw_open (in, out, dispaxis, naps, ny)

		    # Write BAND IDs.
		    k = 1
		    call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			call pargi (k)
		    call sprintf (Memc[str], SZ_LINE,
			"spectrum - background %s, weights %s, clean %s")
			call pargstr (bkg)
			call pargstr (wt)
			call pargstr (clean)
		    call imastr (out, Memc[str1], Memc[str])
		    k = k + 1
		    if (extras) {
			if (raw != NULL) {
			    call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
				call pargi (k)
			    call sprintf (Memc[str], SZ_LINE,
				"raw - background %s, weights none, clean no")
				call pargstr (bkg)
			    call imastr (out, Memc[str1], Memc[str])
			    k = k + 1
			}
			if (sky != NULL) {
			    call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
				call pargi (k)
			    call sprintf (Memc[str], SZ_LINE,
				"background - background %s")
				call pargstr (bkg)
			    call imastr (out, Memc[str1], Memc[str])
			    k = k + 1
			}
			if (sig != NULL) {
			    call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
				call pargi (k)
			    call sprintf (Memc[str], SZ_LINE,
				"sigma - background %s, weights %s, clean %s")
				call pargstr (bkg)
				call pargstr (wt)
				call pargstr (clean)
			    call imastr (out, Memc[str1], Memc[str])
			}
		    }

		    # Write keyword to allow matching by subaperture.
		    if (nsubaps > 1)
			call imaddi (out, "SUBAP", l)

		    do k = 1, naps {
			low = AP_CEN(aps[k],apaxis) + AP_LOW(aps[k],apaxis)
			high = AP_CEN(aps[k],apaxis) + AP_HIGH(aps[k],apaxis)
			step = (high - low) / nsubaps
			call apmw_setap (apmw, k, AP_ID(aps[k]),
			    AP_BEAM(aps[k]), low+(l-1)*step, low+l*step)
		    }
		    do k = 1, naps {
			if (AP_TITLE(aps[k]) != NULL) {
			    call sprintf (Memc[str], SZ_LINE, "APID%d")
				call pargi (k)
			    call imastr (out, Memc[str],
				Memc[AP_TITLE(aps[k])])
			}
		    }
		} else {
		    if (l == 1)
			out = outsave
		    else
			out = immap (Memc[name1], READ_WRITE, 0)
		}
	    
		k = iap
		buf = impl2r (out, k)
		call amovr (Memr[spec+(l-1)*ny], Memr[buf], ny)
		if (extras) {
		    m = 2
		    if (raw != NULL) {
			buf = impl3r (out, k, m)
			call amovr (Memr[raw+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		    if (sky != NULL) {
			buf = impl3r (out, k, m)
			call amovr (Memr[sky+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		    if (sig != NULL) {
			buf = impl3r (out, k, m)
			call amovr (Memr[sig+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		}

		if (iap == 1) {
		    call apmw_saveim (apmw, out, fmt)
		    call apmw_close (apmw)
		}
		if (l != 1 || iap == naps)
		    call imunmap (out)
		if (l == 1)
		    outsave = out

		if (nsubaps == 1) {
		    call sprintf (Memc[str], SZ_LINE,
			"EXTRACT - Aperture %d from %s --> %s")
			call pargi (AP_ID(ap))
			call pargstr (image)
			call pargstr (Memc[name1])
		} else {
		    call sprintf (Memc[str], SZ_LINE,
			"EXTRACT - Aperture %d-%d from %s --> %s")
			call pargi (AP_ID(ap))
			call pargi (l)
			call pargstr (image)
			call pargstr (Memc[name1])
		}
		call ap_log (Memc[str], YES, YES, NO)
	    }

	    call ap_plot1 (gt, Memr[spec], ny, nsubaps)
			
	case ONEDSPEC:
	    do l = 1, nsubaps {
		apid = AP_ID(ap) + (l - 1) * 1000
		low = AP_CEN(ap,apaxis) + AP_LOW(ap,apaxis)
		high = AP_CEN(ap,apaxis) + AP_HIGH(ap,apaxis)
		step = (high - low) / nsubaps
		low = low + (l - 1) * step
		high = low + step

		call sprintf (Memc[str], SZ_LINE, "%s.%0*d")
		    call pargstr (Memc[name])
		    call pargi (int(log10(real(nsubaps)))+4)
		    call pargi (apid)
		out = immap (Memc[str], NEW_COPY, in)
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s.%0*d")
		    call pargi (apid)
		    call pargstr (image)
		    call pargstr (Memc[name])
		    call pargi (int(log10(real(nsubaps)))+4)
		    call pargi (apid)
		call ap_log (Memc[str], YES, YES, NO)

		apmw = apmw_open (in, out, dispaxis, 1, ny)
		call apmw_setap (apmw, 1, apid, AP_BEAM(ap), low, high)
		if (AP_TITLE(ap) != NULL)
		    call imastr (out, "APID1", Memc[AP_TITLE(ap)])

		IM_PIXTYPE(out) = TY_REAL
		IM_NDIM(out) = 1
		IM_LEN(out, 1) = ny
		IM_LEN(out, 2) = 1
		IM_LEN(out, 3) = 1
		if (extras) {
		    if (sky != NULL)
			IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		    if (raw != NULL)
			IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		    if (sig != NULL)
			IM_LEN(out, 3) = IM_LEN(out, 3) + 1
		}
		if (IM_LEN(out, 2) > 1)
		    IM_NDIM(out) = 2
		if (IM_LEN(out, 3) > 1)
		    IM_NDIM(out) = 3

		# Write BAND IDs.
		k = 1
		call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
		    call pargi (k)
		call sprintf (Memc[str], SZ_LINE,
		    "spectrum: background %s, weights %s, clean %s")
		    call pargstr (bkg)
		    call pargstr (wt)
		    call pargstr (clean)
		call imastr (out, Memc[str1], Memc[str])
		k = k + 1
		if (extras) {
		    if (raw != NULL) {
			call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			    call pargi (k)
			call sprintf (Memc[str], SZ_LINE,
			    "spectrum: background %s, weights none, clean no")
			    call pargstr (bkg)
			call imastr (out, Memc[str1], Memc[str])
			k = k + 1
		    }
		    if (sky != NULL) {
			call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			    call pargi (k)
			call sprintf (Memc[str], SZ_LINE,
			    "background: background %s")
			    call pargstr (bkg)
			call imastr (out, Memc[str1], Memc[str])
			k = k + 1
		    }
		    if (sig != NULL) {
			call sprintf (Memc[str1], SZ_LINE, "BANDID%d")
			    call pargi (k)
			call sprintf (Memc[str], SZ_LINE,
			    "sigma - background %s, weights %s, clean %s")
			    call pargstr (bkg)
			    call pargstr (wt)
			    call pargstr (clean)
			call imastr (out, Memc[str1], Memc[str])
		    }
		}

		buf = impl2r (out, 1)
	        call amovr (Memr[spec+(l-1)*ny], Memr[buf], ny)
		if (extras) {
		    m = 2
		    if (raw != NULL) {
			buf = impl3r (out, 1, m)
	                call amovr (Memr[raw+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		    if (sky != NULL) {
			buf = impl3r (out, 1, m)
	                call amovr (Memr[sky+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		    if (sig != NULL) {
			buf = impl3r (out, 1, m)
	                call amovr (Memr[sig+(l-1)*ny], Memr[buf], ny)
			m = m + 1
		    }
		}

		call apmw_saveim (apmw, out, fmt)
		call apmw_close (apmw)
		call imunmap (out)

	    }

	    call ap_plot1 (gt, Memr[spec], ny, nsubaps)

	case STRIP:
	    do l = 1, nsubaps {
		apid = AP_ID(ap) + (l - 1) * 1000
		low = AP_CEN(ap,apaxis) + AP_LOW(ap,apaxis)
		high = AP_CEN(ap,apaxis) + AP_HIGH(ap,apaxis)
		step = (high - low) / nsubaps
		low = low + (l - 1) * step
		high = low + step

		call sprintf (Memc[str], SZ_LINE, "%s.%0*d")
		    call pargstr (Memc[name])
		    call pargi (int(log10(real(nsubaps)))+4)
		    call pargi (apid)
		out = immap (Memc[str], NEW_COPY, in)
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s.%0*d")
		    call pargi (apid)
		    call pargstr (image)
		    call pargstr (Memc[name])
		    call pargi (int(log10(real(nsubaps)))+4)
		    call pargi (apid)
		call ap_log (Memc[str], YES, YES, NO)

		apmw = apmw_open (in, out, dispaxis, 1, ny)
		call apmw_setap (apmw, 1, apid, AP_BEAM(ap), low, high)
		call sprintf (Memc[str], SZ_LINE, "%s - Aperture %d")
		    call pargstr (IM_TITLE(out))
		    call pargi (AP_ID(ap))
		call strcpy (Memc[str], IM_TITLE(out), SZ_IMTITLE)
		if (AP_TITLE(ap) != NULL)
		    call imastr (out, "APID1", Memc[AP_TITLE(ap)])

		IM_PIXTYPE(out) = TY_REAL
		IM_NDIM(out) = 2
		IM_LEN(out, 1) = ny
		IM_LEN(out, 2) = high - low + 1

		if (profile == NULL)
		    call ap_strip (ap, low, high, out, dbuf, nc, nl, c1, l1,
			sbuf, nx, ny, xs, ys)
		else
		    call ap_pstrip (ap, low, high, out, gain, Memr[spec],
			Memr[profile], nx, ny, xs, ys)

		call apmw_saveim (apmw, out, fmt)
		call apmw_close (apmw)
		call imunmap (out)
	    }

	    call ap_plot1 (gt, Memr[spec], ny, nsubaps)

	case NORM, FLAT:
	    if (iap == 1) {
	        out = immap (Memc[name], NEW_COPY, in)
	        IM_PIXTYPE(out) = TY_REAL
		if (imaccf (out, "CCDMEAN") == YES)
		    call imdelf (out, "CCDMEAN")
	        call ap_fitspec (ap, in, Memr[spec], ny)
		k = YES
	    } else {
	        call ap_fitspec (ap, in, Memr[spec], ny)
		k = NO
	    }
	    if (apaxis == 1) {
		if (fmt == NORM)
		    call ap_lnorm (ap, out, gain, dbuf, nc, nl, c1, l1,
			Memr[spec], ny, ys, k)
		else
		    call ap_lflat (ap, out, dbuf, nc, nl, c1, l1, Memr[spec],
		        sbuf, Memr[profile], nx, ny, xs, ys, k)
	    } else {
		if (fmt == NORM)
		    call ap_cnorm (ap, out, gain, dbuf, nc, nl, c1, l1,
			Memr[spec], ny, ys, k)
		else
		    call ap_cflat (ap, out, dbuf, nc, nl, c1, l1, Memr[spec],
		        sbuf, Memr[profile], nx, ny, xs, ys, k)
	    }
	    if (iap == naps)
	        call imunmap (out)

	    if (Memc[name] != EOS) {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s")
		    call pargi (AP_ID(ap))
		    call pargstr (image)
		    call pargstr (Memc[name])
		call ap_log (Memc[str], YES, YES, NO)
		call ap_plot1 (gt, Memr[spec], ny, nsubaps)
	    }

	case RATIO, FIT:
	    if (iap == 1) {
		out = immap (Memc[name], NEW_COPY, in)
	        IM_PIXTYPE(out) = TY_REAL
		k = YES
	    } else
		k = NO
	    if (apaxis == 1) {
		switch (fmt) {
		case RATIO:
		    call ap_lflat (ap, out, dbuf, nc, nl, c1, l1, Memr[spec],
		        sbuf, Memr[profile], nx, ny, xs, ys, k)
		case FIT:
		    call ap_lfit (ap, out, gain, Memr[spec], Memr[profile],
			nx, ny, xs, ys, k)
		}
	    } else {
		switch (fmt) {
		case RATIO:
		    call ap_cflat (ap, out, dbuf, nc, nl, c1, l1, Memr[spec],
		        sbuf, Memr[profile], nx, ny, xs, ys, k)
		case FIT:
		    call ap_cfit (ap, out, gain, Memr[spec], Memr[profile],
			nx, ny, xs, ys, k)
		}
	    }
	    if (iap == naps)
	        call imunmap (out)

	    if (Memc[name] != EOS) {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s")
		    call pargi (AP_ID(ap))
		    call pargstr (image)
		    call pargstr (Memc[name])
		call ap_log (Memc[str], YES, YES, NO)
		call ap_plot1 (gt, Memr[spec], ny, nsubaps)
	    }

	case DIFF:
	    if (iap == 1) {
	        out = immap (Memc[name], NEW_COPY, in)
	        IM_PIXTYPE(out) = TY_REAL
		do k = 1, IM_LEN(in,2) {
		    buf = impl2r (out, k)
		    call amovr (Memr[imgl2r(in,k)], Memr[buf], IM_LEN(out,1))
		}
		k = NO
	    } else
		k = NO
	    if (apaxis == 1)
		call ap_ldiff (ap, out, gain, dbuf, nc, nl, c1, l1, Memr[spec],
		    Memr[profile], nx, ny, xs, ys, k)
	    else
		call ap_cdiff (ap, out, gain, dbuf, nc, nl, c1, l1, Memr[spec],
		    Memr[profile], nx, ny, xs, ys, k)
	    if (iap == naps)
	        call imunmap (out)

	    if (Memc[name] != EOS) {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s")
		    call pargi (AP_ID(ap))
		    call pargstr (image)
		    call pargstr (Memc[name])
		call ap_log (Memc[str], YES, YES, NO)
		call ap_plot1 (gt, Memr[spec], ny, nsubaps)
	    }

	case NOISE:
	    if (iap == 1) {
		low = clgetr ("dmin")
		high = clgetr ("dmax")
		l = clgetr ("nbins")
		if (high < low) {
		    step = low; low = high; high = step
		}
		step = (high - low) / l
		call malloc (sum2, l, TY_REAL)
		call malloc (sum4, l, TY_REAL)
		call malloc (nsum, l, TY_INT)
		call aclrr (Memr[sum2], l)
		call aclrr (Memr[sum4], l)
		call aclri (Memi[nsum], l)
	    }
	    call ap_noise (ap, gain, dbuf, nc, nl, c1, l1, sbuf, Memr[spec],
		Memr[profile], nx, ny, xs, ys, Memr[sum2], Memr[sum4],
		Memi[nsum], l, low, high)
	    if (iap == naps) {
		do k = 0, l-1 {
		    m = Memi[nsum+k]
		    if (m > 10) {
			Memr[sum2+k] = sqrt (Memr[sum2+k] / (m - 1))
			step = max (0., Memr[sum4+k] / m - Memr[sum2+k]**2)
			Memr[sum4+k] = sqrt (sqrt (step / m))
		    } else {
			Memr[sum2+k] = 0.
			Memr[sum4+k] = 0.
		    }
		}
		call ap_nplot (image, in, Memr[sum2], Memr[sum4], l,
		    low, high)
		call mfree (sum2, TY_REAL)
		call mfree (sum4, TY_REAL)
		call mfree (nsum, TY_INT)
	    }

	    if (Memc[name] != EOS) {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT - Aperture %d from %s --> %s")
		    call pargi (AP_ID(ap))
		    call pargstr (image)
		    call pargstr (Memc[name])
		call ap_log (Memc[str], YES, YES, NO)
		call ap_plot1 (gt, Memr[spec], ny, nsubaps)
	    }
	}

	call gt_free (gt)
	call sfree (sp)
end


# AP_SUM -- Simple, unweighted aperture sum.

procedure ap_sum (ap, dbuf, nc, nl, c1, l1, sbuf, nx, ny, xs, ys, spec,
	nsubaps, asi)

pointer	ap			# Aperture structure
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
pointer	sbuf			# Sky values (NULL if none)
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Origin of sky array
real	spec[ny, nsubaps]	# Spectrum
int	nsubaps			# Number of subapertures
pointer	asi			# Interpolator for edge pixel weighting

int	i, ix, iy, ix1, ix2
real	low, high, step, x1, x2, wt1, wt2, s, sval, skyval
real	ap_cveval()
pointer	cv, data, sky
errchk	asifit

begin
	i = AP_AXIS(ap)
	low = AP_CEN(ap,i) + AP_LOW(ap,i)
	high = AP_CEN(ap,i) + AP_HIGH(ap,i)
	step = (high - low) / nsubaps
	cv = AP_CV(ap)
	do iy = 1, ny {
	    s = ap_cveval (cv, real (iy + ys - 1)) - c1 + 1
	    call ap_asifit (dbuf+(iy+ys-1-l1)*nc, nc, xs[iy]-c1+1,
		low+s, high+s, data, asi)
#	    data = dbuf + (iy + ys - 1 - l1) * nc + xs[iy] - c1 - 1
#	    if (asi != NULL)
#		call asifit (asi, Memr[data], nc-xs[iy]+c1)
	    do i = 1, nsubaps {
	        x1 = max (0.5, low + (i - 1) * step + s) + c1 - xs[iy]
	        x2 = min (nc + 0.49, low + i * step + s) + c1 - xs[iy]
	        if (x2 <= x1) {
		    spec[iy,i] = 0.
		    next
	        }
	        ix1 = nint (x1)
	        ix2 = nint (x2)

		# Compute end pixel weights.  Remember asi is offset by 1.
		call ap_edge (asi, x1+1, x2+1, wt1, wt2)

		# Sum pixels.
		sval = wt1 * Memr[data+ix1] + wt2 * Memr[data+ix2]
		do ix = ix1+1, ix2-1
		    sval = sval + Memr[data+ix]

		# Subtract sky if desired.
	        if (sbuf != NULL) {
		    sky = sbuf + (iy - 1) * nx - 1
		    skyval = wt1 * Memr[sky+ix1] + wt2 * Memr[sky+ix2]
	            do ix = ix1+1, ix2-1
		        skyval = skyval + Memr[sky+ix]
		    sval = sval - skyval
	        }

		# Save extracted pixel value.
		spec[iy,i] = sval
	    }
	}
end


# AP_EDGE -- Compute edge weights.

procedure ap_edge (asi, x1, x2, wt1, wt2)

pointer	asi			#I Image interpolator pointer
real	x1, x2			#I Aperture edges
real	wt1, wt2		#I Weights

int	ix1, ix2
real	a, b
real	asieval(), asigrl()

begin
	    # Edge pixel centers.
	    ix1 = nint (x1)
	    ix2 = nint (x2)

	    # Default weights are fractions of pixel.
	    if (ix1 == ix2) {
		wt1 = (x2 - x1)
		wt2 = 0
	    } else {
		wt1 = (ix1 - x1 + 0.5)
		wt2 = (x2 - ix2 + 0.5)
	    }

	    # If there is an interpolator compute fraction of integral.
	    # We require that data and integrals be positive.
	    if (asi != NULL) {
		if (asieval (asi, real(ix1)) > 0) {
		    b = asigrl (asi, ix1-0.5, ix1+0.5)
		    if (b > 0) {
			if (ix1 == ix2)
			    a = asigrl (asi, x1, x2)
			else
			    a = asigrl (asi, x1, ix1+0.5)
			if (a > 0 && a < b)
			    wt1 = a / b
		    }
		}
		if (ix1 != ix2 && asieval (asi, real(ix2)) > 0) {
		    b = asigrl (asi, ix2-0.5, ix2+0.5)
		    if (b > 0) {
			a = asigrl (asi, ix2-0.5, x2)
			if (a > 0 && a < b)
			    wt2 = a / b
		    }
		}
	    }
end


# AP_STRIP -- Simple, unweighted aperture strip.
# Interpolate so that the lower edge of the aperture is the first pixel.

procedure ap_strip (ap, aplow, aphigh, out, dbuf, nc, nl, c1, l1, sbuf, nx, ny,
	xs, ys)

pointer	ap			# Aperture structure
real	aplow, aphigh		# Aperture limits
pointer	out			# Output IMIO pointer
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
pointer	sbuf			# Sky values (NULL if none)
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Origin of sky array

int	i, na, iy, ix1, ix2, nasi
real	low, high, s, x, ap_cveval(), asieval()
pointer	obuf, cv, asi, data, sky, ptr, imps2r()

begin
	i = AP_AXIS(ap)
	low = aplow - c1 + 1
	high = aphigh - c1 + 1
	cv = AP_CV(ap)
	call asiinit (asi, II_LINEAR)

	na = IM_LEN(out,2)
	obuf = imps2r (out, 1, ny, 1, na)
	call aclrr (Memr[obuf], na * ny)

	do iy = 1, ny {
	    i = iy + ys - 1
	    s = ap_cveval (cv, real (i))
	    ix1 = max (1, nint (low + s) - 1)
	    ix2 = min (nc, nint (high + s) + 1)
	    nasi = ix2 - ix1 + 1
	    if (nasi < 3)
		next
	    data = dbuf + (i - l1) * nc + ix1 - 1
	    iferr (call asifit (asi, Memr[data], nasi))
		next
	    
	    x = low + s - ix1 + 1
	    ptr = obuf + iy - 1
	    if (sbuf == NULL) {
	        do i = 1, na {
		    if (x >= 1 && x <= nasi)
		        Memr[ptr] = asieval (asi, x)
		    x = x + 1.
		    ptr = ptr + ny
	        }
	    } else {
		sky = sbuf + (iy - 1) * nx + nint (low + s) - xs[iy] + c1 - 2
	        do i = 1, na {
		    if (x >= 1 && x <= nasi)
		        Memr[ptr] = asieval (asi, x) - Memr[sky+i]
		    x = x + 1.
		    ptr = ptr + ny
	        }
	    }
	}

	call asifree (asi)
end


# AP_PSTRIP -- Profile based strip.
# Interpolate the profile spectrum so that the lower aperture edge is the
# first pixel.

procedure ap_pstrip (ap, aplow, aphigh, out, gain, spec, profile, nx, ny,
	xs, ys)

pointer	ap			# Aperture structure
real	aplow, aphigh		# Aperture limits
pointer	out			# Output IMIO pointer
real	gain			# Gain
real	spec[ny]		# Spectrum
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Origin of profile array

int	na, ix, iy
real	low, high, s, x, ap_cveval(), asieval()
pointer	sp, cv, asi, data, impl2r()

begin
	call smark (sp)
	call salloc (data, nx, TY_REAL)

	ix = AP_AXIS(ap)
	low = aplow
	high = aphigh
	cv = AP_CV(ap)
	na = IM_LEN(out,2)
	call asiinit (asi, II_LINEAR)

	do iy = 1, ny {
	    s = spec[iy] / gain
	    do ix = 1, nx
		Memr[data+ix-1] = s * profile[iy,ix]
	    call asifit (asi, Memr[data], nx)
	    s = ap_cveval (cv, real (iy+ys-1)) - xs[iy] + 1
	    x = low + s
	    do ix = 1, na {
		profile[iy,ix] = asieval (asi, x)
		x = x + 1
	    }
	}

	do ix = 1, na
	    call amovr (profile[1,ix], Memr[impl2r(out,ix)], ny)

	call asifree (asi)
end


# AP_ASIFIT -- Return interpolation pointer and data pointer.
#
# The main reason for this routine is to shift the origin of the data by
# one pixel so that the interpolator may be called to evaluate across
# the extent of the first and last pixels.  This means the calling program
# will reference asi fit between 1.5 and N+1.5.  It also means the returned
# data pointer may start before the first point but will never be
# dereferenced outside of the data range.

procedure ap_asifit (dbuf, nc, xs, low, high, data, asi)

pointer	dbuf			#I Data buffer pointer
int	nc			#I Size of data buffer
int	xs			#I Start of aperture array (in dbuf coords)
real	low			#I Low aperture edge (in dbuf coords)
real	high			#I High aperture edge (in dbuf coords)
pointer	data			#O Data pointer
pointer	asi			#I ASI pointer

int	i, ix1, ix2, n
real	x1, x2
pointer	fit

begin
	# Check for in bounds data.
	x1 = max (0.5, low)
	x2 = min (nc + 0.49, high)
	if (x1 >= x2)
	    return

	# Set data pointer relative to the aperture start with an offset for
	# one indexing; i.e. pixel i is referenced as Memr[data+i].  The
	# aperture start may put this outside the data buffer but we expect
	# routines using the pointer to never index outside of the buffer.

	data = (dbuf + xs - 1) - 1

	# If not using an interpolator we are done.

	if (asi == NULL)
	    return

	# If the aperture, with one extra pixel on each end for integration
	# across the end pixel, is within the data buffer then fit an
	# interpolator directly.  Otherwise we need to use a temporary
	# padded buffer.  The origin of the fitted buffer is relative
	# to the data pointer.  Note that this means that evaluating the
	# fit requires the aperture start coordinates to be incremented
	# by 1.

	ix1 = 0
	ix2 = nint (x2) + 1 - (xs - 1)
	n = ix2 + ix1 + 1
	if (data + ix1 >= dbuf && data + ix2 <= dbuf + nc - 1) {
	    call asifit (asi, Memr[data+ix1], n)
	    return
	}

	# One or the other end point is out of bounds so to avoid potential
	# NAN and segmentation errors use an internal array to pad.

	call malloc (fit, n, TY_REAL)
	do i = 0, n-1 {
	    if (data + i < dbuf)
		Memr[fit+i] = Memr[dbuf]
	    else if (data + i > dbuf + nc - 1)
		Memr[fit+i] = Memr[dbuf+nc-1]
	    else
		Memr[fit+i] = Memr[data+i]
	}
	call asifit (asi, Memr[fit], n)
	call mfree (fit, TY_REAL)
end
