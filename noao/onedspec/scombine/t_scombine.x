include	<imhdr.h>
include	<error.h>
include	<mach.h>
include	"../shdr.h"
include "icombine.h"


# T_SCOMBINE - Combine spectra
# The input spectra are combined by medianing, averaging or summing
# with optional rejection, scaling and weighting.  The input may be
# grouped by aperture or by image.  The combining algorithms are
# similar to those in IMCOMBINE.

procedure t_scombine()

int	ilist			# list of input images
int	olist			# list of output images
pointer	nlist			# image name for number combined
pointer	aps			# aperture ranges
int	group			# grouping option

int	reject1
real	flow1, fhigh1, pclip1

real	rval
bool	grdn, ggain
int	i, j, n, naps, npts
pointer	im, mw, nout, shin, shout
pointer	sp, input, output, noutput, str, logfile, sh, ns
pointer	sp1, d, id, nc, m, lflag, scales, zeros, wts

real	clgetr(), imgetr()
bool	clgetb(), is_in_range()
int	clgeti(), clgwrd(), decode_ranges(), ctor()
int	imtopenp(), imtgetim(), open()
pointer	immap(), smw_openim(), impl2i(), impl2r()
errchk	open, immap, smw_openim, shdr_open, imgetr
errchk	scb_output, scb_combine

include	"icombine.com"

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (noutput, SZ_FNAME, TY_CHAR)
	call salloc (aps, 300, TY_INT)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	# Get parameters
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	nlist = imtopenp ("noutput")
	call clgstr ("apertures", Memc[str], SZ_LINE)
	group = clgwrd ("group", Memc[input], SZ_FNAME, GROUP)

	# IMCOMBINE parameters
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	combine = clgwrd ("combine", Memc[input], SZ_FNAME, COMBINE)
	reject1 = clgwrd ("reject", Memc[input], SZ_FNAME, REJECT)
	blank = clgetr ("blank")
	call clgstr ("gain", Memc[gain], SZ_FNAME)
	call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
	lthresh = clgetr ("lthreshold")
	hthresh = clgetr ("hthreshold")
	lsigma = clgetr ("lsigma")
	hsigma = clgetr ("hsigma")
	pclip1 = clgetr ("pclip")
	flow1 = clgetr ("nlow")
	fhigh1 = clgetr ("nhigh")
	grow = clgeti ("grow")
	mclip = clgetb ("mclip")
	sigscale = clgetr ("sigscale")

	# Check parameters, map INDEFs, and set threshold flag
	if (pclip1 == 0. && reject1 == PCLIP)
	    call error (1, "Pclip parameter may not be zero")
	if (IS_INDEFR (blank))
	    blank = 0.
	if (IS_INDEFR (lsigma))
	    lsigma = MAX_REAL
	if (IS_INDEFR (hsigma))
	    hsigma = MAX_REAL
	if (IS_INDEFR (pclip1))
	    pclip1 = -0.5
	if (IS_INDEFR (flow1))
	    flow1 = 0
	if (IS_INDEFR (fhigh))
	    fhigh = 0
	if (IS_INDEFI (grow))
	    grow = 0
	if (IS_INDEF (sigscale))
	    sigscale = 0.

	if (IS_INDEF(lthresh) && IS_INDEF(hthresh))
	    dothresh = false
	else {
	    dothresh = true
	    if (IS_INDEF(lthresh))
		lthresh = -MAX_REAL
	    if (IS_INDEF(hthresh))
		hthresh = MAX_REAL
	}

	# Get read noise and gain?
	grdn = false
	ggain = false
	if (reject1 == CCDCLIP || reject1 == CRREJECT) {
	    i = 1
	    if (ctor (Memc[rdnoise], i, rval) == 0)
		grdn = true
	    i = 1
	    if (ctor (Memc[gain], i, rval) == 0)
		ggain = true
	}

	# Open the log file.
	logfd = NULL
	if (Memc[logfile] != EOS) {
	    iferr (logfd = open (Memc[logfile], APPEND, TEXT_FILE)) {
	        logfd = NULL
	        call erract (EA_WARN)
	    }
	}

	if (decode_ranges (Memc[str], Memi[aps], 100, i) == ERR)
	    call error (1, "Error in aperture list")

	# Loop through input images.
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF) {
		call eprintf ("No output image\n")
		break
	    }
	    if (imtgetim (nlist, Memc[noutput], SZ_FNAME) == EOF)
		Memc[noutput] = EOS

	    # Get spectra to combine
	    naps = 0
	    repeat {
		iferr (im = immap (Memc[input], READ_ONLY, 0)) {
		    if (group == GRP_IMAGES) {
			call erract (EA_WARN)
			next
		    } else {
			call erract (EA_ERROR)
		    }
		}
		mw = smw_openim (im)
		shin = NULL

		do i = 1, IM_LEN(im,2) {
		    call shdr_open (im, mw, i, 1, INDEFI, SHDATA, shin)
		    if (grdn)
			RA(shin) = imgetr (im, Memc[rdnoise])
		    if (ggain)
			DEC(shin) = imgetr (im, Memc[gain])
		    if (!is_in_range (Memi[aps], AP(shin)))
			next
		    if (group == GRP_APERTURES) {
			for (j=1; j<=naps; j=j+1)
			    if (AP(shin) == AP(SH(sh,j,1)))
				break
			n = 10
		    } else {
			j = 1
			n = 1
		    }

		    if (naps == 0) {
			call calloc (sh, n, TY_POINTER)
			call calloc (ns, n, TY_INT)
		    } else if (j > naps && mod (naps, n) == 0) {
			call realloc (sh, naps+n, TY_POINTER)
			call realloc (ns, naps+n, TY_INT)
			call aclri (Memi[sh+naps], n)
			call aclri (Memi[ns+naps], n)
		    }
		    if (j > naps)
			naps = naps + 1
		    n = NS(ns,j)
		    if (n == 0)
			call malloc (Memi[sh+j-1], 10, TY_POINTER)
		    else if (mod (n, 10) == 0)
			call realloc (Memi[sh+j-1], n+10, TY_POINTER)

		    n = n + 1
		    SH(sh,j,n) = NULL
		    NS(ns,j) = n
		    call shdr_copy (shin, SH(sh,j,n), NO)
		}

		call imunmap (IM(shin))
		call shdr_close (shin)

		if (group == GRP_IMAGES)
		    break
	    } until (imtgetim (ilist, Memc[input], SZ_FNAME) == EOF)

	    if (naps < 1) {
		call eprintf ("No input spectra to combine\n")
		next
	    }

	    # Set the output and combine the spectra.
	    call scb_output (sh, ns, naps, Memc[output], Memc[noutput],
		im, mw, nout)

	    do j = 1, naps {
		call shdr_open (im, mw, j, 1, INDEFI, SHHDR, shout)
		npts = SN(shout)
		n = NS(ns,j)

		# Allocate additional memory
		call smark (sp1)
		call salloc (d, n, TY_POINTER)
		call salloc (id, n, TY_POINTER)
		call salloc (nc, npts, TY_INT)
		call salloc (m, n, TY_POINTER)
		call salloc (lflag, n, TY_INT)
		call salloc (scales, n, TY_REAL)
		call salloc (zeros, n, TY_REAL)
		call salloc (wts, n, TY_REAL)
		call calloc (SX(shout), npts, TY_REAL)
		call calloc (SY(shout), npts, TY_REAL)
		call amovki (D_ALL, Memi[lflag], n)

		# Convert the pclip parameter to a number of pixels rather than
		# a fraction.  This number stays constant even if pixels are
		# rejected.  The number of low and high pixel rejected, however,
		# are converted to a fraction of the valid pixels.

		reject = reject1
		if (reject == PCLIP) {
		    pclip = pclip1
		    i = (n - 1) / 2.
		    if (abs (pclip) < 1.)
			pclip = pclip * i
		    if (pclip < 0.)
			pclip = min (-1, max (-i, int (pclip)))
		    else
			pclip = max (1, min (i, int (pclip)))
		}
		if (reject == MINMAX) {
		    flow = flow1
		    fhigh = fhigh1
		    if (flow >= 1)
			flow = flow / n
		    if (fhigh >= 1)
			fhigh = fhigh / n
		    i = flow * n + fhigh * n
		    if (i == 0)
			reject = NONE
		    else if (i >= n) {
			call eprintf ("Bad minmax rejection parameters\n")
			call eprintf ("Using no rejection\n")
			reject = NONE
		    }
		}

		# Combine spectra
		call ic_combiner (SH(sh,j,1), shout, Memi[d], Memi[id],
		    Memi[nc], Memi[m], Memi[lflag], Memr[scales], Memr[zeros],
		    Memr[wts], n, npts)

		# Write the results
		call amovr (Memr[SY(shout)], Memr[impl2r(im,j)], npts)
		if (nout != NULL)
		    call amovi (Memi[nc], Memi[impl2i(nout,j)], npts)
		call sfree (sp1)
	    }

	    # Finish up
	    call shdr_close (shout)
	    call mw_close (mw)
	    call imunmap (im)
	    if (nout != NULL)
		call imunmap (nout)

	    do j = 1, naps {
		mw = NULL
		do i = 1, NS(ns,j) {
		    shin = SH(sh,j,i)
		    if (MW(shin) != mw) {
			mw = MW(shin)
			call mw_close (MW(shin))
		    }
		    call shdr_close (shin)
		}
		call mfree (Memi[sh+j-1], TY_POINTER)
	    }
	    call mfree (sh, TY_POINTER)
	    call mfree (ns, TY_INT)
	}

	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (nlist)

	call sfree (sp)
end


# SCB_REBIN - Rebin input spectra to output dispersion
# Use the SX array ask mask

procedure scb_rebin (sh, shout, lflag, ns, npts)

pointer	sh[ns]			# Input spectra structures
pointer	shout			# Output spectrum structure
int	lflag[ns]		# Empty mask flags
int	ns			# Number of spectra
int	npts			# NUmber of output points

int	i, j
real	a, b, c
pointer	shin
double	shdr_wl(), shdr_lw()

include	"icombine.com"
	
begin
	# Rebin to common dispersion
	# Determine overlap with output and set mask arrays

	do i = 1, ns {
	    shin = sh[i]
	    c = shdr_wl (shout, shdr_lw (shin, double(0.5)))
	    b = shdr_wl (shout, shdr_lw (shin, double(SN(shin)+0.5)))
	    a = max (1, nint (min (b, c) + 0.99))
	    b = min (npts, nint (max (b, c) - 0.99))
	    j = b - a + 1
	    if (j < 1) {
		lflag[i] = D_NONE
		next
	    }
	    else if (j < npts)
		lflag[i] = D_MIX
	    else
		lflag[i] = D_ALL

	    call shdr_rebin (shin, shout)
	    call aclri (Memi[SX(shin)], SN(shin))
	    j = a - 1
	    if (j > 0)
		call amovki (1, Memi[SX(shin)], j)
	    j = SN(shin) - b
	    if (j > 0)
		call amovki (1, Memi[SX(shin)+SN(shin)-j], j)
	}

	dflag = lflag[1]
	do i = 2, ns {
	    if (dflag != lflag[i]) {
		dflag = D_MIX
		break
	    }
	}
end


# SCB_OUTPUT - Set the output spectrum

procedure scb_output (sh, ns, naps, output, noutput, im, mw, nout)

pointer	sh			# spectra structures
int	ns			# number of spectra
int	naps			# number of apertures
char	output[SZ_FNAME]	# output spectrum name
char	noutput[SZ_FNAME]	# output number combined image name
pointer	im			# output IMIO pointer
pointer	mw			# output MWCS pointer
pointer	nout			# output number combined IMIO pointer

int	i, ap, beam, dtype, nw, nmax, axis[2]
double	w1, dw, z, aplow, aphigh
pointer	sp, key, coeff, sh1, junk
pointer	immap(), mw_open(), smw_openim()
bool	strne()
errchk	immap, smw_openim
data	axis/1,2/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	coeff = NULL

	# Create output image using the first input image as a reference
	nout = immap (SPECTRUM(SH(sh,1,1)), READ_ONLY, 0)
	im = immap (output, NEW_COPY, nout)
	call imunmap (nout)

	# Set new header.
	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axis, 2, "multispec",
	    "label=Wavelength units=Angstroms")

	nmax = 0
	do i = 1, naps {
	    sh1 = SH(sh,i,1)
	    call shdr_gwattrs (MW(sh1), PINDEX1(sh1), ap, beam, dtype,
		w1, dw, nw, z, aplow, aphigh, coeff)
	    call scb_default (SH(sh,i,1), NS(ns,i),
		dtype, w1, dw, nw, z, Memc[coeff])
	    call shdr_swattrs (mw, i, ap, beam, dtype,
		w1, dw, nw, z, aplow, aphigh, Memc[coeff])
	    if (strne (IM_TITLE(im), TITLE(sh1))) {
		call sprintf (Memc[key], SZ_FNAME, "APID%d")
		    call pargi (i)
		call imastr (im, Memc[key], TITLE(sh1))
	    }
	    nmax = max (nmax, nw)
	}

	if (naps == 1)
	    IM_NDIM(im) = 1
	else
	    IM_NDIM(im) = 2
	IM_LEN(im,1) = nmax
	IM_LEN(im,2) = naps
	IM_PIXTYPE(im) = TY_REAL

	# Set header.  Use smw_openim to clean up old keywords.
	junk = smw_openim (im)
	call mw_close (junk)
	call smw_saveim (mw, im)
	call mw_close (mw)
	mw = smw_openim (im)

	# Create number combined image
	if (noutput[1] != EOS) {
	    nout = immap (noutput, NEW_COPY, im)
	    IM_PIXTYPE(nout) = TY_INT
	    call sprintf (IM_TITLE(nout), SZ_LINE, "Number combined for %s")
		call pargstr (output)
	}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# SCB_DEFAULT - Set default values for the starting wavelength, ending
# wavelength, wavelength increment and spectrum length for the output
# spectrum.

procedure scb_default (shdr, ns, dtype, w1, dw, nw, z, coeff)

pointer	shdr[ARB]		# spectra structures
int	ns			# number of spectra
int	dtype			# dispersion type
double	w1			# starting wavelength
double	dw			# wavelength increment
int	nw			# spectrum length
double	z			# redshift
char	coeff[ARB]		# nonlinear coefficient array

bool	clgetb()
int	i, clgeti()
double	w2, aux, clgetd()
pointer	sh

begin
	if (clgetb ("first"))
	    return

	w1 = clgetd ("w1")
	w2 = clgetd ("w2")
	dw = clgetd ("dw")
	nw = clgeti ("nw")
	if (clgetb ("log"))
	    dtype = DCLOG
	else
	    dtype = DCLINEAR
	z = 0.
	coeff[1] = EOS

	# Starting wavelength
	if (IS_INDEFD (w1)) {
	    if (IS_INDEFD (dw) || dw > 0.) {
		w1 = MAX_REAL
		do i = 1, ns {
		    sh = shdr[i]
		    if (WP(sh) > 0.)
			aux = W0(sh)
		    else
			aux = W1(sh)
		    if (aux < w1)
			w1 = aux
		}
	    } else {
		w1 = -MAX_REAL
		do i = 1, ns {
		    sh = shdr[i]
		    if (WP(sh) > 0.)
			aux = W1(sh)
		    else
			aux = W0(sh)
		    if (aux > w1)
			w1 = aux
		}
	    }
	}

	# Ending wavelength
	if (IS_INDEFD (w2)) {
	    if (IS_INDEFD (dw) || dw > 0.) {
		w2 = -MAX_REAL
		do i = 1, ns {
		    sh = shdr[i]
		    if (WP(sh) > 0.)
			aux = W1(sh)
		    else
			aux = W0(sh)
		    if (aux > w2)
			w2 = aux
		}
	    } else {
		w2 = MAX_REAL
		do i = 1, ns {
		    sh = shdr[i]
		    if (WP(sh) > 0.)
			aux = W0(sh)
		    else
			aux = W1(sh)
		    if (aux < w2)
			w2 = aux
		}
	    }
	}

	# Wavelength increment
	if (IS_INDEFD (dw)) {
	    dw = MAX_REAL
	    do i = 1, ns {
		aux = abs (WP(shdr[i]))
		if (aux < dw)
		    dw = aux
	    }
	}
	if ((w2 - w1) / dw < 0.)
	    dw = -dw

	# Spectrum length
	if (IS_INDEFI (nw))
	    nw = int ((w2 - w1) / dw + 0.5) + 1

	# Force the ending wavelength
	w2 = w1 + (nw - 1) * dw

	# Reset to log if necessary
	if (dtype == DCLOG) {
	    w1 = log10 (w1)
	    dw = log10 (w2/w1)/(nw-1)
	}
end
