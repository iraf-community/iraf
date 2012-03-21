include	<imhdr.h>
include	<error.h>
include	<mach.h>
include	<smw.h>
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
real	flow1, fhigh1, pclip1, nkeep1

real	rval
bool	grdn, ggain, gsn
int	i, j, k, l, n, naps, npts
pointer	im, mw, nout, refim, shin, shout
pointer	sp, input, output, noutput, scale, zero, weight, str, logfile, sh, ns
pointer	sp1, d, id, nc, m, lflag, scales, zeros, wts

real	clgetr(), imgetr()
bool	clgetb(), rng_elementi()
int	clgeti(), clgwrd(), ctor()
int	imtopenp(), imtgetim(), open(), nowhite()
pointer	rng_open(), immap(), smw_openim(), impl2i(), impl2r()
errchk	open, immap, smw_openim, shdr_open, imgetr
errchk	scb_output, scb_combine, ic_combiner

include	"icombine.com"

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (noutput, SZ_FNAME, TY_CHAR)
	call salloc (scale, SZ_FNAME, TY_CHAR)
	call salloc (zero, SZ_FNAME, TY_CHAR)
	call salloc (weight, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)
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
	call clgstr ("scale", Memc[scale], SZ_FNAME)
	call clgstr ("zero", Memc[zero], SZ_FNAME)
	call clgstr ("weight", Memc[weight], SZ_FNAME)
	call clgstr ("gain", Memc[gain], SZ_FNAME)
	call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
	call clgstr ("snoise", Memc[snoise], SZ_FNAME)
	lthresh = clgetr ("lthreshold")
	hthresh = clgetr ("hthreshold")
	lsigma = clgetr ("lsigma")
	hsigma = clgetr ("hsigma")
	pclip1 = clgetr ("pclip")
	flow1 = clgetr ("nlow")
	fhigh1 = clgetr ("nhigh")
	nkeep1 = clgeti ("nkeep")
	grow = clgeti ("grow")
	mclip = clgetb ("mclip")
	sigscale = clgetr ("sigscale")

	i = nowhite (Memc[scale], Memc[scale], SZ_FNAME)
	i = nowhite (Memc[zero], Memc[zero], SZ_FNAME)
	i = nowhite (Memc[weight], Memc[weight], SZ_FNAME)

	# Check parameters, map INDEFs, and set threshold flag
	if (combine == SUM)
	    reject1 = NONE
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
	if (IS_INDEFI (nkeep1))
	    nkeep1 = 0
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
	gsn = false
	if (reject1 == CCDCLIP || reject1 == CRREJECT) {
	    i = 1
	    if (ctor (Memc[rdnoise], i, rval) == 0)
		grdn = true
	    i = 1
	    if (ctor (Memc[gain], i, rval) == 0)
		ggain = true
	    i = 1
	    if (ctor (Memc[snoise], i, rval) == 0)
		gsn = true
	}

	# Open the log file.
	logfd = NULL
	if (Memc[logfile] != EOS) {
	    iferr (logfd = open (Memc[logfile], APPEND, TEXT_FILE)) {
	        logfd = NULL
	        call erract (EA_WARN)
	    }
	}

	iferr (aps = rng_open (Memc[str], INDEF, INDEF, INDEF))
	    call error (1, "Error in aperture list")

	# Loop through input images.
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF) {
		call eprintf ("No output image\n")
		break
	    }
	    if (imtgetim (nlist, Memc[noutput], SZ_FNAME) == EOF)
		Memc[noutput] = EOS

	    # Get spectra to combine.
	    # Because the input images are unmapped we must get all the
	    # data we need for combining into the spectrum data structures.
	    # In particular any header keyword parameters that will be
	    # used.  We save the header values in unused elements of
	    # the spectrum data structure.

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

		do i = 1, SMW_NSPEC(mw) {
		    call shdr_open (im, mw, i, 1, INDEFI, SHDATA, shin)
		    if (Memc[scale] == '!')
			ST(shin) = imgetr (im, Memc[scale+1])
		    if (Memc[zero] == '!')
			HA(shin) = imgetr (im, Memc[zero+1])
		    if (Memc[weight] == '!')
			AM(shin) = imgetr (im, Memc[weight+1])
		    if (grdn)
			RA(shin) = imgetr (im, Memc[rdnoise])
		    if (ggain)
			DEC(shin) = imgetr (im, Memc[gain])
		    if (gsn)
			UT(shin) = imgetr (im, Memc[snoise])
		    if (!rng_elementi (aps, AP(shin)))
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
		MW(shin) = NULL
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
		im, mw, nout, refim)

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
		nkeep = nkeep1
		if (nkeep < 0)
		    nkeep = n + nkeep
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
	    call smw_close (mw)
	    call imunmap (im)
	    call imunmap (refim)
	    if (nout != NULL)
		call imunmap (nout)

	    # Find all the distinct SMW pointers and free them.
	    do j = 1, naps {
		do i = 1, NS(ns,j) {
		    mw = MW(SH(sh,j,i))
		    if (mw != NULL) {
			do k = 1, naps  {
			    do l = 1, NS(ns,k) {
				shin = SH(sh,k,l)
				if (MW(shin) == mw)
				    MW(shin) = NULL
			    }
			}
			call smw_close (mw)
		    }
		}
	    }
	    do j = 1, naps {
		do i = 1, NS(ns,j)
		    call shdr_close (SH(sh,j,i))
		call mfree (Memi[sh+j-1], TY_POINTER)
	    }
	    call mfree (sh, TY_POINTER)
	    call mfree (ns, TY_INT)
	}

	call rng_close (aps)
	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (nlist)

	call sfree (sp)
end


# SCB_REBIN - Rebin input spectra to output dispersion
# Use the SX array as mask.  If less than 1% of an input
# pixel contributes to an output pixel then flag it as missing data.

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
	    a = max (1, nint (min (b, c) + 0.01))
	    b = min (npts, nint (max (b, c) - 0.01))
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
	    call aclrr (Memr[SX(shin)], SN(shin))
	    j = a - 1
	    if (j > 0)
		call amovkr (1.0, Memr[SX(shin)], j)
	    j = SN(shin) - b
	    if (j > 0)
		call amovkr (1.0, Memr[SX(shin)+SN(shin)-j], j)
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

procedure scb_output (sh, ns, naps, output, noutput, im, mw, nout, refim)

pointer	sh			# spectra structures
int	ns			# number of spectra
int	naps			# number of apertures
char	output[SZ_FNAME]	# output spectrum name
char	noutput[SZ_FNAME]	# output number combined image name
pointer	im			# output IMIO pointer
pointer	mw			# output MWCS pointer
pointer	nout			# output number combined IMIO pointer
pointer	refim			# reference image for output image

int	i, ap, beam, dtype, nw, nmax, axis[2]
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	sp, key, coeff, sh1
pointer	immap(), mw_open(), smw_openim()
errchk	immap, smw_openim
data	axis/1,2/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	coeff = NULL

	# Create output image using the first input image as a reference
	refim = immap (IMNAME(SH(sh,1,1)), READ_ONLY, 0)
	im = immap (output, NEW_COPY, refim)

	# Use smw_openim to clean up old keywords(?).
	mw = smw_openim (im)
	call smw_close (mw)

	if (naps == 1)
	    IM_NDIM(im) = 1
	else
	    IM_NDIM(im) = 2
	call imaddi (im, "SMW_NDIM", IM_NDIM(im))
	IM_LEN(im,2) = naps
	if (IM_PIXTYPE(im) != TY_DOUBLE)
	    IM_PIXTYPE(im) = TY_REAL

	# Set new header.
	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axis, 2, "multispec",
	    "label=Wavelength units=Angstroms")
	call smw_open (mw, NULL, im)

	nmax = 0
	do i = 1, naps {
	    sh1 = SH(sh,i,1)
	    call smw_gwattrs (MW(sh1), APINDEX(sh1), 1, ap, beam, dtype,
		w1, dw, nw, z, aplow, aphigh, coeff)
	    call scb_default (SH(sh,i,1), NS(ns,i),
		dtype, w1, dw, nw, z, Memc[coeff])
	    call smw_swattrs (mw, i, 1, ap, beam, dtype,
		w1, dw, nw, z, aplow, aphigh, Memc[coeff])
	    call smw_sapid (mw, i, 1, TITLE(sh1))
	    nmax = max (nmax, nw)
	}

	IM_LEN(im,1) = nmax

	# Set MWCS header.
	call smw_saveim (mw, im)
	call smw_close (mw)
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
int	i, nwa, clgeti()
double	w2, aux, w1a, w2a, dwa, clgetd()
pointer	sh

begin
	if (clgetb ("first"))
	    return

	w1a = clgetd ("w1")
	w2a = clgetd ("w2")
	dwa = clgetd ("dw")
	nwa = clgeti ("nw")
	if (clgetb ("log"))
	    dtype = DCLOG
	else
	    dtype = DCLINEAR
	z = 0.
	coeff[1] = EOS

	# Dispersion type
	if (dtype == DCLINEAR) {
	    do i = 1, ns {
		if (DC(shdr[i]) == DCNO) {
		    dtype = DCNO
		    break
		}
	    }
	}

	w1 = w1a
	w2 = w2a
	dw = dwa
	nw = nwa

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

	# Adjust the values.
	if (IS_INDEFD (dwa))
	    dw = (w2 - w1) / (nw - 1)
	else if (IS_INDEFD (w2a))
	    w2 = w1 + (nw - 1) * dw
	else if (IS_INDEFD (w1a))
	    w1 = w2 - (nw - 1) * dw
	else {
	    nw = int ((w2 - w1) / dw + 0.5) + 1
	    w2 = w1 + (nw - 1) * dw
	}
end
