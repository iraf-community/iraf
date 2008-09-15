include	<imhdr.h>
include	<error.h>
include	<mach.h>
include	<mwset.h>
include	<smw.h>
include	"src/icombine.h"

# Grouping options
define	GROUP	"|all|images|apertures|"
define	GRP_ALL		1
define	GRP_IMAGES	2
define	GRP_APERTURES	3

# Mask formats
define	MASKFORMATS	"|bpmpixel|bpmspectrum|"
define	BPMPIX		1
define	BPMSPEC		2

# Spectrum data structure
define	NS	Memi[$1+$2-1]			# Number of spec of given ap
define	SH	Memi[Memi[$1+$2-1]+$3-1]	# Spectrum header structure


# T_ODCOMBINE - Combine spectra matched in world coordinates.
# 
# The input spectra are combined by medianing, averaging or summing with
# optional rejection, scaling and weighting.  The combining algorithms and
# other features are the same as those in IMCOMBINE.
# 
# The main difference with IMCOMBINE is that the spectra are first resampled
# to a common grid of pixels in dispersion.  To do this each spectrum
# is resampled to a temporary file which are then combined and deleted.
# When bad pixels are used they are also resampled in the same way.
# 
# Since there can be multiple spectra per file (each with different
# aperture numbers) there are three ways to group the spectra for combining.
# One is by image where all spectra in the file are combined.  The second
# is by aperture where the same aperture across multple files are combine.
# The third is to combine all spectra independent of aperture or file.
# 
# The structure of the program is to first internally collect all the
# spectra from each input file.  When combining by image this is done file
# by file otherwise all the files are collected together.  The reason for
# this is to avoid opening, search, reading, and closing the same file
# for each aperture Then the spectra for one output are rebinned to a
# common dispersion and written to temporary files.  The same is done for
# bad pixel masks if used.  The spectra in the files are then combined.
# Finally the temporary files are deleted.  The rebinning and combine are
# repeated for each output spectrum.

procedure t_odcombine()

pointer	aps			# aperture ranges
int	group			# grouping option

int	mformat, mtype, mvalue
int	i, j, index, naps
pointer	im, mw, refim, shout
pointer	sp, input, output, headers, bmask, rmask, nrmask, emask, sigma, logfile
pointer	tmp, str, s, b, ns
int	ilist1, ilist2, ilist, olist, hlist, blist, rlist, slist, nrlist, elist

bool	clgetb()
int	clgeti(), clgwrd()
int	imtopen(), imtopenp(), imtgetim(), imtlen()
real	clgetr()
pointer	rng_open()
errchk	shdr_open, odc_gspec, odc_rebin, odc_output, odc_combine

include	"src/icombine.com"

begin
	# Allocate stack memory.  Note some of the variables are declared in
	# the icombine common block but still need to be allocated here.

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (headers, SZ_FNAME, TY_CHAR)
	call salloc (bmask, SZ_FNAME, TY_CHAR)
	call salloc (rmask, SZ_FNAME, TY_CHAR)
	call salloc (nrmask, SZ_FNAME, TY_CHAR)
	call salloc (emask, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (ictask, SZ_FNAME, TY_CHAR)
	call salloc (expkeyword, SZ_FNAME, TY_CHAR)
	call salloc (statsec, SZ_FNAME, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)

	# Set the IMCOMBINE parameters.

	call strcpy ("ODCOMBINE", Memc[ictask], SZ_FNAME)
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	hlist = imtopenp ("headers")
	blist = imtopenp ("bpmasks")
	rlist = imtopenp ("rejmasks")
	nrlist = imtopenp ("nrejmasks")
	elist = imtopenp ("expmasks")
	slist = imtopenp ("sigmas")
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)

	project = false
	combine = clgwrd ("combine", Memc[str], SZ_LINE, COMBINE)
	reject = clgwrd ("reject", Memc[str], SZ_LINE, REJECT)
	blank = clgetr ("blank")
	call clgstr ("expname", Memc[expkeyword], SZ_FNAME)
	call clgstr ("statsec", Memc[statsec], SZ_FNAME)
	call clgstr ("gain", Memc[gain], SZ_FNAME)
	call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
	call clgstr ("snoise", Memc[snoise], SZ_FNAME)
	lthresh = clgetr ("lthreshold")
	hthresh = clgetr ("hthreshold")
	lsigma = clgetr ("lsigma")
	hsigma = clgetr ("hsigma")
	pclip = clgetr ("pclip")
	flow = clgetr ("nlow")
	fhigh = clgetr ("nhigh")
	nkeep = clgeti ("nkeep")
	grow = clgetr ("grow")
	mclip = clgetb ("mclip")
	sigscale = clgetr ("sigscale")

	# Check parameters, map INDEFs, and set threshold flag

	if (pclip == 0. && reject == PCLIP)
	    call error (1, "Pclip parameter may not be zero")
	if (IS_INDEFR (blank))
	    blank = 0.
	if (IS_INDEFR (lsigma))
	    lsigma = MAX_REAL
	if (IS_INDEFR (hsigma))
	    hsigma = MAX_REAL
	if (IS_INDEFR (pclip))
	    pclip = -0.5
	if (IS_INDEFR (flow))
	    flow = 0
	if (IS_INDEFR (fhigh))
	    fhigh = 0
	if (IS_INDEFR (grow))
	    grow = 0.
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

	# Get ODCOMBINE specific parameters.

	call clgstr ("apertures", Memc[str], SZ_LINE)
	group = clgwrd ("group", Memc[input], SZ_FNAME, GROUP)

	# Expand aperture list.
	iferr (aps = rng_open (Memc[str], INDEF, INDEF, INDEF))
	    call error (1, "Error in aperture list")

	# We need to know about the mask in order to resample them.
	# This does not support specifying a mask by name or keyword.

	mformat = clgwrd ("smaskformat", Memc[str], SZ_LINE, MASKFORMATS)
	mtype = clgwrd ("smasktype", Memc[str], SZ_LINE, MASKTYPES)
	if (mtype == 0)
	    call error (1, "Unsupported masktype")
	mvalue = clgeti ("smaskvalue")
	if (mtype == M_BADBITS && mvalue == 0) 
	    mtype = M_NONE
	if (mtype == M_NONE)
	    call clpstr ("masktype", "none")
	else
	    call clpstr ("masktype", "goodvalue")
	call clputi ("maskvalue", 0)

	# Check lists.
	i = imtlen (ilist)
	if (i == 0)
	    call error (1, "No input images to combine")
	switch (group) {
	case GRP_ALL, GRP_APERTURES:
	    if (imtlen (olist) != 1)
		call error (1, "Wrong number of output images")
	    if (imtlen (hlist) > 1)
		call error (1, "Wrong number of header files")
	    if (imtlen (blist) > 1)
		call error (1, "Wrong number of bad pixel masks")
	    if (imtlen (rlist) > 1)
		call error (1, "Wrong number of rejection masks")
	    if (imtlen (nrlist) > 1)
		call error (1, "Wrong number of number rejected masks")
	    if (imtlen (elist) > 1)
		call error (1, "Wrong number of exposure masks")
	    if (imtlen (slist) > 1)
		call error (1, "Wrong number of sigma images")
	case GRP_IMAGES:
	    if (imtlen (olist) != i)
		call error (1, "Wrong number of output images")
	    if (imtlen (hlist) > 0 && imtlen (hlist) != i)
		call error (1, "Wrong number of header files")
	    if (imtlen (blist) > 0 && imtlen (blist) != i)
		call error (1, "Wrong number of bad pixel masks")
	    if (imtlen (rlist) > 0 && imtlen (rlist) != i)
		call error (1, "Wrong number of rejection masks")
	    if (imtlen (nrlist) > 0 && imtlen (nrlist) != i)
		call error (1, "Wrong number of number rejected masks")
	    if (imtlen (elist) > 0 && imtlen (elist) != i)
		call error (1, "Wrong number of exposure masks")
	    if (imtlen (slist) > 1 && imtlen (slist) != i)
		call error (1, "Wrong number of sigma images")
	}

	# Set temporary output rootname.
	call mktemp ("tmp", Memc[tmp], SZ_FNAME)

	# Loop through input images.
	index = 0
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {

	    # Get all requested apertures from an image.  When not grouping
	    # by image go through all images and exhaust the input list.

	    naps = 0
	    repeat {
		iferr (call odc_gspec (Memc[input], aps, group, mtype, mformat,
		    s, b, ns, naps)) {
		    if (group == GRP_IMAGES) {
			call erract (EA_WARN)
			next
		    } else {
			call erract (EA_ERROR)
		    }
		}
		if (group == GRP_IMAGES)
		    break
	    } until (imtgetim (ilist, Memc[input], SZ_FNAME) == EOF)

	    if (naps < 1) {
		call eprintf ("No input spectra to combine\n")
		next
	    }

	    # Create each output spectrum.  This involves rebinning to
	    # temporary files, combining, and cleaning up.  The files are
	    # deleted in the odc_combine routine.

	    do i = 1, naps {

		# Set the output dispersion in a temporary template image.
		call odc_output (SH(s,i,1), NS(ns,i), Memc[tmp], im, mw, refim)
		call shdr_open (im, mw, i, 1, INDEFI, SHDATA, shout)

		# Rebin the spectra.
		call odc_rebin (im, shout, SH(s,i,1), SH(b,i,1), NS(ns,i),
		    mformat, mtype, mvalue, Memc[tmp])

		# Close and delete the template image.
		call shdr_close (shout)
		call smw_close (mw)
		call imunmap (im)
		call imunmap (refim)
		iferr (call imdelete (Memc[tmp]))
		    ;

		# Set lists to be combined.
		call sprintf (Memc[str], SZ_LINE, "%s.*\\[^x]")
		    call pargstr (Memc[tmp])
		ilist1 = imtopen (Memc[str])
		if (mtype != NONE) {
		    call sprintf (Memc[str], SZ_LINE, "%sbpm.*\\[^x]")
			call pargstr (Memc[tmp])
		    ilist2 = imtopen (Memc[str])
		} else
		    ilist2 = imtopen ("")

		# Set output names.
		switch (group) {
		case GRP_ALL:
		    index = 1
		    j = INDEFI
		case GRP_IMAGES:
		    index = index + 1
		    j = INDEFI
		case GRP_APERTURES:
		    index = 1
		    j = AP(SH(s,i,1))
		}
		call odc_imtgetim (olist, index, j, Memc[output], SZ_FNAME)
		call odc_imtgetim (hlist, index, j, Memc[headers], SZ_FNAME)
		call odc_imtgetim (blist, index, j, Memc[bmask], SZ_FNAME)
		call odc_imtgetim (rlist, index, j, Memc[rmask], SZ_FNAME)
		call odc_imtgetim (nrlist, index, j, Memc[nrmask], SZ_FNAME)
		call odc_imtgetim (elist, index, j, Memc[emask], SZ_FNAME)
		call odc_imtgetim (slist, index, j, Memc[sigma], SZ_FNAME)

		# Combine and delete the lists.
		iferr (call odc_combine (ilist1, ilist2, Memc[output],
		    Memc[headers], Memc[bmask], Memc[rmask], Memc[nrmask],
		    Memc[emask], Memc[sigma], Memc[logfile], YES))
		    call erract (EA_WARN)

		call imtclose (ilist1)
		call imtclose (ilist2)
	    }

	    # Free all the spectrum data structures.
	    call odc_fspec (s, b, ns, naps)
	}

	# Finish up.
	call rng_close (aps)
	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (hlist)
	call imtclose (blist)
	call imtclose (rlist)
	call imtclose (nrlist)
	call imtclose (elist)
	call imtclose (slist)
	call sfree (sp)
end


# ODC_GSPEC -- Get spectra from an input image.
#
# This allocates and sets arrays of spectrum structures.  There is an array
# for each "output" aperture and the number of elements is given by another
# array (ns).  The number of output apertures is given by naps.  Note that each
# call to this accumulates new spectra.

procedure odc_gspec (input, aps, group, mtype, mformat, s, b, ns, naps)

char	input[ARB]		#I Input spectrum file
pointer	aps			#I Apertures to select
int	group			#I Grouping for combining
int	mtype			#I Mask type
int	mformat			#I Mask format
pointer	s			#U Spectra data structure
pointer	b			#U Spectra data structure for pixel masks
int	ns			#U Number of spectra per group
int	naps			#U Number of output apertures

int	i, j, k, n
pointer	im, mw, sh, sh1, bpm, err

bool	rng_elementi()
pointer	immap(), smw_openim()
errchk	immap, smw_openim, shdr_open

begin
	# Map the input spectrum file. Check format.
	im = immap (input, READ_ONLY, 0)
	mw = smw_openim (im)
	if (SMW_FORMAT(mw) != SMW_ES && SMW_FORMAT(mw) != SMW_MS) {
	    call smw_close (mw)
	    call imunmap (im)
	    call salloc (err, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[err], SZ_LINE,
	       "Unsupported spectral format (%s)")
	       call pargstr (input)
	    call error (1, Memc[err])
	}
	sh = NULL

	# Get the associated mask if requested.  It is not an error if there
	# is mask.

	if (mtype == M_NONE)
	    bpm = NULL
	else {
	    switch (mformat) {
	    case BPMPIX, BPMSPEC:
		call malloc (bpm, SZ_FNAME, TY_CHAR)
		iferr (call imgstr (im, "BPM", Memc[bpm], SZ_FNAME))
		    call mfree (bpm, TY_CHAR)
	    default:
	        bpm = NULL
	    }
	}

	# Select the requested apertures and group by output aperture.
	do i = 1, SMW_NSPEC(mw) {
	    call shdr_open (im, mw, i, 1, INDEFI, SHDATA, sh)
	    if (!rng_elementi (aps, AP(sh)))
		next

	    if (group == GRP_APERTURES) {
		for (j=1; j<=naps; j=j+1)
		    if (AP(sh) == AP(SH(s,j,1)))
			break
		n = 10
	    } else {
		j = 1
		n = 1
	    }

	    if (naps == 0) {
		call calloc (s, n, TY_POINTER)
		call calloc (b, n, TY_POINTER)
		call calloc (ns, n, TY_INT)
	    } else if (j > naps && mod (naps, n) == 0) {
		call realloc (s, naps+n, TY_POINTER)
		call realloc (b, naps+n, TY_POINTER)
		call realloc (ns, naps+n, TY_INT)
		call aclri (Memi[s+naps], n)
		call aclri (Memi[b+naps], n)
		call aclri (Memi[ns+naps], n)
	    }
	    if (j > naps)
		naps = naps + 1
	    n = NS(ns,j)
	    if (n == 0) {
		call malloc (Memi[s+j-1], 10, TY_POINTER)
		call malloc (Memi[b+j-1], 10, TY_POINTER)
	    } else if (mod (n, 10) == 0) {
		call realloc (Memi[s+j-1], n+10, TY_POINTER)
		call realloc (Memi[b+j-1], n+10, TY_POINTER)
	    }

	    n = n + 1
	    SH(s,j,n) = NULL
	    SH(b,j,n) = NULL
	    call shdr_copy (sh, SH(s,j,n), NO)
	    NS(ns,j) = n
	}

	call imunmap (IM(sh))
	MW(sh) = NULL
	call shdr_close (sh)

	# Get BPMs if defined.
	if (bpm != NULL) {
	    im = immap (Memc[bpm], READ_ONLY, 0)
	    mw = smw_openim (im)
	    sh = NULL

	    switch (mformat) {
	    case BPMPIX:
		do j = 1, naps {
		    n = NS(ns,j)
		    sh1 = SH(s,j,n)
		    if (sh1 == NULL)
			next
		    k = LINDEX(sh1,1)
		    call shdr_open (im, mw, k, 1, INDEFI, SHDATA, sh)
		    if (LINDEX(sh,1) != k)
			next
		    call shdr_copy (sh1, SH(b,j,n), YES)
		    sh1 = SH(b,j,n)
		    call strcpy (IMNAME(sh), IMNAME(sh1), LEN_SHDRS)
		    call strcpy (IMSEC(sh), IMSEC(sh1), LEN_SHDRS)
		    call strcpy (TITLE(sh), TITLE(sh1), LEN_SHDRS)
		    call amovi (LINDEX(sh,1), LINDEX(sh1,1), 2)
		    call amovi (PINDEX(sh,1), PINDEX(sh1,1), 2)
		    APINDEX(sh) = APINDEX(sh1)
		    call amovr (Memr[SY(sh)], Memr[SY(sh1)],
			min (SN(sh), SN(sh1)))
		}
		call smw_close (mw)
	    case BPMSPEC:
		do j = 1, naps {
		    n = NS(ns,j)
		    sh1 = SH(s,j,n)
		    if (sh1 == NULL)
			next
		    k = AP(sh1)
		    call shdr_open (im, mw, 1, 1, k, SHDATA, sh)
		    if (AP(sh) != k)
			next
		    call shdr_copy (sh, SH(b,j,n), NO)
		}
	    }

	    call imunmap (IM(sh))
	    MW(sh) = NULL
	    call shdr_close (sh)
	    call mfree (bpm, TY_CHAR)
	}
end



# ODC_FSPEC -- Free spectrum data structures.

procedure odc_fspec (s, b, ns, naps)

pointer	s		#U Spectrum data structures
pointer	b		#U BPM data structures
pointer	ns		#U Number of spectra per output aperture
int	naps		#I Number of output apertures

int	i, j, k, l
pointer	sh, mw

begin
	# Find all the distinct SMW pointers and free them.
	# Then free all the spectrum data pointers.

	do j = 1, naps {
	    do i = 1, NS(ns,j) {
		sh = SH(s,j,i)
		if (sh == NULL)
		    next
		mw = MW(sh)
		if (mw != NULL) {
		    do k = 1, naps  {
			do l = 1, NS(ns,k) {
			    sh = SH(s,k,l)
			    if (sh == NULL)
				next
			    if (MW(sh) == mw)
				MW(sh) = NULL
			}
		    }
		    call smw_close (mw)
		}
	    }
	}
	do j = 1, naps {
	    do i = 1, NS(ns,j) {
		sh = SH(s,j,i)
		if (sh == NULL)
		    next
		call shdr_close (sh)
	    }
	    call mfree (Memi[s+j-1], TY_POINTER)
	}
	call mfree (s, TY_POINTER)

	do j = 1, naps {
	    do i = 1, NS(ns,j) {
		sh = SH(b,j,i)
		if (sh == NULL)
		    next
		mw = MW(sh)
		if (mw != NULL) {
		    do k = 1, naps  {
			do l = 1, NS(ns,k) {
			    sh = SH(b,k,l)
			    if (sh == NULL)
				next
			    if (MW(sh) == mw)
				MW(sh) = NULL
			}
		    }
		    call smw_close (mw)
		}
	    }
	}
	do j = 1, naps {
	    do i = 1, NS(ns,j) {
		sh = SH(b,j,i)
		if (sh == NULL)
		    next
		call shdr_close (sh)
	    }
	    call mfree (Memi[b+j-1], TY_POINTER)
	}
	call mfree (b, TY_POINTER)

	call mfree (ns, TY_INT)
end


# ODC_REBIN -- Rebin spectra and masks.

procedure odc_rebin (refim, shout, s, b, n, mformat, mtype, mvalue, output)

pointer	refim			#I Output reference image
pointer	shout			#I Output spectrum structure
pointer	s[ARB]			#I Array of spectrum structures
pointer	b[ARB]			#I Array of BPM spectrum structures
int	n			#I Number of spectra
int	mformat			#I Mask format
int	mtype			#I Mask type
int	mvalue			#I Mask value
char	output[ARB]		#I Output rootname

int	i, j, k, p1, p2, npts
double	c[3], d[3,3]
pointer	sh, bpm, im, mw
pointer	sp, str

int	mw_stati()
double	shdr_lw(), shdr_wl()
pointer	immap(), mw_openim(), impl1r()
errchk	immap, mw_openim, impl1r

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	j = 0
	do i = 1, n {
	    sh = s[i]
	    bpm = b[i]

	    # Determine limits of input spectrum relative to the output
	    # spectrum.

	    c[1] = shdr_wl (shout, shdr_lw (sh, double(0.5)))
	    c[2] = shdr_wl (shout, shdr_lw (sh, double(SN(sh)+0.5)))
	    p1 = max (1, nint (min (c[1], c[2]) + 0.01))
	    p2 = min (SN(shout), nint (max (c[1], c[2]) - 0.01))
	    npts = p2 - p1 + 1
	    if (npts < 1)
		next
	    p1 = 1 - p1

	    # Rebin the spectra and masks.
	    call shdr_rebin (sh, shout)
	    call odc_bpm (bpm, shout, mtype, mvalue)

	    # Write the results.  We only write the part of the output
	    # contained by the input spectrum and then let the combining deal
	    # with the origin offsets.  This is done by setting the physical
	    # pixel coordinate system to match the desired output system.
	    # The main reason for this it to make the output of bounds
	    # pixel implicitly bad or excluded.

	    j = j + 1
	    call sprintf (Memc[str], SZ_LINE, "%s.%04d")
		call pargstr (output)
		call pargi (j)
	    im = immap (Memc[str], NEW_COPY, refim)
	    call sprintf (Memc[str], SZ_LINE, "%s%s(%s)")
		call pargstr (IMNAME(sh))
		call pargstr (IMSEC(sh))
		call pargi (AP(sh))
	    call imastr (im, "ICFNAME", Memc[str])
	    IM_LEN(im,1) = npts
	    if (p1 != 0) {
		mw = mw_openim (im)
		k = mw_stati (mw, MW_NPHYSDIM)
		call mw_gltermd (mw, d, c, k)
		c[1] = c[1] + p1
		call mw_sltermd (mw, d, c, k)
		call mw_saveim (mw, im)
	    }
	    call amovr (Memr[SY(sh)-p1], Memr[impl1r(im)], npts)
	    if (bpm != NULL) {
		switch (mformat) {
		case BPMPIX:
		    call sprintf (Memc[str], SZ_LINE, "%s%s")
			call pargstr (IMNAME(bpm))
			call pargstr (IMSEC(bpm))
		case BPMSPEC:
		    call sprintf (Memc[str], SZ_LINE, "%s%s(%s)")
			call pargstr (IMNAME(bpm))
			call pargstr (IMSEC(bpm))
			call pargi (AP(bpm))
		}
		call imastr (im, "ICBPM", Memc[str])
		call sprintf (Memc[str], SZ_LINE, "%sbpm.%04d")
		    call pargstr (output)
		    call pargi (j)
		call imastr (im, "BPM", Memc[str])
	    } else iferr (call imdelf (im, "BPM"))
		;
	    call imunmap (im)

	    if (bpm == NULL)
		next

	    im = immap (Memc[str], NEW_COPY, refim)
	    IM_PIXTYPE(im) = TY_INT
	    IM_LEN(im,1) = npts
	    if (p1 != 0) {
		mw = mw_openim (im)
		k = mw_stati (mw, MW_NPHYSDIM)
		call mw_gltermd (mw, d, c, k)
		c[1] = c[1] + p1
		call mw_sltermd (mw, d, c, k)
		call mw_saveim (mw, im)
	    }
	    call amovr (Memr[SY(bpm)-p1], Memr[impl1r(im)], npts)
	    iferr (call imdelf (im, "BPM"))
	         ;
	    call imunmap (im)
	}

	call sfree (sp)
end


# ODC_BPM -- Rebin the bad pixel masks.
#
# Even though the input mask can be specified by good or bad values or bits
# the rebinned mask is created as a boolean mask.  Note that the rebinning
# is done by setting a large mask value and then values computed from good
# and bad pixels will have some intermediate value which we then threshold
# to define good and bad.

procedure odc_bpm (sh, shout, mtype, mvalue)

pointer	sh		#I SHDR pointer for mask spectrum
pointer	shout		#I SHDR pointer for template output spectrum
int	mtype		#I Mask type
int	mvalue		#I Mask value

int	i, n, val, and()
pointer	ptr

begin
	if (sh == NULL)
	    return

	n = SN(sh)
	ptr = SY(sh)
	switch (mtype) {
	case M_GOODVAL:
	    do i = 1, n {
	        val = nint (Memr[ptr])
	        if (val == mvalue)
		    Memr[ptr] = 0
		else
		    Memr[ptr] = 1000
		ptr = ptr + 1
	    }
	case M_BADVAL:
	    do i = 1, n {
	        val = nint (Memr[ptr])
	        if (val != mvalue)
		    Memr[ptr] = 0
		else
		    Memr[ptr] = 1000
		ptr = ptr + 1
	    }
	case M_GOODBITS:
	    do i = 1, n {
	        val = nint (Memr[ptr])
	        if (and (val, mvalue) != 0)
		    Memr[ptr] = 0
		else
		    Memr[ptr] = 1000
		ptr = ptr + 1
	    }
	case M_BADBITS:
	    do i = 1, n {
	        val = nint (Memr[ptr])
	        if (and (val, mvalue) == 0)
		    Memr[ptr] = 0
		else
		    Memr[ptr] = 1000
		ptr = ptr + 1
	    }
	}

	call shdr_rebin (sh, shout)

	n = SN(sh)
	ptr = SY(sh)
	do i = 1, n {
	    val = nint (Memr[ptr])
	    if (val < 10)
		Memr[ptr] = 0
	    else
		Memr[ptr] = 1
	    ptr = ptr + 1
	}
end


# ODC_OUTPUT - Set the output spectrum.

procedure odc_output (sh, ns, output, im, mw, refim)

pointer	sh[ARB]			# spectra structures
int	ns			# number of spectra
char	output[SZ_FNAME]	# output spectrum name
pointer	im			# output IMIO pointer
pointer	mw			# output MWCS pointer
pointer	refim			# reference image for output image

int	ap, beam, dtype, nw, axis[2]
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	coeff
pointer	immap(), mw_open(), smw_openim()
errchk	immap, smw_openim
data	axis/1,2/

begin
	coeff = NULL

	# Create output image using the first input image as a reference
	refim = immap (IMNAME(sh[1]), READ_ONLY, 0)
	im = immap (output, NEW_COPY, refim)

	# Use smw_openim to clean up old keywords(?).
	mw = smw_openim (im)
	call smw_close (mw)

	IM_NDIM(im) = 1
	call imaddi (im, "SMW_NDIM", IM_NDIM(im))
	if (IM_PIXTYPE(im) != TY_DOUBLE)
	    IM_PIXTYPE(im) = TY_REAL

	# Set new header.
	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axis, 2, "multispec",
	    "label=Wavelength units=Angstroms")
	call smw_open (mw, NULL, im)

	call smw_gwattrs (MW(sh[1]), APINDEX(sh[1]), 1, ap, beam, dtype,
	    w1, dw, nw, z, aplow, aphigh, coeff)
	call odc_default (sh, ns, dtype, w1, dw, nw, z, Memc[coeff])
	call smw_swattrs (mw, 1, 1, ap, beam, dtype,
	    w1, dw, nw, z, aplow, aphigh, Memc[coeff])
	call smw_sapid (mw, 1, 1, TITLE(sh[1]))

	IM_LEN(im,1) = nw

	# Set MWCS header.
	call smw_saveim (mw, im)
	call smw_close (mw)
	mw = smw_openim (im)

	call mfree (coeff, TY_CHAR)
end


# ODC_DEFAULT - Set default values for the starting wavelength, ending
# wavelength, wavelength increment and spectrum length for the output
# spectrum.

procedure odc_default (shdr, ns, dtype, w1, dw, nw, z, coeff)

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
	if (clgetb ("first")) {
	    # For now we don't allow non-linear dispersions because the
	    # generic combine routines don't understand multispec.
	    if (dtype == DCFUNC) {
	        dtype = DCLINEAR
		coeff[1] = EOS
		z = 0.
	    }

	    return
	}

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


# ODC_IMTGETIM -- Set output image from an list of root names.

procedure odc_imtgetim (list, index, aperture, image, maxch)

int	list			#I List of images
int	index			#I List index
int	aperture		#I Aperture
char	image[maxch]		#O Image name
int	maxch			#I Maximum character for image

pointer	sp, root, extn

int	imtrgetim()

begin
	if (imtrgetim (list, index, image, maxch) == EOF) {
	    image[1] = EOS
	    return
	}

	if (aperture == INDEFI)
	    return

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	call iki_init()
	call iki_parse (image, Memc[root], Memc[extn])
	if (Memc[extn] == EOS) {
	    call sprintf (image, maxch, "%s.%04d")
		call pargstr (Memc[root])
		call pargi (aperture)
	} else {
	    call sprintf (image, maxch, "%s.%04d.%s")
		call pargstr (Memc[root])
		call pargi (aperture)
		call pargstr (Memc[extn])
	}

	call sfree (sp)
end


# ODC_COMBINE -- Combine the spectra by calling the IMCOMBINE source.

procedure odc_combine (slist, blist, output, headers, bmask, rmask, nrmask,
	emask, sigma, logfile, delete)

int	slist			#I List of 1D spectra to combine
int	blist			#I List of 1D bad pixel spectra
char	output[ARB]		#I Output combined spectrum
char	headers[ARB]		#I Output headers
char	bmask[ARB]		#I Output bad pixel mask
char	rmask[ARB]		#I Output rejection mask
char	nrmask[ARB]		#I Output number rejected mask
char	emask[ARB]		#I Ouput exposure time mask
char	sigma[ARB]		#I Output sigma
char	logfile[ARB]		#I Logfile
int	delete			#I Delete input spectra?

int	n
pointer	sp, fname, scales, zeros, wts

int	imtlen(), imtgetim()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Allocate and initialize scaling factors.
	n = imtlen (slist)
	call salloc (scales, 3*n, TY_REAL)
	zeros = scales + n
	wts = scales + 2 * n
	call amovkr (INDEFR, Memr[scales], 3*n)

	# Combine.
	iferr (call icombine (slist, output, headers, bmask, rmask,
	    nrmask, emask, sigma, logfile, Memr[scales], Memr[zeros],
	    Memr[wts], NO, NO))
	    call erract (EA_WARN)

	# Delete the files.
	if (delete == YES) {
	    call imtrew (slist)
	    while (imtgetim (slist, Memc[fname], SZ_FNAME) != EOF)
		call imdelete (Memc[fname])
	    call imtrew (blist)
	    while (imtgetim (blist, Memc[fname], SZ_FNAME) != EOF)
		call imdelete (Memc[fname])
	}

	call sfree (sp)
end
