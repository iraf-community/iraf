include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	<math/iminterp.h>
include	"apertures.h"
include	"extract.h"
include	"exio.h"

define	EX_CMAX	20	# Maximum number of simultaneous extractions
define	EX_LMAX	20	# Maximum number of simultaneous extractions

# T_APNORMALIZE -- Normalize aperture spectra.
#
# A normalization spectrum is extracted for each aperture.  The spectrum
# is smoothed by a fitting function and a minimum threshold may be set (to
# avoid dividing by small numbers).  The function fitting may be performed
# interactively using the icfit package.  The normalization spectrum is
# divided into the 2D aperture spectrum.  Regions outside the apertures are
# set to unity.

procedure t_apnormalize ()

int	listin			# List of input images to be normalized
int	listout			# List of output images (may be empty)
int	listrefs		# List of reference images (may be empty)

int	interactive		# Run interactively?
int	recenter		# Recenter reference apertures?
int	find			# Find apertures automatically?
int	edit			# Edit apertures?
int	trace			# Trace apertures?
int	fittrace		# Fit traced points interactively?
int	norm			# Normalize aperture spectra?
int	normedit		# Edit normalization apertures?
int	fitnorm			# Fit normalization spectra interactively?
int	dbwrite			# Write aperture data to database?

pointer	aps			# Pointer to array of aperture pointers
int	naps			# Number of apertures

int	i, line, nsum, trline
pointer	sp, image, reference, str

bool	clgetb(), strne()
int	ap_answer(), ap_getim(), btoi(), clgeti(), imtopenp()

errchk	tr_trace, ap_flat, ap_find, ap_edit, ap_getaps, ap_recenter, ap_dbread

begin
	call smark (sp)
	call salloc (aps, AP_MAXAPS, TY_INT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (reference, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	listin = imtopenp ("input")
	listout = imtopenp ("output")
	listrefs = imtopenp ("references")

	interactive = btoi (clgetb ("interactive"))
	recenter = ap_answer ("recenter", interactive)
	find = ap_answer ("find", interactive)
	edit = ap_answer ("edit", interactive)
	trace = ap_answer ("trace", interactive)
	fittrace = ap_answer ("aptrace.fittrace", interactive)
	norm = ap_answer ("normalize", interactive)
	normedit = ap_answer ("normedit", interactive)
	fitnorm = ap_answer ("fitnorm", interactive)
	dbwrite = ap_answer ("apio.dbwrite", interactive)

	if (interactive == NO) {
	    edit = ALWAYSNO
	    fittrace = ALWAYSNO
	    normedit = ALWAYSNO
	    fitnorm = ALWAYSNO
	}

	naps = 0
	Memc[reference] = EOS

	line = clgeti ("apedit.line")
	nsum = clgeti ("apedit.nsum")
	trline = clgeti ("aptrace.line")

	# Initialize the trace and normalize parameters.
	call tr_init ()
	call norm_open ()

	# Process each input image.
	while (ap_getim (listin, Memc[image], SZ_FNAME) != EOF) {
	    if (ap_getim (listrefs, Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[reference], SZ_FNAME)

	    iferr {
		if (clgetb ("apio.verbose"))
		    call printf ("Searching aperture database ...\n")

		# Get the reference apertures.
		iferr (call ap_dbread (Memc[reference], Memi[aps], naps))
		    call error (0, "Reference image apertures not found")
		if ((naps != 0) && strne (Memc[reference], Memc[image]))
		    call ap_recenter (Memc[image], line, nsum,
			recenter, dbwrite, Memi[aps], naps)

		# If no apertures then get the image apertures.
		if (naps == 0)
	    	    iferr (call ap_dbread (Memc[image], Memi[aps], naps))
			;

		# Find apertures additional apertures.
	        call ap_find (Memc[image], line, nsum, find, dbwrite, 
		    Memi[aps], naps)

		# Edit apertures.
		call ap_edit (Memc[image], line, nsum, edit, dbwrite,
		    Memi[aps], naps)

		# Trace the apertures.
		call tr_trace (Memc[image], trline, trace, fittrace,
		    dbwrite, Memi[aps], naps)

		# Make an aperture plot.
		call ap_plot (Memc[image], line, nsum, Memi[aps], naps)

		# Record last apertures.
		if ((dbwrite == YES) || (dbwrite == ALWAYSYES))
	            call ap_dbwrite ("last", ALWAYSYES, Memi[aps], naps)

		# Normalize the apertures.
		if (ap_getim (listout, Memc[str], SZ_LINE) == EOF) {
		    call mktemp ("tmp", Memc[str], SZ_LINE)
	            call normalize (Memc[image], Memc[str], Memi[aps], naps,
			line, nsum, norm, normedit, fitnorm)
		    call imdelete (Memc[image])
		    call imrename (Memc[str], Memc[image])
		} else
	            call normalize (Memc[image], Memc[str], Memi[aps], naps,
			line, nsum, norm, normedit, fitnorm)

		# Free memory.
		for (i = 1; i <= naps; i = i + 1)
		    call ap_free (Memi[aps+i-1])
		naps = 0
	    } then
		call erract (EA_WARN)
	}

	# Free memory and finish up.
	call ap_gclose ()
	call tr_done ()
	call norm_close ()
	call imtclose (listin)
	call imtclose (listrefs)
	call imtclose (listout)
	call sfree (sp)
end


# NORMALIZE -- Normalize the apertures in the input image.

procedure normalize (input, output, aps, naps, line, nsum, norm, normedit,
    fitnorm)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures
int	line			# Line to be edited
int	nsum			# Number of lines to sum in edit line
int	norm			# Normalize apertures?
int	normedit		# Edit normalization apertures?
int	fitnorm			# Fit normalization spectra interactively?

pointer	im, in, out, sp, aps1, str, ex, normspec
int	i, j, naps1, fitnorm1, dispaxis, npts, ex_max, init

pointer	ex_map(), immap()
errchk	ex_map, immap

begin
	if (naps < 1)
	    return

	call smark (sp)
	call salloc (aps1, AP_MAXAPS, TY_POINTER)
	call salloc (str, SZ_LINE, TY_CHAR)

	call sprintf (Memc[str], SZ_LINE, "Normalize apertures in %s?")
	    call pargstr (input)
	call xt_answer (Memc[str], norm)
	if ((norm == NO) || (norm == ALWAYSNO)) {
	    call sfree (sp)
	    return
	}

	# Copy the apertures and allow the user to change them.
	do i = 1, naps
	    call ap_copy (aps[i], Memi[aps1+i-1])
	naps1 = naps

	call sprintf (Memc[str], SZ_LINE,
	    "Edit normalization apertures for %s?")
	   	call pargstr (input)
	call xt_answer (Memc[str], normedit)
	if ((normedit == YES) || (normedit == ALWAYSYES))
	    call ap_edit (input, line, nsum, ALWAYSYES, ALWAYSNO, Memi[aps1],
		naps1)
	if (naps1 != naps)
	    call error (0, "Wrong number of normalization apertures")
		
	call sprintf (Memc[str], SZ_LINE,
	    "Fit normalization spectra for %s interactively?")
	    call pargstr (input)
	call xt_answer (Memc[str], fitnorm)

	fitnorm1 = fitnorm
	if (fitnorm1 == NO)
	    fitnorm1 = ALWAYSNO

	# Map the input and output images.
	im = ex_map (input)
	in = EX_IM(im)
	out = immap (output, NEW_COPY, in)
	IM_PIXTYPE(out) = TY_REAL
	dispaxis = EX_DAXIS(im)
	npts = EX_DLEN(im)

	# Set extraction parameters.
	call salloc (ex, EX_LENSTRUCT, TY_STRUCT)
	EX_ONED(ex) = YES
	EX_WTTYPE(ex) = PROFILE
	EX_BKGD(ex) = NONE
	EX_CLEAN(ex) = NO
	call asiinit (EX_ASI(ex), II_SPLINE3)

	switch (dispaxis) {
	case 1:
	    ex_max = EX_CMAX
	    call salloc (normspec, min (ex_max, naps) * npts, TY_REAL)
	    init = YES
	    do i = 1, naps, ex_max {
		j = min (naps, i + ex_max - 1) - i + 1
	        call ex_sum1 (ex, im, im, Memi[aps1+i-1],
		    Memi[aps1+i-1], j, Memr[normspec], Memr[normspec], npts)
		call fitnormspec (in, Memi[aps1+i-1], j, Memr[normspec], npts,
		    fitnorm1)
		call cnormalize (in, out, Memr[normspec], npts, aps[i], j, init)
		init = NO
	    }
	case 2:
	    ex_max = EX_LMAX
	    call salloc (normspec, min (ex_max, naps) * npts, TY_REAL)
	    init = YES
	    do i = 1, naps, ex_max {
		j = min (naps, i + ex_max - 1) - i + 1
	        call ex_sum1 (ex, im, im, Memi[aps1+i-1],
		    Memi[aps1+i-1], j, Memr[normspec], Memr[normspec], npts)
		call fitnormspec (in, Memi[aps1+i-1], j, Memr[normspec], npts,
		    fitnorm1)
		call lnormalize (in, out, Memr[normspec], npts, aps[i], j, init)
		init = NO
	    }
	}

	call imaddr (out, "ccdmean", 1.)
	call ex_unmap (im)
	call imunmap (out)
	for (i = 1; i <= naps1; i = i + 1)
	    call ap_free (Memi[aps1+i-1])
	call sfree (sp)
end


# FITNORMSPEC -- Fit the normalization spectrum by a smoothing function.

procedure fitnormspec (in, aps, naps, normspec, npts, fitnorm)

pointer	in			# Input image (used for labels)
pointer	aps[naps]		# Apertures (used for labels)
int	naps			# Number of apertures
real	normspec[npts,naps]	# Normalization spectra
int	npts			# Number of points in spectra
int	fitnorm			# Fit normalization spectra interactively?

int	i, fd, apaxis
real	threshold, width
pointer	sp, str, x, wts
pointer	cv, gp, gt, ic

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, npts, TY_REAL)
	call salloc (wts, npts, TY_REAL)

	do i = 1, npts {
	    Memr[x+i-1] = i
	    Memr[wts+i-1] = 1
	}

	call norm_params (ic, gt, threshold)
	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real (npts))
	apaxis = AP_AXIS(aps[1])
	switch (apaxis) {
	case 1:
	    call ic_pstr (ic, "xlabel", "Line")
	case 2:
	    call ic_pstr (ic, "xlabel", "Column")
	}
	call gt_sets (gt, GTTYPE, "line")

	# Fit each normalization spectrum by a smoothing function.
	do i = 1, naps {
	    call sprintf (Memc[str], SZ_LINE,
		"%s: %s - Aperture %s")
		call pargstr (IM_HDRFILE(in))
		call pargstr (IM_TITLE(in))
		call pargi (AP_ID(aps[i]))
	    call gt_sets (gt, GTTITLE, Memc[str])

	    # Query the user to fit the normalization spectrum interactively.
	    call sprintf (Memc[str], SZ_LINE,
	    "Fit spectrum for aperture %d for %s interactively?")
		call pargi (AP_ID(aps[i]))
	        call pargstr (IM_HDRFILE(in))
	    call xt_answer (Memc[str], fitnorm)

	    # Normalize by the aperture width.
	    width = AP_HIGH(aps[i], apaxis) - AP_LOW(aps[i], apaxis)
	    call adivkr (normspec[1,i], width, normspec[1,i], npts)

	    if ((fitnorm == YES) || (fitnorm == ALWAYSYES)) {
		call ap_gopen (gp)
	        call icg_fit (ic, gp, "apio.cursor", gt, cv, Memr[x],
		    normspec[1,i], Memr[wts], npts)
		call amovkr (1., Memr[wts], npts)
	    } else
	        call ic_fit (ic, cv, Memr[x], normspec[1,i], Memr[wts], npts,
		    YES, YES, YES, YES)

	    # Make a graph to the plot log.
	    call ap_popen (gp, fd)
	    if (gp != NULL) {
		call icg_graphr (ic, gp, gt, cv, Memr[x], normspec[1,i],
		    Memr[wts], npts)
		call ap_pclose (gp, fd)
	    }

	    call cvvector (cv, Memr[x], normspec[1,i], npts)
	    call cvfree (cv)

	    # Replace spectrum values below threshold by threshold.
	    if (!IS_INDEF (threshold))
	        call arltr (normspec[1,i], npts, threshold, threshold)
	}
end


# LNORMALIZE -- Normalize the input line apertures by the norm spectra.

procedure lnormalize (in, out, normspec, npts, aps, naps, init)

pointer	in			# Input image
pointer	out			# Output image
real	normspec[npts, naps]	# Normalization spectra
int	npts			# Number of points in spectra
pointer	aps[naps]		# Apertures to normalize
int	naps			# Number of apertures
int	init			# Fill between apertures with 1?

int	i, j, ncols, low, high
real	center, cveval()
pointer	datain, dataout, imgl2r(), impl2r()

begin
	ncols = IM_LEN(out, 1)
	do i = 1, npts {
	    dataout = impl2r (out, i)
	    if (init == YES)
	        call amovkr (1., Memr[dataout], ncols)
	    else {
	        datain = imgl2r (out, i)
		call amovr (Memr[datain], Memr[dataout], ncols)
	    }
	    datain = imgl2r (in, i)

	    do j = 1, naps {
		center = AP_CEN(aps[j], 1) + cveval (AP_CV(aps[j]), real (i))
		low = center + AP_LOW(aps[j], 1) + 0.5
		high = center + AP_HIGH(aps[j], 1) + 0.5
		low = max (1, min (ncols, low))
		high = max (1, min (ncols, high))
		if (high >= low)
		    call adivkr (Memr[datain+low-1], normspec[i,j],
		        Memr[dataout+low-1], high-low+1)
	    }
	}
end


# CNORMALIZE -- Normalize the input column apertures by the norm spectra.

procedure cnormalize (in, out, normspec, npts, aps, naps, init)

pointer	in			# Input image
pointer	out			# Output image
real	normspec[npts, naps]	# Normalization spectra
int	npts			# Number of points in spectra
pointer	aps[naps]		# Apertures to normalize
int	naps			# Number of apertures
int	init			# Initialize between the apertures?

int	i, j, k, ncols, nlines, low, high, minlow, maxhigh
int	line1, line2, nl, bufsize, imstati(), sizeof()
real	center, norm, cveval()
pointer	datain, dataout, imgs2r(), imps2r()
pointer	sp, lowbuf, highbuf, minval, maxval, ptr1, ptr2, ptr3, ptr4

begin
	call smark (sp)
	call salloc (lowbuf, naps * npts, TY_INT)
	call salloc (highbuf, naps * npts, TY_INT)
	call salloc (minval, naps, TY_INT)
	call salloc (maxval, naps, TY_INT)

	ncols = IM_LEN (out, 1)
	nlines = IM_LEN (out, 2)

	ptr1 = lowbuf
	ptr2 = highbuf
	do j = 1, naps {
	    minlow = nlines
	    maxhigh = 0
	    do i = 1, npts {
	        center = AP_CEN(aps[j], 2) + cveval (AP_CV(aps[j]), real (i))
	    	low = center + AP_LOW(aps[j], 2) + 0.5
	    	high = center + AP_HIGH(aps[j], 2) + 0.5
	    	low = max (1, min (ncols, low))
	    	high = max (1, min (ncols, high))
		if (low < minlow)
		    minlow = low
		if (high > maxhigh)
		    maxhigh = high
		Memi[ptr1] = low
		Memi[ptr2] = high
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + 1
	    }
	    Memi[minval+j-1] = minlow
	    Memi[maxval+j-1] = maxhigh
	}
	call alimi (Memi[minval], naps, minlow, i)
	call alimi (Memi[maxval], naps, i, maxhigh)

	# Do the normalization in blocks of the default IMIO buffer size.
	# The output is set to unity when initializing between the
	# apertures and then the regions containing the
	# apertures are replaced by the normalized input spectra.

	bufsize = imstati (out, IM_BUFSIZE)
	nl = bufsize / ncols / sizeof (IM_PIXTYPE(out))
	do line1 = 1, nlines, nl {
	    line2 = min (line1+nl-1, nlines)
	    if (init == YES) {
	        dataout = imps2r (out, 1, ncols, line1, line2)
	        call amovkr (1., Memr[dataout], ncols*(line2-line1+1))
		if (line1 > maxhigh)
		    next
		if (line2 < minlow)
		    next
	    } else {
		if (line1 > maxhigh)
		    next
		if (line2 < minlow)
		    next
	        dataout = imps2r (out, 1, ncols, line1, line2)
	        datain = imgs2r (out, 1, ncols, line1, line2)
	        call amovr (Memr[datain], Memr[dataout], ncols*(line2-line1+1))
	    }
	    datain = imgs2r (in, 1, ncols, line1, line2)

	    do j = 1, naps {
		# Check that the aperture overlaps the current buffer.
		if (line1 > Memi[maxval+j-1])
		    next
		if (line2 < Memi[minval+j-1])
		    next

	        ptr1 = lowbuf + (j - 1) * npts
	        ptr2 = highbuf + (j - 1) * npts
	        do i = 1, ncols {
		    low = max (line1, Memi[ptr1])
		    high = min (line2, Memi[ptr2])
		    ptr1 = ptr1 + 1
		    ptr2 = ptr2 + 1

	    	    if (low > high)
			next
		    ptr3 = datain + i - 1
		    ptr4 = dataout + i - 1
		    norm = normspec[i,j]
		    do k = low - line1, high - line1
			Memr[ptr4+k*ncols] = Memr[ptr3+k*ncols] / norm
		}
	    }
	}

	call sfree (sp)
end


# NORM_PARAMS -- Return parameters for normalization operation.
# NORM_OPEN -- Open normalization operation.
# NORM_CLOSE -- Close normalization operation.

procedure norm_params (ic, gt, threshold, interactive)

pointer	ic		# ICFIT pointer
pointer	gt		# GTOOLS pointer
real	threshold	# Threshold for normalization spectrum
int	interactive	# Fit interactively?

int	clgeti()
real	threshold1, clgetr()
pointer	ic1, gt1, gt_init()
pointer	sp, str

begin
	ic = ic1
	gt = gt1
	threshold = threshold1
	return

entry	norm_open ()

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call ic_open (ic1)

	call clgstr ("function", Memc[str], SZ_LINE)
	call ic_pstr (ic1, "function", Memc[str])
	call ic_puti (ic1, "order", clgeti ("order"))
	call clgstr ("sample", Memc[str], SZ_LINE)
	call ic_pstr (ic1, "sample", Memc[str])
	call ic_puti (ic1, "naverage", clgeti ("naverage"))
	call ic_puti (ic1, "niterate", clgeti ("niterate"))
	call ic_putr (ic1, "low", clgetr ("low_reject"))
	call ic_putr (ic1, "high", clgetr ("high_reject"))
	call ic_putr (ic1, "grow", clgetr ("grow"))
	call ic_pstr (ic1, "ylabel", "")

	gt1 = gt_init()
	threshold1 = clgetr ("threshold")

	call sfree (sp)
	return

entry	norm_close ()

	call ic_closer (ic1)
	call gt_free (gt1)
end
