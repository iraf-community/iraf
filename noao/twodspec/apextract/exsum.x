include	<imhdr.h>
include	<error.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"exio.h"
include	"apertures.h"
include	"extract.h"

define	EX_MAX	50		# Maximum number of simultaneous extractions

# EX_SUM -- Extract 1D aperture sums
#
# Weighted extractions of the pixels within the extraction apertures
# centered on the traced positions is made on each column or line of the
# input images.  Multiple features in the same image are extracted to
# output images with the specified root name and a sequential extension.

procedure ex_sum (input, output, sky, profiles, review, aps, naps)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image root name
char	sky[SZ_FNAME]		# Output sky root name
char	profiles[SZ_FNAME]	# Profile reference image
int	review			# Review extractions and output names?
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

int	i, ap1, ap2, nap, npts, rev, ex_max, clobber
pointer	in, immod, apsmod, sp, str, ex, bufout, skyout

int	clgeti(), clgwrd(), clginterp(), btoi()
real	clgetr()
bool	clgetb(), streq()
pointer	ex_map()

errchk	ex_map, ex_gmodaps

begin
	if (naps == 0) {
	    call eprintf ("APSUM - No apertures defined for %s\n")
		call pargstr (input)
	    return
	}

	if (clgetb ("interactive"))
	    clobber = NO
	else
	    clobber = ALWAYSNO
	call ex_sumout (input, output, review, clobber, NULL, aps, naps, 1,
	    naps, Memi[1], 1, i)
	if (i == 0) {
	    call eprintf ("APSUM - Output spectra exist for %s\n")
		call pargstr (input)
	    return
	}

	# Map the input image.
	in = ex_map (input)
	npts = EX_DLEN (in)

	# Get extraction parameters.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (ex, EX_LENSTRUCT, TY_STRUCT)

	EX_ONED(ex) = YES
	EX_WTTYPE(ex) = clgwrd ("apsum.weights", Memc[str], SZ_LINE, WEIGHTS)
	EX_BKGD(ex) = clgwrd ("apsum.background", Memc[str],SZ_LINE, BACKGROUND)
	EX_CLEAN(ex) = btoi (clgetb ("apsum.clean"))
	EX_NAVG(ex) = clgeti ("apsum.naverage") + 1
	EX_NCLEAN(ex) = clgeti ("apsum.nclean")
	EX_LSIGMA(ex) = clgetr ("apsum.lsigma")
	EX_USIGMA(ex) = clgetr ("apsum.usigma")
	EX_V0(ex) = clgetr ("apsum.v0")
	EX_V1(ex) = clgetr ("apsum.v1")
	call asiinit (EX_ASI(ex), clginterp ("apsum.interpolator"))

	# If using profile fitting get the profile image and apertures.
	immod = in
	if ((EX_WTTYPE(ex) == VARIANCE) || (EX_CLEAN(ex) == YES)) {
	    call salloc (apsmod, AP_MAXAPS, TY_INT)
	    if (profiles[1] == EOS || streq (profiles, input)) {
	        do i = 1, naps
		    Memi[apsmod+i-1] = aps[i]
	    } else {
	        immod = ex_map (profiles)
		call ex_gmodaps (profiles, aps, Memi[apsmod], naps)
	    }
	}

	# Query the user whether to review the extraction.
	call sprintf (Memc[str], SZ_LINE,
	    "Review extracted spectra from %s?")
	    call pargstr (input)
	call xt_answer (Memc[str], review)

	rev = review
	if (rev == NO)
	    rev = ALWAYSNO

	# Limit the number of aperture extracted simultaneously.
	ex_max = EX_MAX
	call salloc (bufout, min (ex_max, naps) * npts, TY_REAL)
	if (EX_BKGD(ex) != NONE)
	    call salloc (skyout, min (ex_max, naps) * npts, TY_REAL)
	else
	    skyout = bufout

	if (clgetb ("apio.verbose")) {
	    call printf ("Extracting apertures ...\n")
	    call flush (STDOUT)
	}

	do ap1 = 1, naps, ex_max {
	    ap2 = min (naps, ap1 + ex_max - 1)
	    nap = ap2 - ap1 + 1

	    call ex_sum1 (ex, in, immod, aps[ap1],
		Memi[apsmod+ap1-1], nap, Memr[bufout], Memr[skyout], npts)
	    call ex_sumout (input, output, rev, clobber, EX_IM(in), aps, naps,
		ap1, nap, Memr[bufout], npts, i)
	    if (EX_BKGD(ex) != NONE)
		call ex_sumsky (input, sky, clobber, EX_IM(in), aps, naps, ap1,
		    nap, Memr[skyout], npts)
	}

	if (immod != in) {
	    call ex_unmap (immod)
	    do i = 1, naps {
		if (Memi[apsmod+i-1] != aps[i])
		    call ap_free (Memi[apsmod+i-1])
	    }
	}

	call asifree (EX_ASI(ex))
	call ex_unmap (in)
	call sfree (sp)
end


# EX_SUM1 -- Extract 1D aperture sums.
# This procedure does most of the work of getting the profiles from the
# input image, doing any shifting and model fitting, and extracting the
# profile sums.

procedure ex_sum1 (ex, in, immod, aps, apsmod, naps, bufout, skyout, nlines)

pointer	ex			# Pointer to extraction parameters
pointer	in			# Input IMIO pointer
pointer	immod			# Model IMIO pointer
pointer	aps[naps]		# Apertures
pointer	apsmod[naps]		# Model apertures
int	naps			# Number of apertures
real	bufout[nlines,naps]	# Output extracted data
real	skyout[nlines,naps]	# Output sky data
int	nlines			# Number of points

int	i, j, ncols, nextract, line, len_profs, replace, apaxis
real	center, low, high, junk
pointer	sp, data, model, avg, pstart, pcen, pend, pnrep, bckgrnd
pointer	bufin

real	cveval()
pointer	ex_g2r()

begin
	call smark (sp)
	apaxis = EX_AAXIS(in)
	ncols = EX_ALEN(in)

	# Select profile fitting if needed.
	if ((EX_WTTYPE(ex) == VARIANCE) || (EX_CLEAN(ex) == YES)) {
	    # Set the end points of the profile in the profile array
	    call salloc (pstart, naps, TY_INT)
	    call salloc (pcen, naps, TY_REAL)
	    call salloc (pend, naps, TY_INT)
	    call salloc (pnrep, naps, TY_INT)
	    j = 0
	    do i = 1, naps {
		nextract = nint (AP_HIGH(aps[i],apaxis)) -
		    nint (AP_LOW(aps[i],apaxis)) + 1
		nextract = min (nextract, ncols) + 2

		Memi[pstart+i-1] = j + 1
	        Memi[pend+i-1] = j + nextract
		j = j + nextract
	    }
	    len_profs = j

	    # Allocate working memory for the profiles.
	    call salloc (data, len_profs, TY_REAL)
	    call salloc (model, len_profs, TY_REAL)
	    call salloc (bckgrnd, len_profs, TY_REAL)
	    if (EX_NAVG(ex) > nlines) {
		# Compute average profile.
	        call salloc (avg, len_profs, TY_REAL)
		call aclrr (Memr[avg], len_profs)
		do line = 1, nlines {
	            call ex_gprofs (ex, immod, line, apsmod, naps,
			Memi[pstart], Memi[pcen], Memi[pend], YES, Memr[model],
			Memr[bckgrnd])
		    call aaddr (Memr[model], Memr[avg], Memr[avg], len_profs)
		}
	    } else if (EX_NAVG(ex) > 1) {
	        call salloc (avg, len_profs * (EX_NAVG(ex) + 1), TY_REAL)
		if (immod == in && EX_CLEAN(ex) == YES)
		    replace = YES
		else
		    replace = NO
	    }

	    # Extract each line of the apertures.
	    do line = 1, nlines {
		do i = 1, naps {
		    center = AP_CEN(aps[i],apaxis) +
			cveval (AP_CV(aps[i]), real (line))
		    low = nint (center + AP_LOW(aps[i],apaxis))
		    Memr[pcen+i-1] = center - low + Memi[pstart+i-1] + 1
		}

	        # Get the data and model spectra.  Variance weighting is
	        # implemented by extracting the model spectrum.
		if (EX_NAVG(ex) > nlines) {
	            call ex_recen (ex, Memi[pstart], Memr[pcen], Memi[pend],
			naps, Memr[avg], Memr[model], 1)
		} else if (EX_NAVG(ex) > 1) {
		    if (in == immod)
	                call ex_mvsum2 (ex, immod, line, apsmod, naps,
			    Memi[pstart], Memi[pcen], Memi[pend], Memr[avg],
			    Memr[bckgrnd], len_profs)
		    else 
	                call ex_mvsum1 (ex, immod, line, apsmod, naps,
			    Memi[pstart], Memi[pcen], Memi[pend], Memr[avg],
			    Memr[bckgrnd], len_profs)
	            call ex_recen (ex, Memi[pstart], Memr[pcen], Memi[pend],
			naps, Memr[avg], Memr[model], 1)
		} else {
	            call ex_gprofs (ex, immod, line, apsmod, naps,
			Memi[pstart], Memr[pcen], Memi[pend], NO, Memr[model],
			Memr[bckgrnd])
		}
	        call ex_gprofs (ex, in, line, aps, naps, Memi[pstart],
		    Memr[pcen], Memi[pend], NO, Memr[data], Memr[bckgrnd])

		call ex_fit (ex, line, Memi[pstart], Memi[pend], Memi[pnrep],
		    naps, Memr[data], Memr[bckgrnd], Memr[model])
		if (replace == YES)
		    call ex_replace (ex, line, Memi[pstart], Memr[pcen],
			Memi[pend], Memi[pnrep], naps, Memr[data], Memr[avg],
			len_profs)
	        if (EX_WTTYPE(ex) == VARIANCE)
		    call amovr (Memr[model], Memr[data], len_profs)

		# Extract one dimensional sum.
	        do i = 1, naps {
		    center = Memr[pcen+i-1]
		    low = center + AP_LOW(aps[i],apaxis)
		    high = center + AP_HIGH(aps[i],apaxis)
		    call ex_apsum (Memr[data], ncols, low, center, high,
			EX_ASI(ex), NULL, NONE, bufout[line,i], junk)
		    if (EX_BKGD(ex) != NONE)
		        call ex_apsum (Memr[bckgrnd], ncols, low, center, high,
			    EX_ASI(ex), NULL, NONE, skyout[line,i], junk)
	        }
	    }

	} else {
	    # Extract each line of the apertures.
	    do line = 1, nlines {
		bufin = ex_g2r (in, line)
		do i = 1, naps {
		    center = AP_CEN(aps[i],apaxis) +
			cveval (AP_CV(aps[i]), real (line))
		    low = center + AP_LOW(aps[i],apaxis)
		    high = center + AP_HIGH(aps[i],apaxis)
		    call ex_apsum (Memr[bufin], ncols, low, center, high,
			EX_ASI(ex), AP_IC(aps[i]), EX_BKGD(ex),
			bufout[line,i], skyout[line,i])
		}
	    }
	}

	call sfree (sp)
end


define	FORMATS		"|onedspec|multispec|echelle|"
define	ONEDSPEC	1	# Individual 1D spectra
define	MULTISPEC	2	# Multiple spectra
define	ECHELLE		3	# Echelle spectra

# EX_SUMOUT -- Check, Review the extracted spectra and write them to an image.
# To check the output spectra the image pointer is NULL.

procedure ex_sumout (image, output, review, clobber, im, aps, naps, ap1, naps1,
	data, npts, nout)

char	image[ARB]		# Input image name
char	output[ARB]		# Output root name
int	review			# Review spectra and output names?
int	clobber			# Clobber images?
pointer	im			# Input IMIO pointer
pointer	aps[naps]		# Apertures
int	naps			# Number of apertures
int	ap1			# Starting aperture
int	naps1			# Number of apertures to output
real	data[npts, ARB]		# Output data
int	npts			# Number of points
int	nout			# Number of spectra output

int	i, j, k, format, apaxis, dispaxis
pointer	sp, str, str1, name, fmt, out, gt

int	scan(), clgwrd(), imaccess()
pointer	immap(), impl1r(), impl2r(), gt_init()
errchk	immap

begin
	# Allocate string and file name arrays.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (fmt, SZ_FNAME, TY_CHAR)

	format = clgwrd ("apio.format", Memc[fmt], SZ_LINE, FORMATS)

	# Check or write out the extracted spectra.
	nout = naps1
	do i = ap1, ap1+naps1-1 {
	    j = i - ap1 + 1
	    if (output[1] == EOS)
		call strcpy (image, Memc[str], SZ_LINE)
	    else
		call strcpy (output, Memc[str], SZ_LINE)

	    switch (format) {
	    case ECHELLE:
	        call sprintf (Memc[name], SZ_FNAME, "%s.ec")
	            call pargstr (Memc[str])
	    case MULTISPEC:
	        call sprintf (Memc[name], SZ_FNAME, "%s.ms")
	            call pargstr (Memc[str])
	    default:
	        call sprintf (Memc[name], SZ_FNAME, "%s.%04d")
	            call pargstr (Memc[str])
	            call pargi (AP_ID(aps[i]))
	    }

	    if (imaccess (Memc[name], 0) == YES) {
	        switch (format) {
	        case ECHELLE, MULTISPEC:
		    if (i == 1) {
		        if (im == NULL) {
			    call sprintf (Memc[str], SZ_LINE,
			        "Clobber existing output image %s?")
			        call pargstr (Memc[name])
			    call xt_answer (Memc[str], clobber)
			    if (clobber == YES || clobber == ALWAYSYES)
			        call imdelete (Memc[name])
			    else
			        nout = 0
		        }
	    		call sfree (sp)
	    		return
		    }
	        default:
		    if (im == NULL) {
		        call sprintf (Memc[str], SZ_LINE,
		            "Clobber existing output image %s?")
		            call pargstr (Memc[name])
			call xt_answer (Memc[str], clobber)
			if (clobber == YES || clobber == ALWAYSYES)
		            call imdelete (Memc[name])
			else
			    nout = nout - 1
		    }
		    next
	        }
	    }

	    if (im == NULL)
		next
	    
	    # Set the review graph title.
	    call sprintf (Memc[str], SZ_LINE,
		"%s: %s - Aperture %s")
		call pargstr (image)
		call pargstr (IM_TITLE(im))
		call pargi (AP_ID(aps[i]))

	    gt = gt_init (gt)
	    call gt_sets (gt, GTTITLE, Memc[str])

	    # Query the user whether to review the extraction.
	    call sprintf (Memc[str], SZ_LINE,
		"Review extracted spectrum for aperture %d from %s?")
		call pargi (AP_ID(aps[i]))
	        call pargstr (image)
	    call xt_answer (Memc[str], review)

	    # If reviewing graph the spectrum, do a cursor loop, and allow
	    # the user to skip the output or define a new output image.
	    if ((review==YES)||(review==ALWAYSYES)) {
	        call ex_graph (gt, data[1,j], npts)
	    
		if (format == ONEDSPEC) {
	            call printf (
		        "Output image name [use # to skip output] (%s): ")
		        call pargstr (Memc[name])
		    call flush (STDOUT)
	            if (scan() != EOF) {
	                call gargwrd (Memc[str], SZ_LINE)
	                if (Memc[str] == '#')
		            next
	                if (Memc[str] != EOS)
		            call strcpy (Memc[str], Memc[name], SZ_FNAME)
		    }
		}
	    }

	    # Output the image.
	    switch (format) {
	    case ECHELLE, MULTISPEC:
		if (i == 1) {
		    out = immap (Memc[name], NEW_COPY, im)
		    apaxis = AP_AXIS(aps[i])
		    dispaxis = mod (apaxis, 2) + 1
		    IM_PIXTYPE(out) = TY_REAL
		    IM_LEN(out, 1) = IM_LEN(im, dispaxis)
		    IM_LEN(out, 2) = naps
		    call imastr (out, "apformat", Memc[fmt])
		    do k = 1, naps {
			call sprintf (Memc[str], SZ_LINE, "APNUM%d")
			    call pargi (k)
			call sprintf (Memc[str1], SZ_LINE, "%d %d 1 1 %d")
			    call pargi (AP_ID(aps[k]))
			    call pargi (AP_BEAM(aps[k]))
			    call pargi (npts)
			    call imastr (out, Memc[str], Memc[str1])
		    }
		    call ex_setdisp (out, dispaxis)
		} else
		    out = immap (Memc[name], READ_WRITE, 0)
		call amovr (data[1,j], Memr[impl2r(out,i)], npts)
		call imunmap (out)
		    
	    default:
		out = immap (Memc[name], NEW_COPY, im)
		apaxis = AP_AXIS(aps[i])
		dispaxis = mod (apaxis, 2) + 1
		IM_PIXTYPE(out) = TY_REAL
		IM_NDIM(out) = 1
		IM_LEN(out, 1) = IM_LEN(im, dispaxis)
		call sprintf (Memc[str], SZ_LINE, "%s - Aperture %d")
		    call pargstr (IM_TITLE(out))
		    call pargi (AP_ID(aps[i]))
		call strcpy (Memc[str], IM_TITLE(out), SZ_IMTITLE)
		    call imastr (out, "apformat", Memc[fmt])
		call sprintf (Memc[str1], SZ_LINE, "%d %d 1 1 %d")
		    call pargi (AP_ID(aps[i]))
		    call pargi (AP_BEAM(aps[i]))
		    call pargi (npts)
		call imastr (out, "APNUM1", Memc[str1])
		call imaddi (out, "BEAM-NUM", AP_BEAM(aps[i]))
		call sprintf (Memc[str1], SZ_LINE, "%7.2f %7.2f")
		    call pargr (AP_CEN(aps[i], apaxis))
		    call pargr (AP_CEN(aps[i], dispaxis))
		call imastr (out, "APCENTER", Memc[str1])
		call sprintf (Memc[str1], SZ_LINE, "%7.2f %7.2f")
		    call pargr (AP_LOW(aps[i], apaxis))
		    call pargr (AP_LOW(aps[i], dispaxis))
		call imastr (out, "APLOW", Memc[str1])
		call sprintf (Memc[str1], SZ_LINE, "%7.2f %7.2f")
		    call pargr (AP_HIGH(aps[i], apaxis))
		    call pargr (AP_HIGH(aps[i], dispaxis))
		call imastr (out, "APHIGH", Memc[str1])
		call ex_setdisp (out, dispaxis)

		call amovr (data[1,j], Memr[impl1r(out)], npts)
		call imunmap (out)
	    }

	    call sprintf (Memc[str], SZ_LINE,
		"APSUM   - Aperture %d from %s --> %s")
		call pargi (AP_ID(aps[i]))
		call pargstr (image)
		call pargstr (Memc[name])
	    call ap_log (Memc[str])
	    call ex_plot (gt, data[1,j], npts)
	    call gt_free (gt)
	}
end


# EX_SUMSKY -- Output sky spectrum

procedure ex_sumsky (image, sky, clobber, im, aps, naps, ap1, naps1, skydata,
	npts)

char	image[ARB]		# Input image name
char	sky[ARB]		# Output sky root name
int	clobber			# Clobber output images?
pointer	im			# Input IMIO pointer
pointer	aps[naps]		# Apertures
int	naps			# Number of apertures
int	ap1			# Starting aperture
int	naps1			# Number of apertures to output
real	skydata[npts, naps]	# Sky data
int	npts			# Number of points

int	i, j, k, format, apaxis, dispaxis
pointer	sp, str, str1, name, fmt, out

bool	clgetb()
int	clgwrd()
pointer	immap(), impl1r(), impl2r()
errchk	immap

begin
	# Check if sky output is desired.
	if (!clgetb ("apsum.skyextract"))
	    return

	# Allocate string and file name arrays.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (fmt, SZ_FNAME, TY_CHAR)

	format = clgwrd ("apio.format", Memc[fmt], SZ_LINE, FORMATS)

	# Write out the extracted spectra.
	do i = ap1, ap1+naps1-1 {
	    j = i - ap1 + 1
	    if (sky[1] == EOS) {
		call sprintf (Memc[str], SZ_LINE, "%ssky")
		    call pargstr (image)
	    } else
		call strcpy (sky, Memc[str], SZ_LINE)

	    switch (format) {
	    case ECHELLE:
	        call sprintf (Memc[name], SZ_FNAME, "%s.ec")
	            call pargstr (Memc[str])
	    case MULTISPEC:
	        call sprintf (Memc[name], SZ_FNAME, "%s.ms")
	            call pargstr (Memc[str])
	    default:
	        call sprintf (Memc[name], SZ_FNAME, "%s.%04d")
	            call pargstr (Memc[str])
	            call pargi (AP_ID(aps[i]))
	    }

	    # Output the image.
	    switch (format) {
	    case ECHELLE, MULTISPEC:
		if (i == 1) {
		    iferr (out = immap (Memc[name], NEW_COPY, im)) {
			call sprintf (Memc[str], SZ_LINE,
			    "Clobber existing output image %s?")
			    call pargstr (Memc[name])
			call xt_answer (Memc[str], clobber)
			if (clobber == YES || clobber == ALWAYSYES) {
			    call imdelete (Memc[name])
			    out = immap (Memc[name], NEW_COPY, im)
			} else {
			     call sfree (sp)
			     return
			}
		    }
		    apaxis = AP_AXIS(aps[i])
		    dispaxis = mod (apaxis, 2) + 1
		    IM_PIXTYPE(out) = TY_REAL
		    IM_LEN(out, 1) = IM_LEN(im, dispaxis)
		    IM_LEN(out, 2) = naps
		    call imastr (out, "apformat", Memc[fmt])
		    do k = 1, naps {
			call sprintf (Memc[str], SZ_LINE, "APNUM%d")
			    call pargi (k)
			call sprintf (Memc[str1], SZ_LINE, "%d %d 1 1 %d")
			    call pargi (AP_ID(aps[k]))
			    call pargi (AP_BEAM(aps[k]))
			    call pargi (npts)
			    call imastr (out, Memc[str], Memc[str1])
		    }
		    call ex_setdisp (out, dispaxis)
		} else
		    out = immap (Memc[name], READ_WRITE, 0)
		call amovr (skydata[1,j], Memr[impl2r(out,i)], npts)
		call imunmap (out)
			
	    default:
		iferr (out = immap (Memc[name], NEW_COPY, im)) {
		    call sprintf (Memc[str], SZ_LINE,
			"Clobber existing output image %s?")
			call pargstr (Memc[name])
		    call xt_answer (Memc[str], clobber)
		    if (clobber == YES || clobber == ALWAYSYES) {
			call imdelete (Memc[name])
			out = immap (Memc[name], NEW_COPY, im)
		    } else
			next
		}
		apaxis = AP_AXIS(aps[i])
		dispaxis = mod (apaxis, 2) + 1
		IM_PIXTYPE(out) = TY_REAL
		IM_NDIM(out) = 1
		IM_LEN(out, 1) = IM_LEN(im, dispaxis)
		call sprintf (Memc[str], SZ_LINE, "%s - Aperture %d")
		    call pargstr (IM_TITLE(out))
		    call pargi (AP_ID(aps[i]))
		call strcpy (Memc[str], IM_TITLE(out), SZ_IMTITLE)
		    call imastr (out, "apformat", Memc[fmt])
		call sprintf (Memc[str1], SZ_LINE, "%d %d 1 1 %d")
		    call pargi (AP_ID(aps[i]))
		    call pargi (AP_BEAM(aps[i]))
		    call pargi (npts)
		call imastr (out, "APNUM1", Memc[str1])
		call imaddi (out, "BEAM-NUM", AP_BEAM(aps[i]))
		call sprintf (Memc[str1], SZ_LINE, "%7.2f %7.2f")
		    call pargr (AP_CEN(aps[i], apaxis))
		    call pargr (AP_CEN(aps[i], dispaxis))
		call imastr (out, "APCENTER", Memc[str1])
		call sprintf (Memc[str1], SZ_LINE, "%7.2f %7.2f")
		    call pargr (AP_LOW(aps[i], apaxis))
		    call pargr (AP_LOW(aps[i], dispaxis))
		call imastr (out, "APLOW", Memc[str1])
		call sprintf (Memc[str1], SZ_LINE, "%7.2f %7.2f")
		    call pargr (AP_HIGH(aps[i], apaxis))
		    call pargr (AP_HIGH(aps[i], dispaxis))
		call imastr (out, "APHIGH", Memc[str1])
		call ex_setdisp (out, dispaxis)

		call amovr (skydata[1,j], Memr[impl1r(out)], npts)
		call imunmap (out)
	    }

	    call sprintf (Memc[str], SZ_LINE,
		"APSUM   - Sky for aperture %d from %s --> %s")
		call pargi (AP_ID(aps[i]))
		       call pargstr (image)
		call pargstr (Memc[name])
	    call ap_log (Memc[str])
	}
end


# EX_SETDISP -- Set dispersion axis parameters for extracted 1D spectra.

procedure ex_setdisp (im, dispaxis)

pointer	im		# Output IMIO pointer
int	dispaxis	# Input dispersion axis

real	crpix
real	crval
char	ctype[SZ_LINE]

real	imgetr()
errchk	imgetr, imgstr, imdelf, imaddi, imaddr, imastr

begin
	# The extracted image is one dimensional.
	call imaddi (im, "dispaxis", 1)

	# If the input dispersion axis is 2 then the header parameters must
	# be reset.
	if (dispaxis == 2) {
	    iferr {
	        crpix = imgetr (im, "crpix2")
	        call imdelf (im, "crpix2")
	        call imaddr (im, "crpix1", crpix)
	    } then
	        ;

	    iferr {
	        crval = imgetr (im, "crval2")
	        call imdelf (im, "crval2")
	        call imaddr (im, "crval1", crval)
	    } then
	        ;

	    iferr {
	        crval = imgetr (im, "cdelt2")
	        call imdelf (im, "cdelt2")
	        call imaddr (im, "cdelt1", crval)
	    } then
	        ;

	    iferr {
	        call imgstr (im, "ctype2", ctype, SZ_LINE)
	        call imdelf (im, "ctype2")
	        call imastr (im, "ctype1", ctype)
	    } then
	        ;

	    iferr {
	        call imgstr (im, "cunit2", ctype, SZ_LINE)
	        call imdelf (im, "cunit2")
	        call imastr (im, "cunit1", ctype)
	    } then
	        ;
	}
end
