include	<imhdr.h>
include	<pkg/xtanswer.h>

# TR_TRACE -- Trace features in a two dimensional image.
#
# Given an image pointer, the starting dispersion position, and a set
# of apertures defining the centers of features, trace the feature
# centers to other dispersion positions and fit a curve to the positions.
# The user specifies the dispersion step size, the number of dispersion
# lines to sum, and parameters for the feature centering function
# fitting.

procedure tr_trace (image, start, trace, fittrace, dbwrite, aps, naps)

char	image[SZ_FNAME]		# Image name
int	start			# Starting dispersion position
int	trace			# Trace aperture?
int	fittrace		# Fit traces interactively?
int	dbwrite			# Write apertures to database?

pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

int	step			# Tracing step
int	nsum			# Number of dispersion lines to sum
real	cradius			# Centering radius
real	cwidth			# Centering width
real	cthreshold		# Detection threshold for centering

int	interactive, dispaxis, apaxis
real	center
pointer	im, ic, sp, str

bool	clgetb()
int	clgeti(), imgeti()
real	clgetr()
pointer	ap_immap()

errchk	ap_immap, ic_open, tr_ltrace, tr_ctrace, imgeti

begin
	# Query the user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Trace apertures for %s?")
	    call pargstr (image)
	call xt_answer (Memc[str], trace)
	if ((trace == NO) || (trace == ALWAYSNO)) {
	    call sfree (sp)
	    return
	}

	# Query the user.
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE,
	    "Fit traced positions for %s interactively?")
	    call pargstr (image)
	call xt_answer (Memc[str], fittrace)

	if (clgetb ("apio.verbose"))
	    call printf ("Tracing apertures ...\n")

	interactive = fittrace
	if (interactive == NO)
	    interactive = ALWAYSNO

	im = ap_immap (image)
	dispaxis = imgeti (im, "dispaxis")
	apaxis = mod (dispaxis, 2) + 1

	# If no apertures are defined default to the center of the image.
	if (naps == 0) {
	    naps = 1
	    center = IM_LEN (im, apaxis) / 2.
	    call ap_default (im, 1, 1, apaxis, center, real (start), aps[naps])
	    call sprintf (Memc[str], SZ_LINE,
	       "APTRACE - Default aperture defined centered on %s.")
	       call pargstr (image)
	    call ap_log (Memc[str])
	}

	# Centering parameters
	cradius = clgetr ("apedit.radius")
	cwidth = clgetr ("apedit.width")
	cthreshold = clgetr ("apedit.threshold")

	switch (dispaxis) {
	case 1:
	    call tr_ctrace (image, im, ic, start, step, nsum, cradius,
		cwidth, cthreshold, interactive, aps, naps)
	case 2:
	    call tr_ltrace (image, im, ic, start, step, nsum, cradius,
		cwidth, cthreshold, interactive, aps, naps)
	}

	# Log the tracing and write the traced apertures to the database.
	call sprintf (Memc[str], SZ_LINE,
	    "APTRACE - %d apertures traced in %s.")
       	    call pargi (naps)
       	    call pargstr (image)
	call ap_log (Memc[str])

	call ap_dbwrite (image, dbwrite, aps, naps)

	call sfree (sp)
	call imunmap (im)
	return

# Initialize the tracing parameters and fitting parameters.

entry	tr_init ()

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Tracing parameters
	step = clgeti ("aptrace.step")
	nsum = clgeti ("aptrace.nsum")

	# Trace fitting parameters
	call ic_open (ic)
	call clgstr ("aptrace.function", Memc[str], SZ_LINE)
	call ic_pstr (ic, "function", Memc[str])
	call ic_puti (ic, "order", clgeti ("aptrace.order"))
	call clgstr ("aptrace.sample", Memc[str], SZ_LINE)
	call ic_pstr (ic, "sample", Memc[str])
	call ic_puti (ic, "naverage", clgeti ("aptrace.naverage"))
	call ic_puti (ic, "niterate", clgeti ("aptrace.niterate"))
	call ic_putr (ic, "low", clgetr ("aptrace.low_reject"))
	call ic_putr (ic, "high", clgetr ("aptrace.high_reject"))
	call ic_putr (ic, "grow", clgetr ("aptrace.grow"))

	call sfree (sp)
	return

entry	tr_done ()

	call ic_closer (ic)
end
