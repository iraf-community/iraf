include	<pkg/xtanswer.h>
include	"apertures.h"

# AP_RECENTER -- Recenter apertures using the CENTER1D algorithm.

procedure ap_recenter (image, line, nsum, recenter, dbwrite, aps, naps)

char	image[SZ_FNAME]		# Image name
int	line			# Image dispersion line
int	nsum			# Number of dispersion lines to sum
int	recenter		# Recenter apertures?
int	dbwrite			# Write apertures to database?

pointer	aps[AP_MAXAPS]		# Aperture pointers
int	naps			# Number of apertures

real	center
int	i, npts, apaxis
pointer	sp, str, im, imdata, title

bool	clgetb()
real	ap_center(), cveval()
errchk	ap_getdata

begin
	# Check if apertures are defined.
	if (naps == 0)
	     return

	# Query the user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Recenter apertures for %s?")
	    call pargstr (image)
	call xt_answer (Memc[str], recenter)
	if ((recenter == NO) || (recenter == ALWAYSNO)) {
	    call sfree (sp)
	    return
	}

	# Progress information.
	if (clgetb ("apio.verbose"))
	    call printf ("Recentering apertures ...\n")

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	# Recenter the apertures.
	do i = 1, naps {
	    center = AP_CEN(aps[i], AP_AXIS(aps[i])) +
		cveval (AP_CV(aps[i]), real (line))
	    center = ap_center (center, Memr[imdata], npts)
	    if (!IS_INDEF(center))
		AP_CEN(aps[i], AP_AXIS(aps[i])) = center -
		    cveval (AP_CV(aps[i]), real (line))
	}

	# Log the operation, write the apertures to the database,
	# unmap the image and free memory.
	call sprintf (Memc[str], SZ_LINE,
	    "APRECENTER  - %d apertures recentered for %s.")
	    call pargi (naps)
	    call pargstr (image)
	call ap_log (Memc[str])

	call ap_dbwrite (image, dbwrite, aps, naps)

	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call sfree (sp)
end
