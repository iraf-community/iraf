include	<imhdr.h>
include	<pkg/xtanswer.h>
include	"apertures.h"

# AP_FIND -- Find and set apertures automatically.

procedure ap_find (image, line, nsum, find, dbwrite, aps, naps)

char	image[SZ_FNAME]		# Image name
int	line			# Image dispersion line
int	nsum			# Number of dispersion lines to sum
int	find			# Find apertures?
int	dbwrite			# Write apertures to database?

pointer	aps[AP_MAXAPS]		# Aperture pointers
int	naps			# Number of apertures

real	minsep, center
int	i, npts, apaxis, nfind, nx
pointer	im, imdata, title, sp, str, x

bool	clgetb()
int	clgeti()
real	clgetr(), ap_center(), cveval()

errchk	ap_getdata

begin
	# Find apertures only if there are no other apertures defined.
	if (naps != 0)
	     return

	# Query the user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Find apertures for %s?")
	    call pargstr (image)
	call xt_answer (Memc[str], find)
	call sfree (sp)
	if ((find == NO) || (find == ALWAYSNO))
	    return

	# Get CL parameters.
	nfind = min (clgeti ("apfind.nfind"), AP_MAXAPS)
	if (nfind == 0)
	    return
	minsep = clgetr ("apfind.minsep")

	# Progress information.
	if (clgetb ("apio.verbose"))
	    call printf ("Finding apertures ...\n")

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	# Allocate working memory.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, nfind, TY_REAL)

	# Find the peaks.
	nx = 0
	call find_peaks (Memr[imdata], npts, 0., 1, nfind, minsep, 0., Memr[x],
	    nx)
	call asrtr (Memr[x], Memr[x], nx)

	# Center on the peaks and define default apertures..
	naps = 0
	for (i = 1; i <= nx; i = i + 1) {
	    center = Memr[x+i-1]
	    center = ap_center (center, Memr[imdata], npts)

	    if (!IS_INDEF(center)) {
		naps = naps + 1
		if (naps == 1)
		    call ap_default (im, 1, 1, apaxis, INDEFR, real (line),
			aps[naps])
		else
		    call ap_copy (aps[1], aps[naps])

		AP_ID(aps[naps]) = naps
		AP_BEAM(aps[naps]) = naps
		AP_CEN(aps[naps], AP_AXIS(aps[naps])) = center -
		    cveval (AP_CV(aps[naps]), real (line))
	    }
	}

	# Log the apertures found and write them to the database.
	call sprintf (Memc[str], SZ_LINE,
	    "APFIND  - %d apertures found for %s.")
	    call pargi (naps)
	    call pargstr (image)
	call ap_log (Memc[str])

	call ap_dbwrite (image, dbwrite, aps, naps)

	# Free memory and unmap the image.
	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call sfree (sp)
end
