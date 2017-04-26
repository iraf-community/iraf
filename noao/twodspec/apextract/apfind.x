include	<imhdr.h>
include	<mach.h>
include	"apertures.h"

# Sort flags
define	ORDER	"|increasing|decreasing|"

# AP_FIND -- Find and set apertures automatically.

procedure ap_find (image, line, nsum, aps, naps)

char	image[SZ_FNAME]		# Image name
int	line			# Image dispersion line
int	nsum			# Number of dispersion lines to sum
pointer	aps			# Aperture pointers
int	naps			# Number of apertures

real	minsep, center
int	i, j, npts, apaxis, nfind, nx
pointer	im, imdata, title, sp, str, x, ids

bool	clgetb(), ap_answer()
int	apgeti(), apgwrd()
real	apgetr(), ap_center(), ap_cveval()

errchk	ap_getdata, ap_default

begin
	# Find apertures only if there are no other apertures defined.
	if (naps != 0)
	     return

	# Query user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Find apertures for %s?")
            call pargstr (image)
	if (!ap_answer ("ansfind", Memc[str])) {
	    call sfree (sp)
	    return
	}

	if (clgetb ("verbose"))
	    call printf ("Finding apertures ...\n")

	# Get CL parameters.
	nfind = apgeti ("nfind")
	if (nfind == 0)
	    return
	minsep = apgetr ("minsep")

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	# If nfind > 0 find the peaks.  Otherwise divide the image evenly
	# into apertures.

	if (nfind > 0) {
	    # Allocate working memory.
	    call salloc (x, nfind+2, TY_REAL)

	    # Find the peaks.
	    nx = 0
	    call find_peaks (Memr[imdata], npts, 0., 1, nfind+2, minsep,
		-MAX_REAL, Memr[x], nx)
	    #call find_peaks (Memr[imdata], npts, 0., 1, nfind+2, minsep,
	    #	0, Memr[x], nx)
	    #call asrtr (Memr[x], Memr[x], nx)

	    # Center on the peaks.
	    naps = 0
	    for (i = 1; i <= nx && naps < nfind; i = i + 1) {
		center = Memr[x+i-1]
		center = ap_center (center, Memr[imdata], npts)

		if (!IS_INDEF(center)) {
		    if (mod (naps, 100) == 0)
			call realloc (aps, naps+100, TY_POINTER)
		    if (naps == 0)
			call ap_default (im, INDEFI, 1, apaxis, INDEFR,
			    real (line), Memi[aps+naps])
		    else
			call ap_copy (Memi[aps], Memi[aps+naps])

		    AP_CEN(Memi[aps+naps], AP_AXIS(Memi[aps+naps])) = center -
			ap_cveval (AP_CV(Memi[aps+naps]), real (line))
		    naps = naps + 1
		}
	    }

	} else {
	    nfind = abs (nfind)
	    minsep = real (npts) / nfind
	    naps = 0
	    do i = 1, nfind {
		if (mod (naps, 100) == 0)
		    call realloc (aps, naps+100, TY_POINTER)
		center = (i - 0.5) * minsep
		IF (naps == 0)
		    call ap_default (im, INDEFI, 1, apaxis, INDEFR,
			real (line), Memi[aps+naps])
		else
		    call ap_copy (Memi[aps], Memi[aps+naps])

		AP_CEN(Memi[aps+naps], AP_AXIS(Memi[aps+naps])) = center -
		    ap_cveval (AP_CV(Memi[aps+naps]), real (line))
		naps = naps + 1
	    }
	}
	    
	# Set the aperture ID's
	i = apgwrd ("order", Memc[str], SZ_LINE, ORDER)
	call ap_sort (j, Memi[aps], naps, i)
	call ap_gids (ids)
	call ap_ids (Memi[aps], naps, ids)
	call ap_titles (Memi[aps], naps, ids)
	call ap_fids (ids)

	# Log the apertures found and write them to the database.
	call sprintf (Memc[str], SZ_LINE, "FIND - %d apertures found for %s")
	    call pargi (naps)
	    call pargstr (image)
	call ap_log (Memc[str], YES, YES, NO)

	call appstr ("ansdbwrite1", "yes")

	# Free memory and unmap the image.
	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call sfree (sp)
end
