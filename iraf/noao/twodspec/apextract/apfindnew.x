include	<mach.h>
include	"apertures.h"

# Sort flags
define	ORDER	"|increasing|decreasing|"

# AP_FINDNEW -- Find and set new apertures automatically.  This task is
# called from the aperture editor so we don't want to read the image vector
# again.  It also differs from AP_FIND in that existing apertures are
# maintained and new apertures are added.

procedure ap_findnew (line, data, npts, apdef, aps, naps)

int	line			# Dispersion line of data
real	data[npts]		# Image data in which to find features
int	npts			# Number of pixels
pointer	apdef			# Default aperture pointer
pointer	aps			# Aperture pointers
int	naps			# Number of apertures returned

int	i, j, nx, nfind
real	center, minsep
pointer	sp, str, x, ids

bool	clgetb()
int	apgeti(), apgwrd()
real	apgetr(), ap_center(), ap_cveval()

begin
	# Determine the maximum number of apertures to be found and return
	# if that limit has been reached.
	nfind = apgeti ("nfind")
	if (nfind <= naps)
	    return

	if (clgetb ("verbose"))
	    call printf ("Finding apertures ...\n")

	# Set the positions of the currently defined apertures.
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (x, max (nfind, naps), TY_REAL)
	nx = naps
	for (i = 0; i < nx; i = i + 1)
	    Memr[x+i] = AP_CEN (Memi[aps+i], AP_AXIS(Memi[aps+i])) +
		ap_cveval (AP_CV(Memi[aps+i]), real (line))

	# Find peaks not already identified.
	minsep = apgetr ("minsep")
	#call find_peaks (data, npts, 0., 1, nfind, minsep, 0., Memr[x], nx)
	call find_peaks (data, npts, 0., 1, nfind, minsep, -MAX_REAL,
	    Memr[x], nx)
	call asrtr (Memr[x+naps], Memr[x+naps], nx - naps)

	# Center on the new peaks and define new apertures.
	for (i = naps + 1; i <= nx; i = i + 1) {
	    center = Memr[x+i-1]
	    center = ap_center (center, data, npts)

	    if (!IS_INDEF(center)) {
		if (mod (naps, 100) == 0)
		    call realloc (aps, naps+100, TY_POINTER)

		call ap_copy (apdef, Memi[aps+naps])

		AP_ID(Memi[aps+naps]) = INDEFI
		if (AP_TITLE(Memi[aps+naps]) != NULL)
		    call mfree (AP_TITLE(Memi[aps+naps]), TY_CHAR)
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
	call sfree (sp)
end
