include	"apertures.h"

# AP_FINDNEW -- Find and set new apertures automatically.  This task is
# called from the aperture editor so we don't want to read the image vector
# again.  It also differs from AP_FIND in that existing apertures are
# maintained and new apertures are added.

procedure ap_findnew (line, data, npts, apdef, aps, naps)

int	line			# Dispersion line of data
real	data[npts]		# Image data in which to find features
int	npts			# Number of pixels
pointer	apdef			# Default aperture pointer
pointer	aps[AP_MAXAPS]		# Aperture pointers
int	naps			# Number of apertures returned

int	i, nx, nfind
real	center, minsep
pointer	sp, x

bool	clgetb()
int	clgeti()
real	clgetr(), ap_center(), cveval()

begin
	# Determine the maximum number of apertures to be found and return
	# if that limit has been reached.
	nfind = min (clgeti ("apfind.nfind"), AP_MAXAPS)
	if (nfind <= naps)
	    return

	if (clgetb ("apio.verbose"))
	    call printf ("Finding apertures ...\n")

	# Set the positions of the currently defined apertures.
	call smark (sp)
	call salloc (x, max (nfind, naps), TY_REAL)
	nx = naps
	for (i = 1; i <= nx; i = i + 1)
	    Memr[x+i-1] = AP_CEN (aps[i], AP_AXIS(aps[i])) +
		cveval (AP_CV(aps[i]), real (line))

	# Find peaks not already identified.
	minsep = clgetr ("apfind.minsep")
	call find_peaks (data, npts, 0., 1, nfind, minsep, 0., Memr[x], nx)
	call asrtr (Memr[x+naps], Memr[x+naps], nx - naps)

	# Center on the new peaks and define new apertures.
	for (i = naps + 1; i <= nx; i = i + 1) {
	    center = Memr[x+i-1]
	    center = ap_center (center, data, npts)

	    if (!IS_INDEF(center)) {
		naps = naps + 1
		call ap_copy (apdef, aps[naps])

		if (naps > 1)
		    AP_ID(aps[naps]) = AP_ID(aps[naps - 1]) + 1

		AP_BEAM(aps[naps]) = AP_ID(aps[naps])
		AP_CEN(aps[naps], AP_AXIS(aps[naps])) = center -
		    cveval (AP_CV(aps[naps]), real (line))
	    }
	}
end
