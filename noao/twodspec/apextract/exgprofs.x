include	"exio.h"
include	"extract.h"
include	"apertures.h"

# EX_GPROFS -- Read image data and extract profiles.
#
# The data profiles are either centered in the extracted profile array or
# at the point given by pcen.  If background subtracting compute and
# subtract the background from the profiles and return the subtracted
# background.

procedure ex_gprofs (ex, im, line, aps, naps, pstart, pcen, pend, cen, prof,
	bkg)

pointer	ex				# Pointer to extraction parameters
pointer	im				# EXIO pointer
int	line				# Image line
pointer	aps[naps]			# Apertures
int	naps				# Number apertures
int	pstart[naps]			# Starting profile index
real	pcen[naps]			# Profile center index
int	pend[naps]			# Ending profile index
int	cen				# Center profiles?
real	prof[ARB]			# Profiles
real	bkg[ARB]			# Background

int	i, aaxis, npts, c1, c2, nc
real	center, low, high, cveval()
pointer	data, ex_g2r()
errchk	ex_g2r

begin
	aaxis = EX_AAXIS(im)
	npts = EX_ALEN(im)
	data = ex_g2r (im, line)

	do i = 1, naps {
	    if (pstart[i] == 0)
		next

	    # Set aperture limits.
	    c1 = pstart[i]
	    c2 = pend[i]
	    nc = c2 - c1 + 1

	    center = AP_CEN(aps[i],aaxis) + cveval (AP_CV(aps[i]), real (line))
	    if (cen == YES)
	        low = center - (c2 - c1) / 2.
	    else
	        low = center - (pcen[i] - pstart[i])
	    high = low + nc - 1

	    call ex_apstrip (Memr[data], npts, low, center, high, EX_ASI(ex),
		AP_IC(aps[i]), EX_BKGD(ex), prof[c1], bkg[c1], nc)
	}
end
