include <mach.h>
include <imhdr.h>
include "vt.h"

# IMFGLEXR -- IMFilt Get Line with EXtension Real.  Get a line from a
# full disk solar image and extend the boundary appropriately for use
# with acnvr.  All pixels outside the limb are set equal to the value
# of the last pixel inside the limb.  The line is extended in size by
# an amount given by 'extension' beyond the solar disk width.

pointer procedure imfglexr (imptr, linenumber, el, extension)

int	linenumber			# Line of input image to get
int	extension			# Amount of boundary extension needed
real	el[LEN_ELSTRUCT]		# limb ellipse structure
pointer	imptr	   	 		# Input image pointer

pointer	rlptr, sp, tmpptr
real	p, n
int	lpix1, lpix2
int	linelength
int	lexb, rexb, i
short	k

pointer	imgl2r()
short	shifts()
errchk	imgl2r

begin
	k = -4

	# Calculate the left and right bounds of the extended data.
	lexb = E_XCENTER[el] - E_XSEMIDIAMETER[el] - extension
	rexb = E_XCENTER[el] + E_XSEMIDIAMETER[el] + extension

	# Extend 10 extra pixels beyond the minimum.
	lexb = lexb - 10
	rexb = rexb + 10
	linelength = IM_LEN(imptr,1)

	# Make a temporary short buffer for stripping.
	call smark (sp)
	call salloc (tmpptr, linelength, TY_SHORT)

	# Get a line in the normal way. Point the real pointer to it.
	rlptr = imgl2r (imptr, linenumber)

	# Copy the line into the short array for stripping.
	do i = 1, linelength
	    Mems[tmpptr+i-1] = short(Memr[rlptr+i-1])

	# Strip off the squibby brightness. Put back into real array.
	do i = 1, linelength
	    Memr[rlptr+i-1] = real(shifts(Mems[tmpptr+i-1], k))

	# If the whole line is off the limb, return NULL.
	if (abs(linenumber - E_YCENTER[el]) >= E_YSEMIDIAMETER[el])
	    return(NULL)

	# Use ellipse parameters to determine where the limb intersections are.
	p = (real(linenumber) - E_YCENTER[el])**2/E_YSEMIDIAMETER[el]**2
	n = (1.0 - p) * E_XSEMIDIAMETER[el]**2

	# The two limb points are:
	lpix1 = int(-sqrt(abs(n)) + .5) + E_XCENTER[el]
	lpix2 = int(sqrt(abs(n)) + .5) + E_XCENTER[el]

	# Extend the boundary of the data  beyond the limb
	# by duplicating the last inside_the_limb pixel.  This extension
	# is done out to lexb on the left and rexb on the right.

	call amovkr (Memr[rlptr+lpix1+1], Memr[rlptr+lexb], lpix1-1-lexb)
	call amovkr (Memr[rlptr+lpix2-1], Memr[rlptr+lpix2+1], rexb-1-lpix2)
	call sfree (sp)
	return (rlptr)
end
