include <mach.h>
include <imhdr.h>
include "vt.h"

# TRIM -- Trim a full disk image using the squibby brightness template.
# Leave all the squibby brightness information intact, set data outside the
# limb to zero.

procedure t_trim()

char	image[SZ_FNAME]		# image to trim
int	threshold		# squibby brightness threshold defining limb

int	i, numpix
pointer	im, lgp, lpp
pointer	immap(), imgl2s(), impl2s()
int	clgeti()
errchk	immap, imgl2s, impl2s

begin
	# Get parameters from the CL.
	call clgstr ("image", image, SZ_FNAME)
	threshold = clgeti("threshold")

	# Open image.
	im = immap (image, READ_WRITE, 0)

	do i = 1, IM_LEN(im,2) {
	    lgp = imgl2s (im, i)
	    lpp = impl2s (im, i)
	    numpix = IM_LEN(im,1)
	    call trimline (Mems[lgp], Mems[lpp], numpix, threshold)
	}

	# Unmap image.
	call imunmap (im)
end


# TRIMLINE -- trim line1 and put it into line2.

procedure trimline (line1, line2, numpix, threshold)

short	line1[numpix]		# input line
short	line2[numpix]		# output line
int	numpix			# number of pixels in this line
int	threshold		# squibby brightness threshold

int	i, left, right

begin
	left = 0
	right = 0

	do i = 1, numpix {
	    if (and(int(line1[i]),17B) >= threshold) {
	        left = i
	        break
	    } else
		line2[i] = and(int(line1[i]),17B)
	}

	if (left != 0)
	    do i = numpix, 1, -1 {
	        if(and(int(line1[i]),17B) >= threshold) {
	            right = i
	            break
	        } else
		    line2[i] = and(int(line1[i]),17B)
	    }

	if (left != 0 && right != 0 && left < right)
	    do i = left, right
		line2[i] = line1[i]
end
