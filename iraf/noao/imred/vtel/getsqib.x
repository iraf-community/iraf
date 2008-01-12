include <mach.h>
include <imhdr.h>
include "vt.h"

# GETSQIB -- Make a new image from a solar synoptic image containing just
# the squibby brightness.

procedure t_getsqib()

char	image[SZ_FNAME]		# input image
char	sqibimage[SZ_FNAME]	# output squibby brightness image

int	i, numpix
pointer	im, lgp, lpp, sqibim

pointer	immap(), imgl2s(), impl2s()
errchk	immap, imgl2s, impl2s

begin
	# Get parameters from the CL.
	call clgstr ("image", image, SZ_FNAME)
	call clgstr ("sqibimage", sqibimage, SZ_FNAME)

	# Open image.
	im = immap (image, READ_ONLY, 0)
	sqibim = immap (sqibimage, NEW_COPY, im)

	numpix = IM_LEN(im,1)
	do i = 1, IM_LEN(im,2) {
	    lgp = imgl2s (im, i)
	    lpp = impl2s (sqibim, i)
	    call sqibline (Mems[lgp], Mems[lpp], numpix)
	}

	# Unmap images.
	call imunmap (im)
	call imunmap (sqibim)
end


# SQIBLINE -- Unpack squibby brightness from line1 and put it into line2.

procedure sqibline (line1, line2, numpix)

short	line1[numpix]	# input image line
short	line2[numpix]	# output image line
int	numpix		# number of pixels in line

int	i
int	and()

begin
	do i = 1, numpix
	    line2[i] = and(int(line1[i]),17B)
end
