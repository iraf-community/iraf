include <mach.h>
include <imhdr.h>
include "vt.h"

# PUTSQIB -- Murge a solar synoptic 'data only' image with a
# squibby brightness image.  Output image is separate image.

procedure t_putsqib()

char	image[SZ_FNAME]		# input image
char	sqibimage[SZ_FNAME]	# squibby brightness image
char	merged[SZ_FNAME]	# output merged image

int	i, numpix
pointer	im, ldatagp, lsqibgp, lpp,  sqibim, mim
pointer	immap(), imgl2s(), impl2s()
errchk	immap, imgl2s, impl2s

begin
	# Get parameters from the CL.
	call clgstr ("image", image, SZ_FNAME)
	call clgstr ("sqibimage", sqibimage, SZ_FNAME)
	call clgstr ("merged", merged, SZ_FNAME)

	# Open the two input images, see that they are the same size.
	im = immap (image, READ_ONLY, 0)
	sqibim = immap (sqibimage, READ_ONLY, 0)

	# If not, error.
	if (IM_LEN(im,2) != IM_LEN(sqibim,2))
	    call error(0,"sizes of data image and sqib image must match")

	if (IM_LEN(im,1) != IM_LEN(sqibim,1))
	    call error(0,"sizes of data image and sqib image must match")

	# Open the new image.
	mim = immap (merged, NEW_COPY, im)

	do i = 1, IM_LEN(im,2) {
	    ldatagp = imgl2s (im, i)
	    lsqibgp = imgl2s (sqibim, i)
	    lpp = impl2s (mim, i)
	    numpix = IM_LEN(im,1)
	    call sqibput (Mems[ldatagp], Mems[lsqibgp], Mems[lpp], numpix)
	}

	# Unmap images.
	call imunmap (im)
	call imunmap (sqibim)
	call imunmap (mim)
end


# SQIBPUT -- pack squibby brightness from line2 into line1 and put the
# result into line3.

procedure sqibput (line1, line2, line3, numpix)

short	line1[numpix]		# data line
short	line2[numpix]		# sqib line
short	line3[numpix]		# out line
int	numpix			# number of pixels

int	i

begin
	do i = 1, numpix
	    line3[i] = line1[i]*16 + line2[i]
end
