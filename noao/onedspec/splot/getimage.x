include	<imhdr.h>
include "../idsmtn.h"

# GETIMAGE -- Read new image pixels.

procedure getimage (image, nline, wave_scl, im, ids, gt, pix, npts, x1, x2, dx)

char	image[ARB]
int	nline
bool	wave_scl
pointer	im, ids, gt, pix
int	npts
real	x1, x2, dx

int	temp

int	clgeti()
pointer	immap(), imgl1r(), imgl2r()
errchk	immap

begin
	# Map the image.
	im = immap (image, READ_ONLY, 0)

	# Get header info.
	nline = 0
	if (IM_NDIM(im) > 1) {
	    if (IM_LEN(im,2) > 1)
	        nline = max (1, min (IM_LEN(im,2), clgeti ("line")))
	    else
		nline = 1
	}
	call load_ids_hdr (ids, im, nline)

	# Point beginning of data array to first plottable point
	# Note that NP1 is the OFFSET into the data array and
	# NP2 - NP1 is the number of data elements. Presumably this
	# derives from the FORTH indices on a DO LOOP construct:
	#   NP2 NP1 DO ... LOOP

	if (NP1(ids) > NP2(ids)) {
	    temp = NP1(ids)
	    NP1(ids) = NP2(ids)
	    NP2(ids) = temp
	}
	npts = min (NP2(ids), IM_LEN (im,1)) - NP1(ids)

	# Get the pixel data.  Do not use the IMIO buffer because the
	# pixel data may be changed.

	call mfree (pix, TY_REAL)
	call malloc (pix, npts, TY_REAL)
	nline = LINE(ids)
	if (nline > 0)
	    call amovr (Memr[imgl2r(im,nline)+NP1(ids)], Memr[pix], npts)
	else
	    call amovr (Memr[imgl1r(im)+NP1(ids)], Memr[pix], npts)

	# Use wavelength coordinates if possible
	if (!IS_INDEF (W0(ids)))
	    x1 = W0(ids)
	else
	    x1 = 1.0

	if (!IS_INDEF (WPC(ids)))
	    dx = WPC(ids)
	else
	    dx = 1.0

	# Adjust beginning wavelength according to starting good pixel
	x1 = x1 + NP1(ids) * dx
	x2 = x1 + (npts-1) * dx

	# If user has forced a channel scale, re-adjust the x-axis
	if (!wave_scl) {
	    dx = 1.0
	    x1 = max (1.0, NP1(ids)+1.0)
	    x2 = x1 + (npts-1) * dx
	}

	# Make a title.
	call mktitle (image, nline, im, ids, gt)
end
