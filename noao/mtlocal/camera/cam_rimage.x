include	<imhdr.h>
include <mach.h>
include	"rcamera.h"

# CAM_READ_IMAGE --  Read CAMERA image pixels to IRAF image file.

procedure cam_read_image (cam_fd, im)

int	cam_fd
pointer	im

int	i, nlines
short	linemin, linemax
pointer	buf
long	v[IM_MAXDIM]

int	cam_init_read_pixels(), cam_read_pixels(), impnls()
long	clktime()
errchk	impnls, init_read_pixels(), read_pixels()

include	"rcamera.com"

begin
	call cam_set_image_header (im)

	if (NAXIS(im) == 0)
	    return

	IRAFMAX(im) = -MAX_REAL
	IRAFMIN(im) = MAX_REAL

	call amovkl (long(1), v, IM_MAXDIM)

	nlines = PARAM6(im)

	# CAMERA data is converted to type SHORT.
	i= cam_init_read_pixels (len_record, BITPIX, TY_SHORT)

	do i = 1, nlines {
	    if (impnls (im, buf, v) == EOF)
		call error (3, "Error writing CAMERA data")
	    if (cam_read_pixels (cam_fd, Mems[buf], PARAM5(im)) !=
		PARAM5(im))
		call error (4, "Error reading CAMERA data")
	    call alims (Mems[buf], PARAM5(im), linemin, linemax)
	    IRAFMAX(im) = max (IRAFMAX(im), real (linemax))
	    IRAFMIN(im) = min (IRAFMIN(im), real (linemin))
	}

	LIMTIME(im) = clktime (long (0))
end


# CAM_SET_IMAGE_HEADER -- Set remaining header fields not set in read_header.

procedure cam_set_image_header (im)

pointer	im

include	"rcamera.com"

begin
	# Set IRAF image pixel type
	if (data_type == ERR) {
	    if (BITPIX <= SZ_SHORT * SZB_CHAR * NBITS_BYTE)
		PIXTYPE(im) = TY_SHORT
	    else
		PIXTYPE(im) = TY_LONG
	} else
	    PIXTYPE(im) = data_type
end
