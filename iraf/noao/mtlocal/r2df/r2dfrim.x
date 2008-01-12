include	<imhdr.h>
include <mach.h>
include	"r2df.h"

# R2DFRIM -- Read 2D-FRUTTI image pixels to IRAF image file.

procedure r2dfrim (cam_fd, im)

int	cam_fd
pointer	im

pointer	buf
int	i, nlines
short	linemin, linemax
long	v[IM_MAXDIM]

long	clktime()
int	r2dfin_pixel(), r2dfrpix(), impnls()
errchk	impnls, init_read_pixels(), read_pixels()
include	"r2df.com"

begin
	call r2dfset_im_hdr (im)

	if (NAXIS(im) == 0)
	    return

	IRAFMAX(im) = -MAX_REAL
	IRAFMIN(im) = MAX_REAL

	call amovkl (long(1), v, IM_MAXDIM)
	nlines = PARAM6(im)

	# 2D-FRUTTI data is converted to type SHORT.

	i= r2dfin_pixel (len_record, BITPIX, TY_SHORT)

	do i = 1, nlines {
	    if (impnls (im, buf, v) == EOF)
		call error (3, "Error writing 2D-FRUTTI data")
	    if (r2dfrpix (cam_fd, Mems[buf], PARAM5(im)) !=
		PARAM5(im))
		call error (4, "Error reading 2D-FRUTTI data")
	    call alims (Mems[buf], PARAM5(im), linemin, linemax)
	    IRAFMAX(im) = max (IRAFMAX(im), real (linemax))
	    IRAFMIN(im) = min (IRAFMIN(im), real (linemin))
	}

	LIMTIME(im) = clktime (long (0))
end


# R2DFSET_IM_HDR -- Set remaining header fields not set in read_header.

procedure r2dfset_im_hdr (im)

pointer	im
include	"r2df.com"

begin
	# Set IRAF image pixel type.

	if (data_type == ERR) {
	    if (BITPIX <= SZ_SHORT * SZB_CHAR * NBITS_BYTE)
		PIXTYPE(im) = TY_SHORT
	    else
		PIXTYPE(im) = TY_LONG
	} else
	    PIXTYPE(im) = data_type
end
