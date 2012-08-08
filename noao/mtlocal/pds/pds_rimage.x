
include	<imhdr.h>
include <mach.h>
include	"rpds.h"


# PDS_READ_IMAGE --  Read PDS image pixels to IRAF image file

procedure pds_read_image (pds_fd, im, parameters)

int	pds_fd
pointer	im
long	parameters[LEN_PAR_ARRAY]

short	linemin, linemax
int	i, nlines
pointer	buf
long	v[IM_MAXDIM]

int	pds_init_read_scan(), pds_read_scan(), impnls()
long	clktime()

errchk	impnls, pds_init_read_scan, pds_read_scan

include	"rpds.com"

begin
	call pds_set_image_header (im)

	if (NAXIS(im) == 0)
	    return

	IRAFMAX(im) = -MAX_REAL
	IRAFMIN(im) = MAX_REAL

	call amovkl (long(1), v, IM_MAXDIM)

	nlines = NLINES(im)

	# PDS data is converted to type SHORT.
	i= pds_init_read_scan (parameters)

	do i = 1, nlines {
	    if (impnls (im, buf, v) == EOF)
		call error (3, "Error writing PDS data")
	    if (pds_read_scan (pds_fd, Mems[buf]) != NCOLS(im))
		call error (4, "Error reading PDS data")
	    call alims (Mems[buf], NCOLS(im), linemin, linemax)
	    IRAFMAX(im) = max (IRAFMAX(im), real (linemax))
	    IRAFMIN(im) = min (IRAFMIN(im), real (linemin))
	}

	LIMTIME(im) = clktime (long(0))
end


# PDS_SET_IMAGE_HEADER -- Set remaining header fields not set in read_header.

procedure pds_set_image_header (im)

pointer	im

include	"rpds.com"

begin
	# Set IRAF image pixel type
	if (data_type == ERR) {
	    if (BITPIX <= SZ_SHORT * SZB_CHAR * NBITS_BYTE)
		IM_PIXTYPE(im) = TY_SHORT
	    else
		IM_PIXTYPE(im) = TY_LONG
	} else
	    IM_PIXTYPE(im) = data_type
end
