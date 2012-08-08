# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"stf.h"

define	NBITS_CHAR	(SZB_CHAR * NBITS_BYTE)


# STF_NEWIMAGE -- Set up the IMIO/STF descriptor for an image opened with mode
# new_image or new_copy of non-STF images.  Note that the parameters GROUP 
# and GCOUNT were set earlier by stf_open().

procedure stf_newimage (im)

pointer	im			# image descriptor

pointer	stf
pointer	o_im
long	totpix
char	pname[SZ_KEYWORD]
int	old_kernel, pixtype, bitpix, nbytes, pno, ndim, i, j
errchk	stf_addpar
string	zero "0"
string	one  "1"

include <szpixtype.inc>

begin
	# Get length of axes and datatype from imio descriptor;
	# these may be changed by the user between image mapping 
	# and first i/o to pixfile so we set up the group parameter block
	# and size of pixfile on first i/o operation

	stf = IM_KDES(im)
	o_im = IM_OHDR(im)
	ndim = IM_NDIM(im)
	STF_NAXIS(stf) = ndim
	do i = 1, ndim
	    STF_LENAXIS(stf,i) = IM_LEN(im,i)

	# Get datatype for the pixfile; stf_open has set this to datatype 
	# of template file if it exists, otherwise defaults to real(assuming
	# the user hasn't changed it by now)

	pixtype = IM_PIXTYPE(im)

	bitpix = pix_size[pixtype] * NBITS_CHAR
	nbytes = bitpix / NBITS_BYTE

	call sprintf (STF_DATATYPE(stf), SZ_DATATYPE, "%s*%d")
	    switch (pixtype) {
	    case TY_USHORT:
		call pargstr ("UNSIGNED")
	    case TY_SHORT, TY_LONG, TY_INT:
		call pargstr ("INTEGER")
	    case TY_REAL, TY_DOUBLE:
		call pargstr ("REAL")
	    case TY_COMPLEX:
		call pargstr ("COMPLEX")
	    default:
		pixtype = TY_REAL
		bitpix = SZ_REAL * NBITS_CHAR
		nbytes = bitpix / NBITS_BYTE
		call pargstr ("REAL")
	    }
	    call pargi (nbytes)

	STF_BITPIX(stf) = bitpix

	# Set the IMIO min/max fields.

	IM_MIN(im) = 0.
	IM_MAX(im) = 0.
	IM_LIMTIME(im) = 0

	# For a new copy image(of an already-existing file), DO NOT add group
	# parameters to the GPB, unless the original image is not an STF
	# image.  The following are the "standard" set of datamin/max and the
	# FITS coordinate parms which SDAS files are supposed to have.

	if (IM_ACMODE(im) == NEW_COPY && o_im != NULL)
	    old_kernel = IM_KERNEL(o_im)

	if ((IM_ACMODE(im) == NEW_FILE) || 
	   ((IM_ACMODE(im) == NEW_COPY) && IM_KERNEL(im) != old_kernel)) {

	    # Set up the standard STF group parameter block parameters.
	    STF_GROUPS(stf) = YES
	    STF_PCOUNT(stf) = 2 + (ndim * 3) + (ndim * ndim)
	    STF_PSIZE(stf) = 2 * (SZ_REAL * NBITS_CHAR) +	# DATAMIN/MAX
		ndim * (SZ_DOUBLE * NBITS_CHAR) +		# CRVALn
		ndim * (SZ_REAL * NBITS_CHAR) +			# CRPIXn
		ndim * (8 * NBITS_BYTE) +			# CTYPEn
		(ndim * ndim) * (SZ_REAL * NBITS_CHAR)		# CD matrix

	    # Free any unneeded space in the STF descriptor.
	    if (STF_PCOUNT(stf) > 0) {
		call realloc (stf,
		    LEN_STFBASE + STF_PCOUNT(stf)*LEN_PDES, TY_STRUCT)
		IM_KDES(im) = stf
	    }

	    # Set up the group data block in the STF descriptor and in 
	    # the IMIO FITS format user area.  WARNING--the STF kernel
	    # is implicitly assuming that the GPB related parameters
	    # in non-STF format images are at the beginning of the user
	    # area, if they are present at all.   If this is not true
	    # then the following code will APPEND them to the user area.
	    # At STScI, non-STF format images are usually made from STF
	    # images and these parameters are at the beginning of the user
	    # area in that case.

	    pno = 1
	    call stf_addpar (im, "DATAMIN", TY_REAL, 1, zero, pno)
	    call stf_addpar (im, "DATAMAX", TY_REAL, 1, zero, pno)

	    do i = 1, ndim {
		call sprintf (pname,  SZ_KEYWORD, "CRPIX%d"); call pargi (i)
		call stf_addpar (im, pname, TY_REAL, 1, zero, pno)
		call sprintf (pname,  SZ_KEYWORD, "CRVAL%d"); call pargi (i)
		call stf_addpar (im, pname, TY_DOUBLE, 1, zero, pno)
		call sprintf (pname,  SZ_KEYWORD, "CTYPE%d"); call pargi (i)
		call stf_addpar (im, pname, TY_CHAR, 8, "PIXEL", pno)

		do j = 1, ndim {
		    call sprintf (pname,  SZ_KEYWORD, "CD%d_%d")
			call pargi (j)
			call pargi (i)
		    if (i == j)
			call stf_addpar (im, pname, TY_REAL, 1, one,  pno)
		    else
			call stf_addpar (im, pname, TY_REAL, 1, zero, pno)
		}
	    }
	}

	# Compute the size of each group in the pixel file, in chars.
	totpix = IM_LEN(im,1)
	do i = 2, ndim
	    totpix = totpix * IM_LEN(im,i)

	STF_SZGROUP(stf) = totpix * pix_size[IM_PIXTYPE(im)] +
	    STF_PSIZE(stf) / (SZB_CHAR * NBITS_BYTE)
end
