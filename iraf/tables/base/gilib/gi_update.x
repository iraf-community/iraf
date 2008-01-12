# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <imio.h>
include <imhdr.h>
include <mach.h>
include "gi.h"

define   NBITS_CHAR   (SZB_CHAR * NBITS_BYTE)
# GI_UPDATE -- Procedure to update the STF descriptor before writing the
# user area to the image header.

procedure gi_update (im)

pointer im

int	sizeof()
int	pixtype, bitpix, nbytes, clktime()
int     xdim, imaccf(), i
char    keyw[SZ_KEYWORD]
pointer stf

begin

	stf = IM_KDES(im)

	pixtype = IM_PIXTYPE(im)

	bitpix = sizeof(pixtype) * NBITS_CHAR
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
	STF_GROUPS(stf) = YES

	# Set the IMIO min/max fields.

	IM_MIN(im) = 0.
	IM_MAX(im) = 0.
	IM_LIMTIME(im) = clktime(long(0))


	# Do not call stf_updhdr()
	IM_UPDATE(im) = NO
	 
	# Now delete the WCS information corresponding to the extra
	# dimension if it is present on the FITS files created by
	# stwfits.
	if (STF_GCOUNT(stf) > 1) {
	   xdim = STF_NAXIS(stf) + 1
           call sprintf (keyw,  SZ_KEYWORD, "CRPIX%d"); call pargi (xdim)
	   if (imaccf (im, keyw) == YES) call imdelf (im, keyw)
	   call sprintf (keyw,  SZ_KEYWORD, "CRVAL%d"); call pargi (xdim)
	   if (imaccf (im, keyw) == YES) call imdelf (im, keyw)
	   call sprintf (keyw,  SZ_KEYWORD, "CTYPE%d"); call pargi (xdim)
	   if (imaccf (im, keyw) == YES) call imdelf (im, keyw)
	 
	   do i = 1, xdim - 1 {
	     call sprintf (keyw,  SZ_KEYWORD, "CD%d_%d")
	        call pargi (xdim)
	        call pargi (i)
	     if (imaccf (im, keyw) == YES) call imdelf (im, keyw)
	     call sprintf (keyw,  SZ_KEYWORD, "CD%d_%d")
	        call pargi (i)
	        call pargi (xdim)
	     if (imaccf (im, keyw) == YES) call imdelf (im, keyw)
	   }
	   call sprintf (keyw,  SZ_KEYWORD, "CD%d_%d")
	      call pargi (xdim)
	      call pargi (xdim)
	   if (imaccf (im, keyw) == YES) call imdelf (im, keyw)
	}

	call stf_wfitshdr (im)

end
