# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include <mach.h>
include "fxf.h"


# FXF_PAK_DATA -- Convert npix elements of type pixtype as needed for storage
# in a FITS file.  All floating point data will be converted to IEEE format.
# The input and output buffers may be the same if desired.

procedure fxf_pak_data (ibuf, obuf, npix, pixtype)

char    ibuf[ARB]		#I input data buffer
char    obuf[ARB]		#I output data buffer
size_t	npix			#I number of pixels in buffer
int	pixtype			#I input pixel datatype

size_t	nbytes, nchars, c_1
int	sizeof()
errchk	syserr

begin
	c_1 = 1

	### Possibly the MII conversion routines should be used here as
	### they handle all these datatypes (except maybe ushort).

	nchars = npix * sizeof (pixtype)
	nbytes = nchars * SZB_CHAR

	switch (pixtype) {
	case TY_USHORT:
	    call fxf_altmu (ibuf, obuf, npix)
	    if (BYTE_SWAP2 == YES)
		call bswap2 (obuf, c_1, obuf, c_1, nbytes)

	case TY_SHORT:
	    if (BYTE_SWAP2 == YES)
		call bswap2 (ibuf, c_1, obuf, c_1, nbytes)
	    else
		call amovc (ibuf, obuf, nchars)

	case TY_INT:
	    if (BYTE_SWAP4 == YES)
		call bswap4 (ibuf, c_1, obuf, c_1, nbytes)
	    else
		call amovc (ibuf, obuf, nchars)

	case TY_LONG:
	    if ( SZ_LONG == 2 ) {
		if (BYTE_SWAP4 == YES) {
		    call bswap4 (ibuf, c_1, obuf, c_1, nbytes)
		} else {
		    call amovc (ibuf, obuf, nchars)
		}
	    } else {
		if (BYTE_SWAP8 == YES) {
		    call bswap8 (ibuf, c_1, obuf, c_1, nbytes)
		} else {
		    call amovc (ibuf, obuf, nchars)
		}
	    }

	case TY_REAL:
	    # arg1: incompatible pointer
	    call ieevpakr (ibuf, obuf, npix)

	case TY_DOUBLE:
	    # arg1: incompatible pointer
	    call ieevpakd (ibuf, obuf, npix)

	default:
	   call syserr (SYS_FXFPKDTYP)
	}
end
