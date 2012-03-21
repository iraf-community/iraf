# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include <mach.h>
include "fxf.h"

# FXFUPK.X -- Routines to upack an IEEE vector into native format.
#
#	fxf_unpack_data (cbuf, npix, pixtype, bscale, bzero)
#	      fxf_altmr (a, b, npix, bscale, bzero)
#	      fxf_altmd (a, b, npix, bscale, bzero)
#	      fxf_altmu (a, b, npix)
#	      fxf_astmr (a, b, npix, bscale, bzero)

define	NBITS_DOU	(SZB_CHAR * SZ_DOUBLE)
define	IOFF		1


# FITUPK -- Unpack cbuf in place from FITS binary format to local machine type.

procedure fxf_unpack_data (cbuf, npix, pixtype, bscale, bzero)

char	cbuf[ARB]		#U buffer with input,output data
int	npix			#I number of pixels in buffer
int	pixtype			#I input pixtype
double	bscale			#I scale factor to applied to input data
double	bzero			#I offset to applied to input data

int	nchars, nbytes
bool	fp_equald()
errchk	syserr

include <szpixtype.inc>

begin
        nchars = npix * pix_size[pixtype]
	nbytes = nchars * SZB_CHAR

	switch (pixtype) {
	case TY_SHORT, TY_USHORT:
	    if (BYTE_SWAP2 == YES)
		call bswap2 (cbuf, 1, cbuf, 1, nbytes)
	    if (pixtype == TY_USHORT)
		call fxf_altmu (cbuf, cbuf, npix)

	case TY_INT, TY_LONG:
	    if (BYTE_SWAP4 == YES)
		call bswap4 (cbuf, 1, cbuf, 1, nbytes)

	case TY_REAL:
	    ### Rather than perform this test redundantly a flag should be
	    ### passed in from the high level code telling the routine whether
	    ### or not it should apply the scaling.  Testing for floating
	    ### point equality (e.g. bscale != 1.0) is not portable.

	    if (!fp_equald(bscale,1.0d0) || !fp_equald(bzero,0.0d0)) {
		if (BYTE_SWAP4 == YES)
		    call bswap4 (cbuf, 1, cbuf, 1, nbytes)
		call iscl32 (cbuf, cbuf, npix, bscale, bzero)
	    } else
		call ieevupkr (cbuf, cbuf, npix)
	   
	case TY_DOUBLE:
	    ### Same as above.
	    if (!fp_equald(bscale,1.0d0) || !fp_equald(bzero,0.0d0)) {
		if (BYTE_SWAP4 == YES)
		    call bswap4 (cbuf, 1, cbuf, 1, nbytes)
		call iscl64 (cbuf, cbuf, npix, bscale, bzero)
	    } else
		call ieevupkd (cbuf, cbuf, npix)
	   
	default:
	    call syserr (SYS_FXFUPKDTY)
	}
end


# FXF_ALTMR -- Scale a real array.

procedure fxf_altmr (a, b, npix, bscale, bzero)

int	a[ARB]			#I input array
real	b[ARB]			#O output array
int	npix			#I number of pixels
double	bscale, bzero		#I scaling parameters

int	i

begin
	do i = 1, npix
	    b[i] = a[i] * bscale + bzero
end
			       

# FXF_ALTMD -- Scale a double array.

procedure fxf_altmd (a, b, npix, bscale, bzero)

int	a[ARB]			#I input array
double	b[ARB]			#O output array
int	npix			#I number of pixels
double	bscale, bzero		#I scaling parameters

int	i

begin
	### int and double are not the same size so if this operation is
	### to allow an in-place conversion it must go right to left instead
	### of left to right.

	do i = npix, 1, -1
	    b[i] = a[i] * bscale + bzero
end


# FXF_ALTMU -- Scale an array to unsigned short.

procedure fxf_altmu (a, b, npix)

short	 a[ARB]			 #I input array
char	 b[ARB]			 #O output array
int	 npix			 #I number of pixels

int	i
pointer sp, ip

begin
	call smark (sp)
	call salloc (ip, npix+1, TY_INT)
		 
	do i = 1, npix
	    Memi[ip+i] = a[i] + 32768

	call achtlu (Memi[ip+1], b, npix)
	call sfree (sp)
end


# FXF_ASTMR -- Scale an input short array into a real.

procedure fxf_astmr (a, b, npix, bscale, bzero)

short	a[ARB]			#I input array
real	b[ARB]			#O output array
int	npix			#I number of pixels
double	bscale, bzero		#I scaling parameters

int	i

begin
	do i = npix, 1, -1
	    b[i] = a[i] * bscale + bzero
end
			       

