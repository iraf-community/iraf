include <mach.h>		# for SZB_CHAR
include "tbtables.h"

# tbftya -- determine data type from tform
# This is for an ascii table; use tbftyb for a binary table.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 23-Jun-2000  Add tdtype to calling sequence; compare tscal &
#		tzero with 1. & 0. instead of INDEFD to see if they're defined.

procedure tbftya (tform, tdisp, tscal, tzero,
		tdtype, dtype, pformat, maxch, len)

char	tform[ARB]	# i: TFORM from FITS file
char	tdisp[ARB]	# i: TDISP from FITS file
double	tscal, tzero	# i: scaling parameters, or 1 & 0 if not defined
int	tdtype		# o: true data type in FITS table (e.g. integer)
int	dtype		# o: data type to use for table interface
char	pformat[maxch]	# o: spp print format
int	maxch		# i: size of print format string
int	len		# o: size of element
#--
pointer sp
pointer tform_lc	# tform in lower case
pointer spp_fmt		# format (tdisp or tform) converted to spp
pointer errmess		# scratch for error message
int	temp		# string length; value returned by tbbadf and ignored
int	lenfmt		# width of print format
int	ip, ctoi()

begin
	call smark (sp)
	call salloc (tform_lc, SZ_FNAME, TY_CHAR)
	call salloc (spp_fmt, SZ_FNAME, TY_CHAR)

	call strcpy (tform, Memc[tform_lc], SZ_FNAME)
	call strlwr (Memc[tform_lc])

	if (Memc[tform_lc] == 'a') {			# character
	    ip = 2
	    if (ctoi (Memc[tform_lc], ip, temp) < 1)
		dtype = -1
	    else
		dtype = -temp
	    len = (temp + SZB_CHAR-1) / SZB_CHAR * SZ_CHAR
	} else if (Memc[tform_lc] == 'd') {		# double precision
	    dtype = TBL_TY_DOUBLE
	    len = SZ_DOUBLE
	} else if (Memc[tform_lc] == 'e') {		# single precision
	    dtype = TBL_TY_REAL
	    len = SZ_REAL
	} else if (Memc[tform_lc] == 'f') {		# single precision
	    dtype = TBL_TY_REAL
	    len = SZ_REAL
	} else if (Memc[tform_lc] == 'i') {		# 32-bit integer
	    dtype = TBL_TY_INT
	    len = SZ_INT32
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			"unrecognized TFORM:  `%s'")
		call pargstr (tform)
	    call error (1, Memc[errmess])
	}

	# Convert print format from Fortran to SPP.
	if (tdisp[1] == EOS)
	    call tbbftp (tform, pformat)
	else
	    call tbbftp (tdisp, pformat)

	tdtype = dtype

	# If either scaling parameter is defined, promote the data type
	# from integer to floating point.  Note that only dtype is modified;
	# tdtype is the actual data type of the data in the FITS table.
	if (tscal != 1.d0 || tzero != 0.d0) {
	    if (dtype == TBL_TY_INT) {
		ip = 2
		if (ctoi (pformat, ip, lenfmt) < 1) {
		    dtype = TBL_TY_DOUBLE
		    len = SZ_DOUBLE
		} else if (lenfmt <= 7) {	# 6 digits plus sign or decimal
		    dtype = TBL_TY_REAL
		    len = SZ_REAL
		} else {
		    dtype = TBL_TY_DOUBLE
		    len = SZ_DOUBLE
		}
	    }
	}

	call sfree (sp)
end
