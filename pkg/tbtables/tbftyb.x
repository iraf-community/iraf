include <mach.h>		# for SZB_CHAR
include "tbtables.h"

# tbftyb -- determine data type from tform
# This is for a binary table; use tbftya for an ascii table.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 23-Jun-2000  Add tdtype to calling sequence; compare tscal &
#		tzero with 1. & 0. instead of INDEFD to see if they're defined.

procedure tbftyb (tform, tdisp, tscal, tzero,
		tdtype, dtype, pformat, maxch, nelem, len)

char	tform[ARB]	# i: TFORM from FITS file
char	tdisp[ARB]	# i: TDISP from FITS file
double	tscal, tzero	# i: scaling parameters, or 1 & 0 if not defined
int	tdtype		# o: true data type in FITS table (e.g. integer)
int	dtype		# o: data type to use for table interface
char	pformat[maxch]	# o: spp print format
int	maxch		# i: size of print format string
int	nelem		# o: number of elements in array
int	len		# o: nelem * size of one element
#--
pointer sp
pointer tform_lc	# tform in lower case
pointer errmess		# scratch for error message
int	rpt		# repeat count
int	lenstring	# size of string
int	ip, ctoi()

begin
	call smark (sp)
	call salloc (tform_lc, SZ_FNAME, TY_CHAR)

	call strcpy (tform, Memc[tform_lc], SZ_FNAME)
	call strlwr (Memc[tform_lc])

	# Assign a default; this is only used for char string.
	lenstring = 1

	# Read repeat count.
	ip = 1
	if (ctoi (Memc[tform_lc], ip, rpt) < 1)
	    rpt = 1

	nelem = rpt

	if (Memc[tform_lc+ip-1] == 'a') {		# character
	    # Single element has tform wA, but FITSIO supports rAw as well.
	    ip = ip + 1			# skip past 'a' and check for a number
	    if (ctoi (Memc[tform_lc], ip, lenstring) < 1)
		lenstring = rpt
	    dtype = -lenstring
	    nelem = rpt / lenstring
	    len = (lenstring + SZB_CHAR-1) / SZB_CHAR * SZ_CHAR
	    len = nelem * len
	} else if (Memc[tform_lc+ip-1] == 'b') {	# unsigned byte
	    dtype = TBL_TY_SHORT
	    len = nelem * SZ_SHORT
	} else if (Memc[tform_lc+ip-1] == 'c') {	# complex; use double
	    dtype = TBL_TY_DOUBLE
	    len = nelem * SZ_DOUBLE
	} else if (Memc[tform_lc+ip-1] == 'd') {	# double precision
	    dtype = TBL_TY_DOUBLE
	    len = nelem * SZ_DOUBLE
	} else if (Memc[tform_lc+ip-1] == 'e') {	# single precision
	    dtype = TBL_TY_REAL
	    len = nelem * SZ_REAL
	} else if (Memc[tform_lc+ip-1] == 'i') {	# 16-bit integer
	    dtype = TBL_TY_SHORT
	    len = nelem * SZ_SHORT
	} else if (Memc[tform_lc+ip-1] == 'j') {	# 32-bit integer
	    dtype = TBL_TY_INT
	    len = nelem * SZ_INT32
	} else if (Memc[tform_lc+ip-1] == 'l') {	# logical
	    dtype = TBL_TY_BOOL
	    len = nelem * SZ_BOOL
	} else if (Memc[tform_lc+ip-1] == 'm') {	# complex double prec
	    call error (1, "can't handle complex double precision")
	} else if (Memc[tform_lc+ip-1] == 'p') {	# variable length
#	    call error (1, "can't handle variable length arrays")
	    ;
	} else if (Memc[tform_lc+ip-1] == 'x') {	# bit
	    dtype = TBL_TY_SHORT
	    len = nelem * SZ_SHORT
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			"unrecognized TFORM:  `%s'")
		call pargstr (tform)
	    call error (1, Memc[errmess])
	}

	tdtype = dtype

	# If either scaling parameter is defined, promote the data type
	# from integer to floating point.  Note that only dtype is modified;
	# tdtype is the actual data type of the data in the FITS table.
	if (tscal != 1.d0 || tzero != 0.d0) {
	    if (dtype == TBL_TY_SHORT) {
		dtype = TBL_TY_REAL
		len = nelem * SZ_REAL
	    } else if (dtype == TBL_TY_INT) {
		dtype = TBL_TY_DOUBLE
		len = nelem * SZ_DOUBLE
	    }
	}

	# Assign default print format or convert format from Fortran to SPP.
	if (tdisp[1] == NULL)			# not specified
	    call tbbadf ("", dtype, lenstring, pformat, maxch)
	else
	    call tbbftp (tdisp, pformat)

	call sfree (sp)
end
