include <tbset.h>
include "tbtables.h"

# tbcadd -- add new columns
# Allocate column descriptors for new columns.
# Note that TB_COLINFO, the array of pointers to column descriptors,
# is assumed to be long enough already to contain the new pointers.
# This would have been taken care of by tbcdef or by tbzcol.
#
# Phil Hodge, 10-Aug-1987  Datatype for char string specified as -n;
#				lendata is currently ignored.
# Phil Hodge,  8-Oct-1987  TB_COLPTR is of type TY_POINTER.
# Phil Hodge,  6-Mar-1989  Pass datatype[k] instead of dtype to tbbadf.
# Phil Hodge,  1-May-1989  Call tbtchs.
# Phil Hodge, 15-Jan-1992  Add option for text table type;
#				move the call to tbtchs to tbcdef.
# Phil Hodge,  5-Mar-1993  Only call tbzadd if table is actually open.
# Phil Hodge, 29-Jul-1994  Rename lendata to lenarray, and use the value.
# Phil Hodge, 14-Apr-1998  Use strcpy instead of strpak or tbcftp for
#			column name, units, and print format.
# Phil Hodge,  7-Jun-1999  Reallocate TB_MAXCOLS, if necessary.
# Phil Hodge,  5-Aug-1999  Assign a value to COL_NELEM.
# Phil Hodge, 23-Jun-2000  Assign values to COL_TDTYPE, COL_TSCAL, COL_TZERO.
# Phil Hodge, 29-Mar-2001  Set TB_COLUSED equal to TB_ROWLEN for text table.

procedure tbcadd (tp, colptr,
	colname, colunits, colfmt, datatype, lenarray, numcols)

pointer tp				# i: Pointer to table descriptor
char	colname[SZ_COLNAME,numcols]	# i: Names of columns
char	colunits[SZ_COLUNITS,numcols]	# i: Units for columns
char	colfmt[SZ_COLFMT,numcols]	# i: Print formats for columns
int	datatype[numcols]		# i: Data types (-n for string)
int	lenarray[numcols]		# i: number of elements for each col
int	numcols				# i: Number of columns to be defined
pointer colptr[ARB]			# o: Pointers to the new columns
#--
pointer cp			# pointer to column descriptor
pointer prevcol			# pointer to descriptor for previous column
char	pformat[SZ_COLFMT]	# local copy of format for printing a column
int	dtype			# SPP data type of column
int	dlen			# number of char used by a column in table
int	k			# loop index
int	ncols			# current number of columns
int	new_maxcols		# new maximum number of columns
errchk	tbbaln, calloc

begin
	# Reallocate the space for column descriptors if necessary.
	ncols = TB_NCOLS(tp) + numcols		# total
	if (ncols > TB_MAXCOLS(tp)) {
	    new_maxcols = ncols + DEFMAXCOLS
	    call realloc (TB_COLPTR(tp), new_maxcols, TY_POINTER)
	    TB_MAXCOLS(tp) = new_maxcols
	}

	do k = 1, numcols {

	    # Assign value for SPP data type and for data length (of one
	    # element) in table.
	    call tbbaln (datatype[k], dtype, dlen)

	    # Assign default print format if none given; pformat is output.
	    call tbbadf (colfmt[1,k], datatype[k], dlen, pformat, SZ_COLFMT)

	    # Allocate space for column descriptor
	    call calloc (cp, LEN_COLSTRUCT, TY_STRUCT)
	    ncols = TB_NCOLS(tp) + 1
	    TB_NCOLS(tp) = ncols
	    TB_COLINFO(tp,ncols) = cp		# save pointer to col descr

	    COL_NUMBER(cp) = ncols
	    COL_DTYPE(cp)  = dtype
	    COL_NELEM(cp)  = max (1, lenarray[k])
	    COL_LEN(cp)    = dlen * COL_NELEM(cp)
	    # COL_TDTYPE, COL_TSCAL, COL_TZERO are only needed for FITS tables.
	    COL_TDTYPE(cp) = COL_DTYPE(cp)
	    COL_TSCAL(cp)  = 1.d0
	    COL_TZERO(cp)  = 0.d0

	    # Copy name, units, print format into column descriptor.
	    call strcpy (colname[1,k],  COL_NAME(cp),  SZ_COLNAME)
	    call strcpy (colunits[1,k], COL_UNITS(cp), SZ_COLUNITS)
	    call strcpy (pformat,       COL_FMT(cp),   SZ_COLFMT)

	    if (TB_TYPE(tp) == TBL_TYPE_TEXT) {

		# Assign COL_OFFSET(cp) to be a pointer to allocated memory
		# for column values.  Also change the data type if necessary.
		# If the table is not open yet, set col_offset in case the
		# table type will be changed to non-text type.
		if (TB_IS_OPEN(tp)) {
		    call tbzadd (tp, cp)
		    TB_COLUSED(tp) = TB_ROWLEN(tp)
		} else {
		    if (ncols > 1) {
			prevcol = TB_COLINFO(tp,ncols-1)
			COL_OFFSET(cp) = COL_OFFSET(prevcol) + COL_LEN(prevcol)
		    } else {
			COL_OFFSET(cp) = 0
		    }
		    TB_COLUSED(tp) = COL_OFFSET(cp) + COL_LEN(cp)
		}

	    } else {

		# Assign COL_OFFSET(cp) to be the sum of the lengths
		# (unit = char) of all previous columns.
		if (ncols > 1) {
		    prevcol = TB_COLINFO(tp,ncols-1)
		    COL_OFFSET(cp) = COL_OFFSET(prevcol) + COL_LEN(prevcol)
		} else {
		    COL_OFFSET(cp) = 0		# no previous column
		}
		TB_COLUSED(tp) = COL_OFFSET(cp) + COL_LEN(cp)
	    }

	    colptr[k] = cp
	}
end
