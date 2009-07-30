include <mach.h>		# for MAX_INT, MAX_SHORT and SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbyrgb -- Y getrow Boolean
# Read column values from a row.  This is for data type Boolean and
# column-ordered SDAS tables.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge, 22-Jan-1993  Use IS_INDEF instead of == INDEF.
# Phil Hodge,  1-Apr-1993  Include short datatype; in tbyrgb, for types other
#	than boolean, change test from "if (buf == YES)" to "if (buf != NO)".
# Phil Hodge,  4-Nov-1993  Delete check on row number beyond EOF;
#			call sscan as a subroutine, not a function.
# Phil Hodge,  2-Jun-1997  Replace IS_INDEFD with TBL_IS_INDEFD.
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.
# Phil Hodge, 27-Aug-2002  In tbyrgi and tbyrgs, include an explicit test
#	for INDEF, rather than relying on a test on abs (dblbuf).

procedure tbyrgb (tp, colptr, buffer, nullflag, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to column descriptors
bool	buffer[numcols]		# Buffer for values
bool	nullflag[numcols]	# Array of flags:  true ==> element is undefined
int	numcols			# Number of columns from which to get values
long	rownum			# Row number
#--
size_t	sz_val
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
long	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]
long	tbyoff(), read()
int	nscan(), inint()
errchk	seek, read

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		sz_val = SZ_REAL
		stat = read (TB_FILE(tp), realbuf, sz_val)
 		if (IS_INDEFR (realbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (inint(realbuf) != NO)
		    nullflag[k] = false
		}
	    case TY_DOUBLE:
		sz_val = SZ_DOUBLE
		stat = read (TB_FILE(tp), dblbuf, sz_val)
 		if (TBL_IS_INDEFD (dblbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (idnint(dblbuf) != NO)
		    nullflag[k] = false
		}
	    case TY_INT:
		sz_val = SZ_INT
		stat = read (TB_FILE(tp), intbuf, sz_val)
 		if (IS_INDEFI (intbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (intbuf != NO)
		    nullflag[k] = false
		}
	    case TY_SHORT:
		sz_val = SZ_SHORT
		stat = read (TB_FILE(tp), shortbuf, sz_val)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (shortbuf != NO)
		    nullflag[k] = false
		}
	    case TY_BOOL:
		sz_val = SZ_BOOL
		stat = read (TB_FILE(tp), buffer[k], sz_val)
		nullflag[k] = false
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    sz_val = COL_LEN(colptr[k])
		    stat = read (TB_FILE(tp), charbuf, sz_val)
		    sz_val = SZ_LINE
		    call strupk (charbuf, charbuf, sz_val)
		    if (charbuf[1] != EOS) {
			call sscan (charbuf)
			    call gargb (buffer[k])
			if (nscan() < 1) {
			    buffer[k] = false
			    nullflag[k] = true
			} else {
			    nullflag[k] = false
			}
		    } else {
			buffer[k] = false
			nullflag[k] = true
		    }
		} else {
		    call error (ER_TBCOLBADTYP, "tbrgtb:  invalid data type")
		}
	    }
	}
end


# tbyrgd -- Y getrow double
# Read column values from a row.  This is for data type double and
# column-ordered SDAS tables.

procedure tbyrgd (tp, colptr, buffer, nullflag, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to column descriptors
double	buffer[numcols]		# Buffer for values
bool	nullflag[numcols]	# Array of flags:  true ==> element is undefined
int	numcols			# Number of columns from which to get values
long	rownum			# Row number
#--
size_t	sz_val
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
long	stat			# OK or an error reading row
# buffers for copying elements of various types
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff(), read()
int	nscan()
errchk	seek, read

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		sz_val = SZ_REAL
		stat = read (TB_FILE(tp), realbuf, sz_val)
 		if (IS_INDEFR (realbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = realbuf
		    nullflag[k] = false
		}
	    case TY_DOUBLE:
		sz_val = SZ_DOUBLE
		stat = read (TB_FILE(tp), buffer[k], sz_val)
 		if (TBL_IS_INDEFD (buffer[k])) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    nullflag[k] = false
		}
	    case TY_INT:
		sz_val = SZ_INT
		stat = read (TB_FILE(tp), intbuf, sz_val)
 		if (IS_INDEFI (intbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = false
		}
	    case TY_SHORT:
		sz_val = SZ_SHORT
		stat = read (TB_FILE(tp), shortbuf, sz_val)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
	    case TY_BOOL:
		sz_val = SZ_BOOL
		stat = read (TB_FILE(tp), boolbuf, sz_val)
		if (boolbuf)
		    buffer[k] = double(YES)
		else
		    buffer[k] = double(NO)
		nullflag[k] = false
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    sz_val = COL_LEN(colptr[k])
		    stat = read (TB_FILE(tp), charbuf, sz_val)
		    sz_val = SZ_LINE
		    call strupk (charbuf, charbuf, sz_val)
		    call sscan (charbuf)
			call gargd (buffer[k])
		    if (nscan() < 1)
			buffer[k] = INDEFD
		    nullflag[k] = IS_INDEFD (buffer[k])
		} else {
		    call error (ER_TBCOLBADTYP, "tbrgtd:  invalid data type")
		}
	    }
	}
end


# tbyrgr -- Y getrow real
# Read column values from a row.  This is for data type real and
# column-ordered SDAS tables.

procedure tbyrgr (tp, colptr, buffer, nullflag, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to column descriptors
real	buffer[numcols]		# Buffer for values
bool	nullflag[numcols]	# Array of flags:  true ==> element is undefined
int	numcols			# Number of columns from which to get values
long	rownum			# Row number
#--
size_t	sz_val
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
long	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff(), read()
int	nscan()
errchk	seek, read

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		sz_val = SZ_REAL
		stat = read (TB_FILE(tp), buffer[k], sz_val)
		nullflag[k] = IS_INDEFR (buffer[k])
	    case TY_DOUBLE:
		sz_val = SZ_DOUBLE
		stat = read (TB_FILE(tp), dblbuf, sz_val)
 		if (TBL_IS_INDEFD (dblbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = dblbuf
		    nullflag[k] = false
		}
	    case TY_INT:
		sz_val = SZ_INT
		stat = read (TB_FILE(tp), intbuf, sz_val)
 		if (IS_INDEFI (intbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = false
		}
	    case TY_SHORT:
		sz_val = SZ_SHORT
		stat = read (TB_FILE(tp), shortbuf, sz_val)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
	    case TY_BOOL:
		sz_val = SZ_BOOL
		stat = read (TB_FILE(tp), boolbuf, sz_val)
		if (boolbuf)
		    buffer[k] = real(YES)
		else
		    buffer[k] = real(NO)
		nullflag[k] = false
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    sz_val = COL_LEN(colptr[k])
		    stat = read (TB_FILE(tp), charbuf, sz_val)
		    sz_val = SZ_LINE
		    call strupk (charbuf, charbuf, sz_val)
		    call sscan (charbuf)
			call gargr (buffer[k])
		    if (nscan() < 1)
			buffer[k] = INDEFR
		    nullflag[k] = IS_INDEFR (buffer[k])
		} else {
		    call error (ER_TBCOLBADTYP, "tbrgtr:  invalid data type")
		}
	    }
	}
end


# tbyrgi -- Y getrow integer
# Read column values from a row.  This is for data type integer and
# column-ordered SDAS tables.

procedure tbyrgi (tp, colptr, buffer, nullflag, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to column descriptors
int	buffer[numcols]		# Buffer for values
bool	nullflag[numcols]	# Array of flags:  true ==> element is undefined
int	numcols			# Number of columns from which to get values
long	rownum			# Row number
#--
size_t	sz_val
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
long	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff(), read()
int	nscan(), inint()
real	aabs()
errchk	seek, read

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		sz_val = SZ_REAL
		stat = read (TB_FILE(tp), realbuf, sz_val)
 		if (IS_INDEFR (realbuf) || aabs (realbuf) > MAX_INT) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = inint (realbuf)
		    nullflag[k] = false
		}
	    case TY_DOUBLE:
		sz_val = SZ_DOUBLE
		stat = read (TB_FILE(tp), dblbuf, sz_val)
 		if (TBL_IS_INDEFD (dblbuf) || dabs (dblbuf) > MAX_INT) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = idnint (dblbuf)
		    nullflag[k] = false
		}
	    case TY_INT:
		sz_val = SZ_INT
		stat = read (TB_FILE(tp), buffer[k], sz_val)
		nullflag[k] = IS_INDEFI (buffer[k])
	    case TY_SHORT:
		sz_val = SZ_SHORT
		stat = read (TB_FILE(tp), shortbuf, sz_val)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
	    case TY_BOOL:
		sz_val = SZ_BOOL
		stat = read (TB_FILE(tp), boolbuf, sz_val)
		if (boolbuf)
		    buffer[k] = YES
		else
		    buffer[k] = NO
		nullflag[k] = false
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    sz_val = COL_LEN(colptr[k])
		    stat = read (TB_FILE(tp), charbuf, sz_val)
		    sz_val = SZ_LINE
		    call strupk (charbuf, charbuf, sz_val)
		    call sscan (charbuf)
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				dabs (dblbuf) > MAX_INT) {
			buffer[k] = INDEFI
			nullflag[k] = true
		    } else {
			buffer[k] = idnint (dblbuf)
			nullflag[k] = IS_INDEFI (buffer[k])
		    }
		} else {
		    call error (ER_TBCOLBADTYP, "tbrgti:  invalid data type")
		}
	    }
	}
end


# tbyrgs -- Y getrow short
# Read column values from a row.  This is for data type short integer and
# column-ordered SDAS tables.

procedure tbyrgs (tp, colptr, buffer, nullflag, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to column descriptors
short	buffer[numcols]		# Buffer for values
bool	nullflag[numcols]	# Array of flags:  true ==> element is undefined
int	numcols			# Number of columns from which to get values
long	rownum			# Row number
#--
size_t	sz_val
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
long	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff(), read()
int	nscan()
short	snint(), sdnint()
real	aabs()
errchk	seek, read

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		sz_val = SZ_REAL
		stat = read (TB_FILE(tp), realbuf, sz_val)
 		if (IS_INDEFR (realbuf) || aabs (realbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = snint (realbuf)
		    nullflag[k] = false
		}
	    case TY_DOUBLE:
		sz_val = SZ_DOUBLE
		stat = read (TB_FILE(tp), dblbuf, sz_val)
 		if (TBL_IS_INDEFD (dblbuf) || dabs (dblbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = sdnint (dblbuf)
		    nullflag[k] = false
		}
	    case TY_INT:
		sz_val = SZ_INT
		stat = read (TB_FILE(tp), intbuf, sz_val)
 		if (IS_INDEFI (intbuf) || iabs (intbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = IS_INDEFS (buffer[k])
		}
	    case TY_SHORT:
		sz_val = SZ_SHORT
		stat = read (TB_FILE(tp), buffer[k], sz_val)
		nullflag[k] = IS_INDEFS (buffer[k])
	    case TY_BOOL:
		sz_val = SZ_BOOL
		stat = read (TB_FILE(tp), boolbuf, sz_val)
		if (boolbuf)
		    buffer[k] = YES
		else
		    buffer[k] = NO
		nullflag[k] = false
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    sz_val = COL_LEN(colptr[k])
		    stat = read (TB_FILE(tp), charbuf, sz_val)
		    sz_val = SZ_LINE
		    call strupk (charbuf, charbuf, sz_val)
		    call sscan (charbuf)
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				dabs (dblbuf) > MAX_SHORT) {
			buffer[k] = INDEFS
			nullflag[k] = true
		    } else {
			buffer[k] = sdnint (dblbuf)
			nullflag[k] = IS_INDEFS (buffer[k])
		    }
		} else {
		    call error (ER_TBCOLBADTYP, "tbrgts:  invalid data type")
		}
	    }
	}
end


# tbyrgt -- Y getrow text
# Read column values from a row.  This is for character strings and
# column-ordered SDAS tables.

procedure tbyrgt (tp, colptr, buffer, nullflag, lenstring, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to column descriptors
char	buffer[lenstring,numcols]	# Buffer for values
bool	nullflag[numcols]	# Array of flags:  true ==> element is undefined
int	lenstring		# Length of each string in array buffer
int	numcols			# Number of columns from which to get values
long	rownum			# Row number
#--
size_t	sz_val
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
long	stat			# OK or an error reading row
size_t	numchar			# Number of characters to copy string to string
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff(), read()
errchk	seek, read, sprintf

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		sz_val = SZ_REAL
		stat = read (TB_FILE(tp), realbuf, sz_val)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr[k]))
		    call pargr (realbuf)
		nullflag[k] = IS_INDEFR (realbuf)
	    case TY_DOUBLE:
		sz_val = SZ_DOUBLE
		stat = read (TB_FILE(tp), dblbuf, sz_val)
		if (TBL_IS_INDEFD (dblbuf)) {
		    call strcpy ("INDEF", buffer[1,k], lenstring)
		    nullflag[k] = true
		} else {
		    call sprintf (buffer[1,k], lenstring, COL_FMT(colptr[k]))
			call pargd (dblbuf)
		    nullflag[k] = false
		}
	    case TY_INT:
		sz_val = SZ_INT
		stat = read (TB_FILE(tp), intbuf, sz_val)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr[k]))
		    call pargi (intbuf)
		nullflag[k] = IS_INDEFI (intbuf)
	    case TY_SHORT:
		sz_val = SZ_SHORT
		stat = read (TB_FILE(tp), shortbuf, sz_val)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr[k]))
		    call pargs (shortbuf)
		nullflag[k] = IS_INDEFS (shortbuf)
	    case TY_BOOL:
		sz_val = SZ_BOOL
		stat = read (TB_FILE(tp), boolbuf, sz_val)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr[k]))
		    call pargb (boolbuf)
		nullflag[k] = false
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    dlen = COL_LEN(colptr[k])
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    numchar = min (lenstring, SZB_CHAR*dlen)
		    call strupk (charbuf, buffer[1,k], numchar)
		    nullflag[k] = (buffer[1,k] == EOS)
		} else {
		    call error (ER_TBCOLBADTYP, "tbrgtt:  invalid data type")
		}
	    }
	}
end
