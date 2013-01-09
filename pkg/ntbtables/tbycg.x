include <mach.h>		# for MAX_INT, MAX_SHORT and SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbycgb -- Y getcol Boolean
# Read values for one column from a range of rows.  This is for data type
# Boolean and column-ordered SDAS tables.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge, 22-Jan-1993  Use IS_INDEF instead of == INDEF.
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Call sscan as a subroutine, not a function.
# Phil Hodge,  2-Jun-1997  Replace IS_INDEFD with TBL_IS_INDEFD.
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.
# Phil Hodge, 27-Aug-2002  In tbycgi and tbycgs, include an explicit test
#	for INDEF, rather than relying on a test on abs (dblbuf).

procedure tbycgb (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
bool	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	offset			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	nrows			# Number of rows to read
int	datatype		# Data type of element in table
int	dlen			# Number of char in one data element
int	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	read(), nscan()
errchk	seek, read

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)
	nrows = lastrow - firstrow + 1

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (nint(realbuf) != NO)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (nint(dblbuf) != NO)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
 		if (IS_INDEFI (intbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (intbuf != NO)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (shortbuf != NO)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_BOOL:
	    call seek (TB_FILE(tp), offset)
	    stat = read (TB_FILE(tp), buffer, nrows*SZ_BOOL)
	    do k = 1, nrows
		nullflag[k] = false
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), offset)
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    call strupk (charbuf, charbuf, SZ_LINE)
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
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtb:  invalid data type")
	    }
	}
end


# tbycgd -- Y getcol double
# Read values for one column from a range of rows.  This is for data type
# double precision and column-ordered SDAS tables.

procedure tbycgd (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
double	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	offset			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	nrows			# Number of rows to read
int	datatype		# Data type of element in table
int	dlen			# Number of char in one data element
int	stat			# OK or an error reading row
# buffers for copying elements of various types
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	read(), nscan()
errchk	seek, read

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)
	nrows = lastrow - firstrow + 1

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = realbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    call seek (TB_FILE(tp), offset)
	    stat = read (TB_FILE(tp), buffer, nrows*SZ_DOUBLE)
	    do k = 1, nrows {
		if (TBL_IS_INDEFD (buffer[k])) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    nullflag[k] = false
		}
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
 		if (IS_INDEFI (intbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = real(YES)
		else
		    buffer[k] = real(NO)
		nullflag[k] = false
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), offset)
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    call strupk (charbuf, charbuf, SZ_LINE)
		    call sscan (charbuf)
			call gargd (buffer[k])
		    if (nscan() < 1) {
			buffer[k] = INDEFD
			nullflag[k] = true
		    } else {
			nullflag[k] = IS_INDEFD (buffer[k])
		    }
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtd:  invalid data type")
	    }
	}
end


# tbycgr -- Y getcol real
# Read values for one column from a range of rows.  This is for data type real
# and column-ordered SDAS tables.

procedure tbycgr (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
real	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	offset			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	nrows			# Number of rows to read
int	datatype		# Data type of element in table
int	dlen			# Number of char in one data element
int	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	read(), nscan()
errchk	seek, read

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)
	nrows = lastrow - firstrow + 1

	switch (datatype) {
	case TY_REAL:
	    call seek (TB_FILE(tp), offset)
	    stat = read (TB_FILE(tp), buffer, nrows*SZ_REAL)
	    do k = 1, nrows
 		nullflag[k] = IS_INDEFR (buffer[k])

	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = dblbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
 		if (IS_INDEFI (intbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = real(YES)
		else
		    buffer[k] = real(NO)
		nullflag[k] = false
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), offset)
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    call strupk (charbuf, charbuf, SZ_LINE)
		    call sscan (charbuf)
			call gargr (buffer[k])
		    if (nscan() < 1) {
			buffer[k] = INDEFR
			nullflag[k] = true
		    } else {
			nullflag[k] = IS_INDEFR (buffer[k])
		    }
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtr:  invalid data type")
	    }
	}
end


# tbycgi -- Y getcol integer
# Read values for one column from a range of rows.  This is for data type
# integer and column-ordered SDAS tables.

procedure tbycgi (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
int	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	offset			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	nrows			# Number of rows to read
int	datatype		# Data type of element in table
int	dlen			# Number of char in one data element
int	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	read(), nscan()
errchk	seek, read

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)
	nrows = lastrow - firstrow + 1

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf) || abs (realbuf) > MAX_INT) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (realbuf)
		    nullflag[k] = IS_INDEFI (buffer[k])
		}
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf) || abs (dblbuf) > MAX_INT) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (dblbuf)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_INT:
	    call seek (TB_FILE(tp), offset)
	    stat = read (TB_FILE(tp), buffer, nrows*SZ_INT32)
            if (SZ_INT != SZ_INT32)
                call iupk32 (buffer, buffer, nrows)
	    do k = 1, nrows
 		nullflag[k] = IS_INDEFI (buffer[k])

	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = YES
		else
		    buffer[k] = NO
		nullflag[k] = false
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), offset)
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    call strupk (charbuf, charbuf, SZ_LINE)
		    call sscan (charbuf)
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				abs (dblbuf) > MAX_INT) {
			buffer[k] = INDEFI
			nullflag[k] = true
		    } else {
			buffer[k] = nint (dblbuf)
			nullflag[k] = IS_INDEFI (buffer[k])
		    }
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgti:  invalid data type")
	    }
	}
end


# tbycgs -- Y getcol short
# Read values for one column from a range of rows.  This is for data type
# short integer and column-ordered SDAS tables.

procedure tbycgs (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
short	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	offset			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	nrows			# Number of rows to read
int	datatype		# Data type of element in table
int	dlen			# Number of char in one data element
int	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	read(), nscan()
errchk	seek, read

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)
	nrows = lastrow - firstrow + 1

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf) || abs (realbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (realbuf)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf) || abs (dblbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (dblbuf)
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
 		if (IS_INDEFI (intbuf) || abs (intbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = false
		}
		offset = offset + dlen
	    }

	case TY_SHORT:
	    call seek (TB_FILE(tp), offset)
	    stat = read (TB_FILE(tp), buffer, nrows*SZ_SHORT)
	    do k = 1, nrows
 		nullflag[k] = IS_INDEFS (buffer[k])

	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = YES
		else
		    buffer[k] = NO
		nullflag[k] = false
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), offset)
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    call strupk (charbuf, charbuf, SZ_LINE)
		    call sscan (charbuf)
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				abs (dblbuf) > MAX_SHORT) {
			buffer[k] = INDEFS
			nullflag[k] = true
		    } else {
			buffer[k] = nint (dblbuf)
			nullflag[k] = IS_INDEFS (buffer[k])
		    }
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgts:  invalid data type")
	    }
	}
end


# tbycgt -- Y getcol text
# Read values for one column from a range of rows.  This is for character
# strings and column-ordered SDAS tables.

procedure tbycgt (tp, colptr, buffer, nullflag, lenstring, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
char	buffer[lenstring,ARB]	# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	lenstring		# The number of char in each element of buffer
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	offset			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	row			# Row number (loop index)
int	numchar			# Number of characters to copy string to string
int	nrows			# Number of rows to read
int	datatype		# Data type of element in table
int	dlen			# Number of char in one data element
int	stat			# OK or an error reading row
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	read()
errchk	seek, read, sprintf

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)
	nrows = lastrow - firstrow + 1

	switch (datatype) {
	case TY_REAL:
	    k = 1
	    do row = firstrow, lastrow {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargr (realbuf)
 		nullflag[k] = IS_INDEFR (realbuf)
		offset = offset + dlen
		k = k + 1
	    }
	case TY_DOUBLE:
	    k = 1
	    do row = firstrow, lastrow {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
		if (TBL_IS_INDEFD (dblbuf)) {
		    call strcpy ("INDEF", buffer[1,k], lenstring)
		    nullflag[k] = true
		} else {
		    call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
			call pargd (dblbuf)
		    nullflag[k] = false
		}
		offset = offset + dlen
		k = k + 1
	    }
	case TY_INT:
	    k = 1
	    do row = firstrow, lastrow {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargi (intbuf)
 		nullflag[k] = IS_INDEFI (intbuf)
		offset = offset + dlen
		k = k + 1
	    }
	case TY_SHORT:
	    k = 1
	    do row = firstrow, lastrow {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargs (shortbuf)
 		nullflag[k] = IS_INDEFS (shortbuf)
		offset = offset + dlen
		k = k + 1
	    }
	case TY_BOOL:
	    k = 1
	    do row = firstrow, lastrow {
		call seek (TB_FILE(tp), offset)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargb (boolbuf)
		nullflag[k] = false
		offset = offset + dlen
		k = k + 1
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		k = 1
		do row = firstrow, lastrow {
		    call seek (TB_FILE(tp), offset)
		    stat = read (TB_FILE(tp), charbuf, dlen)
		    numchar = min (lenstring, SZB_CHAR*dlen)
		    call strupk (charbuf, buffer[1,k], numchar)
		    nullflag[k] = (buffer[1,k] == EOS)
		    offset = offset + dlen
		    k = k + 1
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtt:  invalid data type")
	    }
	}
end
