include <mach.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbxcgb -- X getcol Boolean
# Read values for one column from a range of rows.  This is for data type
# Boolean and row-oriented SDAS tables.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge, 22-Jan-1993  Use IS_INDEF instead of == INDEF.
# Phil Hodge, 31-Mar-1993  Include short datatype; in tbxcgb, for types other
#	than boolean, change test from "if (buf == YES)" to "if (buf != NO)".
# Phil Hodge,  3-Sep-1993  Change declaration of locn in tbxcgr to long;
# Phil Hodge,  4-Nov-1993  Call sscan as a subroutine, not a function.
# Phil Hodge, 14-Sep-1994  Use tbeszt for length of string.
# Phil Hodge,  2-Jun-1997  Replace IS_INDEFD with TBL_IS_INDEFD.
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.
# Phil Hodge, 27-Aug-2002  In tbxcgi and tbxcgs, include an explicit test
#	for INDEF, rather than relying on a test on abs (dblbuf).

procedure tbxcgb (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
bool	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
#--
long	locn			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
int	stat			# OK or an error reading row
int	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]
int	read(), nscan()
int	tbeszt()
errchk	seek, read

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (nint(realbuf) != NO)
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (nint(dblbuf) != NO)
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
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
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (shortbuf != NO)
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), buffer[k], SZ_BOOL)
		nullflag[k] = false
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    stat = read (TB_FILE(tp), charbuf, nchar)
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
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtb:  invalid data type")
	    }
	}
end


# tbxcgd -- X getcol double
# Read values for one column from a range of rows.  This is for data type
# double precision and row-ordered SDAS tables.

procedure tbxcgd (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
double	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	locn			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
int	stat			# OK or an error reading row
int	nchar			# Size of a string in table file
# buffers for reading values of various types
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
int	read(), nscan()
int	tbeszt()
errchk	seek, read

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = realbuf
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), buffer[k], SZ_DOUBLE)
		if (TBL_IS_INDEFD (buffer[k])) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
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
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFD
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = real(YES)
		else
		    buffer[k] = real(NO)
		nullflag[k] = false
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    stat = read (TB_FILE(tp), charbuf, nchar)
		    call strupk (charbuf, charbuf, SZ_LINE)
		    call sscan (charbuf)
			call gargd (buffer[k])
		    if (nscan() < 1)
			buffer[k] = INDEFD
		    nullflag[k] = IS_INDEFD (buffer[k])
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtd:  invalid data type")
	    }
	}
end


# tbxcgr -- X getcol real
# Read values for one column from a range of rows.  This is for data type real
# and row-ordered SDAS tables.

procedure tbxcgr (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
real	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
int	k			# Index in arrays buffer & nullflag
long	locn			# Location (chars) for reading in table
int	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
int	stat			# OK or an error reading row
int	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
int	read(), nscan()
int	tbeszt()
errchk	seek, read

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), buffer[k], SZ_REAL)
		nullflag[k] = IS_INDEFR (buffer[k])
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = dblbuf
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
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
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFR
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = real(YES)
		else
		    buffer[k] = real(NO)
		nullflag[k] = false
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    stat = read (TB_FILE(tp), charbuf, nchar)
		    call strupk (charbuf, charbuf, SZ_LINE)
		    call sscan (charbuf)
			call gargr (buffer[k])
		    if (nscan() < 1)
			buffer[k] = INDEFR
		    nullflag[k] = IS_INDEFR (buffer[k])
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtr:  invalid data type")
	    }
	}
end


# tbxcgi -- X getcol integer
# Read values for one column from a range of rows.  This is for data type
# integer and row-ordered SDAS tables.

procedure tbxcgi (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
int	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	locn			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
int	stat			# OK or an error reading row
int	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
int	read(), nscan()
int	tbeszt()
errchk	seek, read

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf) || abs (realbuf) > MAX_INT) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (realbuf)
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf) || abs (dblbuf) > MAX_INT) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (dblbuf)
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), buffer[k], SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (buffer[k], buffer[k], 1)
		nullflag[k] = IS_INDEFI (buffer[k])
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
 		if (IS_INDEFS (shortbuf)) {
		    buffer[k] = INDEFI
		    nullflag[k] = true
		} else {
		    buffer[k] = shortbuf
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = YES
		else
		    buffer[k] = NO
		nullflag[k] = false
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    stat = read (TB_FILE(tp), charbuf, nchar)
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
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgti:  invalid data type")
	    }
	}
end


# tbxcgs -- X getcol short
# Read values for one column from a range of rows.  This is for data type
# short integer and row-ordered SDAS tables.

procedure tbxcgs (tp, colptr, buffer, nullflag, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
short	buffer[ARB]		# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	locn			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
int	stat			# OK or an error reading row
int	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]
int	read(), nscan()
int	tbeszt()
errchk	seek, read

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
 		if (IS_INDEFR (realbuf) || abs (realbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		} else {
		    buffer[k] = nint (realbuf)
		}
		nullflag[k] = IS_INDEFS (buffer[k])
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
 		if (TBL_IS_INDEFD (dblbuf) || abs (dblbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = nint (dblbuf)
		    nullflag[k] = IS_INDEFS (buffer[k])
		}
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
 		if (IS_INDEFI (intbuf) || abs (intbuf) > MAX_SHORT) {
		    buffer[k] = INDEFS
		    nullflag[k] = true
		} else {
		    buffer[k] = intbuf
		    nullflag[k] = IS_INDEFS (buffer[k])
		}
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), buffer[k], SZ_SHORT)
		nullflag[k] = IS_INDEFS (buffer[k])
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		if (boolbuf)
		    buffer[k] = YES
		else
		    buffer[k] = NO
		nullflag[k] = false
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    stat = read (TB_FILE(tp), charbuf, nchar)
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
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgts:  invalid data type")
	    }
	}
end


# tbxcgt -- X getcol text
# Read values for one column from a range of rows.  This is for character
# strings and row-ordered SDAS tables.

procedure tbxcgt (tp, colptr, buffer, nullflag, lenstring, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
char	buffer[lenstring,ARB]	# Buffer for values
bool	nullflag[ARB]		# True if element is undefined in table
int	lenstring		# The number of char in each element of buffer
int	firstrow		# Number of first row from which to get values
int	lastrow			# Number of last row from which to get values
#--
long	locn			# Location (chars) for reading in table
int	k			# Index in arrays buffer & nullflag
int	numchar			# Number of characters to copy string to string
int	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
int	stat			# OK or an error reading row
int	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
int	read()
int	tbeszt()
errchk	seek, read, sprintf

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), realbuf, SZ_REAL)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargr (realbuf)
 		nullflag[k] = IS_INDEFR (realbuf)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), dblbuf, SZ_DOUBLE)
		if (TBL_IS_INDEFD (dblbuf)) {
		    call strcpy ("INDEF", buffer[1,k], lenstring)
		    nullflag[k] = true
		} else {
		    call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
			call pargd (dblbuf)
		    nullflag[k] = false
		}
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), intbuf, SZ_INT32)
                if (SZ_INT != SZ_INT32)
                    call iupk32 (intbuf, intbuf, 1)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargi (intbuf)
 		nullflag[k] = IS_INDEFI (intbuf)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), shortbuf, SZ_SHORT)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargs (shortbuf)
 		nullflag[k] = IS_INDEFS (shortbuf)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		stat = read (TB_FILE(tp), boolbuf, SZ_BOOL)
		call sprintf (buffer[1,k], lenstring, COL_FMT(colptr))
		    call pargb (boolbuf)
		nullflag[k] = false
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    stat = read (TB_FILE(tp), charbuf, nchar)
		    numchar = min (lenstring, SZB_CHAR*nchar)
		    call strupk (charbuf, buffer[1,k], numchar)
		    nullflag[k] = (buffer[1,k] == EOS)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcgtt:  invalid data type")
	    }
	}
end
