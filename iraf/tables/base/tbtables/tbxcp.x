include <mach.h>		# for MAX_INT and MAX_SHORT
include "tbtables.h"
include "tblerr.h"

# tbxcpb -- X putcol Boolean
# Write values for one column to a range of rows.  This is for data type
# Boolean and row-ordered SDAS tables.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  tbxcpt:  call sscan as a subroutine, not a function.
# Phil Hodge, 14-Sep-1994  Use tbeszt for length of string; in tbxcpt, use
#				gargd (dblbuf) and then nint for int & short.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge,  3-Mar-1998  Remove call to tbxwsk.
# Phil Hodge, 27-Aug-2002  In tbxcpi and tbxcps, include an explicit test
#	for INDEF, rather than relying on a test on abs (dblbuf).

procedure tbxcpb (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
bool	buffer[ARB]		# i: buffer for values
long	firstrow		# i: first row into which to put values
long	lastrow			# i: last row into which to put values
#--
size_t	sz_val
long	locn			# Location (chars) for reading in table
long	k			# Index in output array buffer
long	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
size_t	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]
long	tbeszt()
errchk	seek, write

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (buffer[k])
		    realbuf = real(YES)
		else
		    realbuf = real(NO)
		nchar = SZ_REAL
		call write (TB_FILE(tp), realbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (buffer[k])
		    dblbuf = double(YES)
		else
		    dblbuf = double(NO)
		nchar = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (buffer[k])
		    intbuf = YES
		else
		    intbuf = NO
		nchar = SZ_INT
		call write (TB_FILE(tp), intbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (buffer[k])
		    shortbuf = YES
		else
		    shortbuf = NO
		nchar = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		nchar = SZ_BOOL
		call write (TB_FILE(tp), buffer[k], nchar)
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    call sprintf (charbuf, SZ_LINE, "%-3b")
			call pargb (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, nchar)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptb:  invalid data type")
	    }
	}
end


# tbxcpd -- X putcol double
# Write values for one column to a range of rows.  This is for data type
# double precision and row-ordered SDAS tables.

procedure tbxcpd (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
double	buffer[ARB]		# i: buffer for values
long	firstrow		# i: first row into which to put values
long	lastrow			# i: last row into which to put values
#--
size_t	sz_val
long	locn			# Location (chars) for reading in table
long	k			# Index in output array buffer
long	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
size_t	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbeszt()
short	sdnint()
errchk	seek, write

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFD (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		nchar = SZ_REAL
		call write (TB_FILE(tp), realbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFD (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		nchar = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFD (buffer[k]) || (dabs (buffer[k]) > MAX_INT))
		    intbuf = INDEFI
		else
		    intbuf = idnint (buffer[k])
		nchar = SZ_INT
		call write (TB_FILE(tp), intbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFD (buffer[k]) || (dabs (buffer[k]) > MAX_SHORT))
		    shortbuf = INDEFS
		else
		    shortbuf = sdnint (buffer[k])
		nchar = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFD (buffer[k]) || idnint(buffer[k]) == NO)
		    boolbuf = false
		else
		    boolbuf = true
		nchar = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, nchar)
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    call sprintf (charbuf, SZ_LINE, "%-25.17g")
			call pargd (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, nchar)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptd:  invalid data type")
	    }
	}
end


# tbxcpr -- X putcol real
# Write values for one column to a range of rows.  This is for data type real
# and row-ordered SDAS tables.

procedure tbxcpr (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
real	buffer[ARB]		# i: buffer for values
long	firstrow		# i: first row into which to put values
long	lastrow			# i: last row into which to put values
#--
size_t	sz_val
long	locn			# Location (chars) for reading in table
long	k			# Index in output array buffer
long	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
size_t	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf			# Buffer for writing double-precision values
int	intbuf			# Buffer for writing integer values
short	shortbuf
bool	boolbuf			# Buffer for writing Boolean values
char	charbuf[SZ_LINE]	# Buffer for writing character values
long	tbeszt()
real	aabs()
int	inint()
short	snint()
errchk	seek, write

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		nchar = SZ_REAL
		call write (TB_FILE(tp), buffer[k], nchar)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFR (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		nchar = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFR (buffer[k]) || (aabs (buffer[k]) > MAX_INT))
		    intbuf = INDEFI
		else
		    intbuf = inint (buffer[k])
		nchar = SZ_INT
		call write (TB_FILE(tp), intbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFR (buffer[k]) || (aabs (buffer[k]) > MAX_SHORT))
		    shortbuf = INDEFS
		else
		    shortbuf = snint (buffer[k])
		nchar = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFR (buffer[k]) || inint(buffer[k]) == NO)
		    boolbuf = false
		else
		    boolbuf = true
		nchar = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, nchar)
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    call sprintf (charbuf, SZ_LINE, "%-15.7g")
			call pargr (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, nchar)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptr:  invalid data type")
	    }
	}
end


# tbxcpi -- X putcol integer
# Write values for one column to a range of rows.  This is for data type
# integer and row-ordered SDAS tables.

procedure tbxcpi (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
int	buffer[ARB]		# i: buffer for values
long	firstrow		# i: first row into which to put values
long	lastrow			# i: last row into which to put values
#--
size_t	sz_val
long	locn			# Location (chars) for reading in table
long	k			# Index in output array buffer
long	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
size_t	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf			# Buffer for writing double-precision values
real	realbuf			# Buffer for writing real values
short	shortbuf
bool	boolbuf			# Buffer for writing Boolean values
char	charbuf[SZ_LINE]	# Buffer for writing character values
long	tbeszt()
errchk	seek, write

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFI (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		nchar = SZ_REAL
		call write (TB_FILE(tp), realbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFI (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		nchar = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		nchar = SZ_INT
		call write (TB_FILE(tp), buffer[k], nchar)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFI (buffer[k]) || (iabs (buffer[k]) > MAX_SHORT))
		    shortbuf = INDEFS
		else
		    shortbuf = buffer[k]
		nchar = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFI (buffer[k]) || (buffer[k] == NO))
		    boolbuf = false
		else
		    boolbuf = true
		nchar = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, nchar)
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    call sprintf (charbuf, SZ_LINE, "%-11d")
			call pargi (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, nchar)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcpti:  invalid data type")
	    }
	}
end


# tbxcps -- X putcol short
# Write values for one column to a range of rows.  This is for data type
# short integer and row-ordered SDAS tables.

procedure tbxcps (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
short	buffer[ARB]		# i: buffer for values
long	firstrow		# i: first row into which to put values
long	lastrow			# i: last row into which to put values
#--
size_t	sz_val
long	locn			# Location (chars) for reading in table
long	k			# Index in output array buffer
long	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
size_t	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbeszt()
errchk	seek, write

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFS (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		nchar = SZ_REAL
		call write (TB_FILE(tp), realbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFS (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		nchar = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFS (buffer[k]))
		    intbuf = INDEFI
		else
		    intbuf = buffer[k]
		nchar = SZ_INT
		call write (TB_FILE(tp), intbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		nchar = SZ_SHORT
		call write (TB_FILE(tp), buffer[k], nchar)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		if (IS_INDEFS (buffer[k]) || (buffer[k] == NO))
		    boolbuf = false
		else
		    boolbuf = true
		nchar = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, nchar)
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    call sprintf (charbuf, SZ_LINE, "%-11d")
			call pargs (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, nchar)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcpts:  invalid data type")
	    }
	}
end


# tbxcpt -- X putcol text
# Write values for one column to a range of rows.  This is for character
# strings and row-ordered SDAS tables.

procedure tbxcpt (tp, colptr, buffer, lenstring, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to descriptor of the column
char	buffer[lenstring,ARB]	# i: buffer for values
int	lenstring		# i: number of char in each element of buffer
long	firstrow		# i: first row into which to put values
long	lastrow			# i: last row into which to put values
#--
size_t	sz_val
long	locn			# Location (chars) for reading in table
long	k			# Index in output array buffer
long	rowlen			# Record length (chars)
int	datatype		# Data type of element in table
size_t	nchar			# Size of a string in table file
# buffers for reading values of various types
double	dblbuf			# Buffer for reading double-precision values
real	realbuf			# Buffer for reading real values
int	intbuf			# Buffer for reading integer values
short	shortbuf
bool	boolbuf			# Buffer for reading Boolean values
char	charbuf[SZ_LINE]	# Buffer for reading character values
int	nscan()
long	tbeszt()
short	sdnint()
errchk	seek, write

begin
	rowlen = TB_ROWLEN(tp)
	datatype = COL_DTYPE(colptr)
	locn = (firstrow-1) * rowlen + TB_BOD(tp) + COL_OFFSET(colptr)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		call sscan (buffer[1,k])
		    call gargr (realbuf)
		if (nscan() < 1)
		    realbuf = INDEFR
		nchar = SZ_REAL
		call write (TB_FILE(tp), realbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		call sscan (buffer[1,k])
		    call gargd (dblbuf)
		if (nscan() < 1)
		    dblbuf = TBL_INDEFD
		else if (IS_INDEFD (dblbuf))
		    dblbuf = TBL_INDEFD
		nchar = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		call sscan (buffer[1,k])
		    call gargd (dblbuf)
		if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				dabs (dblbuf) > MAX_INT) {
		    intbuf = INDEFI
		} else {
		    intbuf = idnint (dblbuf)
		}
		nchar = SZ_INT
		call write (TB_FILE(tp), intbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		call sscan (buffer[1,k])
		    call gargd (dblbuf)
		if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				dabs (dblbuf) > MAX_SHORT) {
		    shortbuf = INDEFS
		} else {
		    shortbuf = sdnint (dblbuf)
		}
		nchar = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, nchar)
		locn = locn + rowlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), locn)
		call sscan (buffer[1,k])
		    call gargb (boolbuf)
		if (nscan() < 1)
		    boolbuf = false
		nchar = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, nchar)
		locn = locn + rowlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		nchar = tbeszt (colptr)
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), locn)
		    sz_val = lenstring
		    call strpak (buffer[1,k], charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, nchar)
		    locn = locn + rowlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptt:  invalid data type")
	    }
	}
end
