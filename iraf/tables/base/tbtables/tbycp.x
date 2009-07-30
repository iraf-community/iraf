include <mach.h>		# for MAX_INT and MAX_SHORT
include "tbtables.h"
include "tblerr.h"

# tbycpb -- Y putcol Boolean
# Write values for one column to a range of rows.  This is for data type
# Boolean and column-ordered SDAS tables.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge,  4-Nov-1993  tbycpt:  call sscan as a subroutine, not a function.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge,  5-Mar-1998  Remove nrows from tbycpd and tbycpt;
#			remove calls to tbytsz, and don't update TB_NROWS,
#			as these are taken care of at a higher level.

procedure tbycpb (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
bool	buffer[ARB]		# Buffer for values
long	firstrow		# Number of first row into which to put values
long	lastrow			# Number of last row into which to put values
#--
size_t	sz_val
long	offset			# Location (chars) for reading in table
long	k			# Index in output array buffer
size_t	nrows			# Number of rows to write
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write, sprintf

begin
	nrows = lastrow - firstrow + 1
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		if (buffer[k])
		    realbuf = real(YES)
		else
		    realbuf = real(NO)
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_REAL
		call write (TB_FILE(tp), realbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		if (buffer[k])
		    dblbuf = double(YES)
		else
		    dblbuf = double(NO)
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		if (buffer[k])
		    intbuf = YES
		else
		    intbuf = NO
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_INT
		call write (TB_FILE(tp), intbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		if (buffer[k])
		    shortbuf = YES
		else
		    shortbuf = NO
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_BOOL:
	    call seek (TB_FILE(tp), offset)
	    call write (TB_FILE(tp), buffer, nrows*SZ_BOOL)
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call sprintf (charbuf, SZ_LINE, "%-3b")
			call pargb (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call seek (TB_FILE(tp), offset)
		    call write (TB_FILE(tp), charbuf, dlen)
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptb:  invalid data type")
	    }
	}
end


# tbycpd -- Y putcol double
# Write values for one column to a range of rows.  This is for data type
# double precision and column-ordered SDAS tables.

procedure tbycpd (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
double	buffer[ARB]		# Buffer for values
long	firstrow		# Number of first row into which to put values
long	lastrow			# Number of last row into which to put values
#--
size_t	sz_val
long	offset			# Location (chars) for reading in table
long	k			# Index in output array buffer
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
short	sdnint()
errchk	seek, write, sprintf

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFD (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_REAL
		call write (TB_FILE(tp), realbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFD (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFD (buffer[k]) || (dabs (buffer[k]) > MAX_INT))
		    intbuf = INDEFI
		else
		    intbuf = idnint (buffer[k])
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_INT
		call write (TB_FILE(tp), intbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFD (buffer[k]) || (dabs (buffer[k]) > MAX_SHORT))
		    shortbuf = INDEFS
		else
		    shortbuf = sdnint (buffer[k])
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFD (buffer[k]) || (idnint (buffer[k]) == NO))
		    boolbuf = false
		else
		    boolbuf = true
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, sz_val)
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call sprintf (charbuf, SZ_LINE, "%-25.17g")
			call pargd (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call seek (TB_FILE(tp), offset)
		    call write (TB_FILE(tp), charbuf, dlen)
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptd:  invalid data type")
	    }
	}
end


# tbycpr -- Y putcol real
# Write values for one column to a range of rows.  This is for data type real
# and column-ordered SDAS tables.

procedure tbycpr (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
real	buffer[ARB]		# Buffer for values
long	firstrow		# Number of first row into which to put values
long	lastrow			# Number of last row into which to put values
#--
size_t	sz_val
long	offset			# Location (chars) for reading in table
long	k			# Index in output array buffer
size_t	nrows			# Number of rows to write
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
# buffers for copying elements of various types
double	dblbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
real	aabs()
int	inint()
short	snint()
errchk	seek, write, sprintf

begin
	nrows = lastrow - firstrow + 1
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)

	switch (datatype) {
	case TY_REAL:
	    call seek (TB_FILE(tp), offset)
	    call write (TB_FILE(tp), buffer, nrows*SZ_REAL)
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFR (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFR (buffer[k]) || (aabs (buffer[k]) > MAX_INT))
		    intbuf = INDEFI
		else
		    intbuf = inint (buffer[k])
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_INT
		call write (TB_FILE(tp), intbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFR (buffer[k]) || (aabs (buffer[k]) > MAX_SHORT))
		    shortbuf = INDEFS
		else
		    shortbuf = snint (buffer[k])
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFR (buffer[k]) || (inint (buffer[k]) == NO))
		    boolbuf = false
		else
		    boolbuf = true
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, sz_val)
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call sprintf (charbuf, SZ_LINE, "%-15.7g")
			call pargr (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call seek (TB_FILE(tp), offset)
		    call write (TB_FILE(tp), charbuf, dlen)
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptr:  invalid data type")
	    }
	}
end


# tbycpi -- Y putcol integer
# Write values for one column to a range of rows.  This is for data type
# integer and column-ordered SDAS tables.

procedure tbycpi (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
int	buffer[ARB]		# Buffer for values
long	firstrow		# Number of first row into which to put values
long	lastrow			# Number of last row into which to put values
#--
size_t	sz_val
long	offset			# Location (chars) for reading in table
long	k			# Index in output array buffer
size_t	nrows			# Number of rows to write
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write, sprintf

begin
	nrows = lastrow - firstrow + 1
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFI (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_REAL
		call write (TB_FILE(tp), realbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFI (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_INT:
	    call seek (TB_FILE(tp), offset)
	    call write (TB_FILE(tp), buffer, nrows*SZ_INT)
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFI (buffer[k]) || (iabs (buffer[k]) > MAX_SHORT))
		    shortbuf = INDEFS
		else
		    shortbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFI (buffer[k]) || (buffer[k] == NO))
		    boolbuf = false
		else
		    boolbuf = true
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, sz_val)
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call sprintf (charbuf, SZ_LINE, "%-11d")
			call pargi (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call seek (TB_FILE(tp), offset)
		    call write (TB_FILE(tp), charbuf, dlen)
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcpti:  invalid data type")
	    }
	}
end


# tbycps -- Y putcol short
# Write values for one column to a range of rows.  This is for data type
# short integer and column-ordered SDAS tables.

procedure tbycps (tp, colptr, buffer, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
short	buffer[ARB]		# Buffer for values
long	firstrow		# Number of first row into which to put values
long	lastrow			# Number of last row into which to put values
#--
size_t	sz_val
long	offset			# Location (chars) for reading in table
long	k			# Index in output array buffer
size_t	nrows			# Number of rows to write
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write, sprintf

begin
	nrows = lastrow - firstrow + 1
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFS (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_REAL
		call write (TB_FILE(tp), realbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFS (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFS (buffer[k]))
		    intbuf = INDEFI
		else
		    intbuf = buffer[k]
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_INT
		call write (TB_FILE(tp), intbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_SHORT:
	    call seek (TB_FILE(tp), offset)
	    call write (TB_FILE(tp), buffer, nrows*SZ_SHORT)
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		if (IS_INDEFS (buffer[k]) || (buffer[k] == NO))
		    boolbuf = false
		else
		    boolbuf = true
		call seek (TB_FILE(tp), offset)
		sz_val = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, sz_val)
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call sprintf (charbuf, SZ_LINE, "%-11d")
			call pargs (buffer[k])
		    sz_val = SZ_LINE
		    call strpak (charbuf, charbuf, sz_val)
		    call seek (TB_FILE(tp), offset)
		    call write (TB_FILE(tp), charbuf, dlen)
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcpts:  invalid data type")
	    }
	}
end


# tbycpt -- Y putcol text
# Write values for one column to a range of rows.  This is for character
# strings and column-ordered SDAS tables.

procedure tbycpt (tp, colptr, buffer, lenstring, firstrow, lastrow)

pointer tp			# Pointer to table descriptor
pointer colptr			# Pointer to descriptor of the column
char	buffer[lenstring,ARB]	# Buffer for values
int	lenstring		# The number of char in each element of buffer
long	firstrow		# Number of first row into which to put values
long	lastrow			# Number of last row into which to put values
#--
size_t	sz_val
long	offset			# Location (chars) for reading in table
long	k			# Index in output array buffer
int	datatype		# Data type of element in table
size_t	dlen			# Number of char for one element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
int	nscan()
errchk	seek, write

begin
	datatype = COL_DTYPE(colptr)
	dlen = COL_LEN(colptr)
	offset = tbyoff (tp, colptr, firstrow)

	switch (datatype) {
	case TY_REAL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		call sscan (buffer[1,k])
		    call gargr (realbuf)
		    if (nscan() < 1)
			realbuf = INDEFR
		sz_val = SZ_REAL
		call write (TB_FILE(tp), realbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_DOUBLE:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		call sscan (buffer[1,k])
		    call gargd (dblbuf)
		    if (nscan() < 1)
			dblbuf = TBL_INDEFD
		    else if (IS_INDEFD (dblbuf))
			dblbuf = TBL_INDEFD
		sz_val = SZ_DOUBLE
		call write (TB_FILE(tp), dblbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_INT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		call sscan (buffer[1,k])
		    call gargi (intbuf)
		    if (nscan() < 1)
			intbuf = INDEFI
		sz_val = SZ_INT
		call write (TB_FILE(tp), intbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_SHORT:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		call sscan (buffer[1,k])
		    call gargs (shortbuf)
		    if (nscan() < 1)
			shortbuf = INDEFS
		sz_val = SZ_SHORT
		call write (TB_FILE(tp), shortbuf, sz_val)
		offset = offset + dlen
	    }
	case TY_BOOL:
	    do k = 1, lastrow-firstrow+1 {
		call seek (TB_FILE(tp), offset)
		call sscan (buffer[1,k])
		    call gargb (boolbuf)
		    if (nscan() < 1)
			boolbuf = false
		sz_val = SZ_BOOL
		call write (TB_FILE(tp), boolbuf, sz_val)
		offset = offset + dlen
	    }
	default:
	    if (datatype < 0 || datatype == TY_CHAR) {
		do k = 1, lastrow-firstrow+1 {
		    call seek (TB_FILE(tp), offset)
		    sz_val = lenstring
		    call strpak (buffer[1,k], charbuf, sz_val)
		    call write (TB_FILE(tp), charbuf, dlen)
		    offset = offset + dlen
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbcptt:  invalid data type")
	    }
	}
end
