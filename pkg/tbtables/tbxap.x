include <mach.h>		# for MAX_INT, MAX_SHORT, and MAX_REAL
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# This file contains tbxap[tbirds] as well as tbxppt for writing an
# array of elements into a row ordered table.
#
# Phil Hodge, 12-Sep-1994  Subroutines created.
# Phil Hodge, 15-Dec-1994  Allocate cbuf instead of using static memory.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge,  4-Mar-1998  Remove calls to tbtwer.
# Phil Hodge,  5-Aug-1999  Use COL_NELEM instead of tbalen to get array length.
# Phil Hodge,  7-Feb-2000  In tbxapt, update TB_NROWS after calling tbxwer.
# Phil Hodge, 28-Apr-2000  Call tbxwer when writing to TB_NROWS+1, if the
#			data type is not the same as in the column.

procedure tbxapd (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	row		# i: row number
double	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
long	eoffset		# offset from BOF to element to put
long	roffset		# offset from BOF
long	offset		# offset of element from beginning of row
int	rowlen		# length of a row in char
int	dtype		# data type of column
int	ntotal		# total number of elements in array
int	nchar		# number of char to write (= nelem * SZ_DOUBLE)
int	i
bool	some_indef	# true if there are any INDEF elements in buffer
pointer cbuf		# scratch for character buffer
double	dbuf
real	rbuf		# buffer for writing single precision elements
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff(), tbxoff()
errchk	seek, write, tbxppt, tbxwer

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	if (ntotal < first+nelem-1)
	    call error (1, "tbaptd:  attempt to put too many elements")
	nchar = nelem * SZ_DOUBLE

	if (row == TB_NROWS(tp)+1 && dtype == TBL_TY_DOUBLE) {

	    # We're writing the next row after the last.

	    rowlen = TB_ROWLEN(tp)
	    roffset = tbxoff (tp, row)		# from BOF to beginning of row
	    offset = COL_OFFSET(cp)		# from beginning of row

	    call seek (TB_FILE(tp), roffset)
	    if (offset > 0)
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)], offset)

	    # Search for INDEF values in buffer, first checking the last
	    # element because INDEF is more likely to be found at the end.
	    some_indef = false			# initial value
	    if (IS_INDEFD (buffer[nelem])) {
		some_indef = true
	    } else {
		do i = 1, nelem-1 {
		    if (IS_INDEFD (buffer[i])) {
			some_indef = true
			break
		    }
		}
	    }
	    if (some_indef) {
		do i = 1, nelem {
		    if (IS_INDEFD (buffer[i]))
			dbuf = TBL_INDEFD
		    else
			dbuf = buffer[i]
		    call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		}
	    } else {
		call write (TB_FILE(tp), buffer, nchar)
	    }

	    call seek (TB_FILE(tp), roffset+offset+nchar)
	    if (offset+nchar < rowlen) {
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)+offset+nchar],
		    rowlen-(offset+nchar))
	    }

	    TB_NROWS(tp) = row

	} else {

	    # row > TB_NROWS was taken care of by tbswer1.
	    if (row == TB_NROWS(tp) + 1) {
		call tbxwer (tp, row)
		TB_NROWS(tp) = row
	    }

	    # Get the offset from BOF to first element to put, and go there.
	    eoffset = tbeoff (tp, cp, row, first)
	    call seek (TB_FILE(tp), eoffset)

	    dtype = COL_DTYPE(cp)
	    switch (dtype) {
	    case TBL_TY_REAL:
		do i = 1, nelem {		# put each element individually
		    if (IS_INDEFD (buffer[i]) || abs (buffer[i]) > MAX_REAL)
			rbuf = INDEFR
		    else
			rbuf = buffer[i]
		    call write (TB_FILE(tp), rbuf, SZ_REAL)
		}
	    case TBL_TY_DOUBLE:
		some_indef = false			# initial value
		if (IS_INDEFD (buffer[nelem])) {
		    some_indef = true
		} else {
		    do i = 1, nelem-1 {
			if (IS_INDEFD (buffer[i])) {
			    some_indef = true
			    break
			}
		    }
		}
		if (some_indef) {
		    do i = 1, nelem {
			if (IS_INDEFD (buffer[i]))
			    dbuf = TBL_INDEFD
			else
			    dbuf = buffer[i]
			call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		    }
		} else {
		    call write (TB_FILE(tp), buffer, nchar)
		}
	    case TBL_TY_INT:
		do i = 1, nelem {
		    if (IS_INDEFD (buffer[i]) || abs (buffer[i]) > MAX_INT)
			ibuf = INDEFI
		    else
			ibuf = nint (buffer[i])
		    if (SZ_INT != SZ_INT32)
			call ipak32 (ibuf, ibuf, 1)
		    call write (TB_FILE(tp), ibuf, SZ_INT32)
		}
	    case TBL_TY_SHORT:
		do i = 1, nelem {
		    if (IS_INDEFD (buffer[i]) || abs (buffer[i]) > MAX_SHORT)
			sbuf = INDEFS
		    else
			sbuf = nint (buffer[i])
		    call write (TB_FILE(tp), sbuf, SZ_SHORT)
		}
	    case TBL_TY_BOOL:
		do i = 1, nelem {
		    if (IS_INDEFD (buffer[i]))
			bbuf = false
		    else
			bbuf = (buffer[i] != double(NO))
		    call write (TB_FILE(tp), bbuf, SZ_BOOL)
		}
	    default:
		if (dtype < 0 || dtype == TY_CHAR) {
		    call smark (sp)
		    call salloc (cbuf, SZ_FNAME, TY_CHAR)
		    do i = 1, nelem {
			eoffset = tbeoff (tp, cp, row, first+i-1)
			call sprintf (Memc[cbuf], SZ_FNAME, "%.16g")
			    call pargd (buffer[i])
			call tbxppt (tp, cp, eoffset, Memc[cbuf], SZ_FNAME, 1)
		    }
		    call sfree (sp)
		} else {
		    call error (ER_TBCOLBADTYP,
			"tbaptd:  bad data type; table or memory corrupted?")
		}
	    }
	}
end

procedure tbxapr (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	row		# i: row number
real	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
long	eoffset		# offset from BOF to element to put
long	roffset		# offset from BOF
long	offset		# offset of element from beginning of row
int	rowlen		# length of a row in char
int	dtype		# data type of column
int	ntotal		# total number of elements in array
int	nchar		# number of char to write (= nelem * SZ_REAL)
int	i
pointer cbuf		# scratch for character buffer
double	dbuf		# buffer for writing double precision elements
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff(), tbxoff()
errchk	seek, write, tbxppt, tbxwer

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	if (ntotal < first+nelem-1)
	    call error (1, "tbaptr:  attempt to put too many elements")
	nchar = nelem * SZ_REAL

	if (row == TB_NROWS(tp)+1 && dtype == TBL_TY_REAL) {

	    # We're writing the next row after the last.

	    rowlen = TB_ROWLEN(tp)
	    roffset = tbxoff (tp, row)		# from BOF to beginning of row
	    offset = COL_OFFSET(cp)		# from beginning of row

	    call seek (TB_FILE(tp), roffset)
	    if (offset > 0)
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)], offset)
	    call write (TB_FILE(tp), buffer, nchar)
	    call seek (TB_FILE(tp), roffset+offset+nchar)
	    if (offset+nchar < rowlen) {
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)+offset+nchar],
		    rowlen-(offset+nchar))
	    }

	    TB_NROWS(tp) = row

	} else {

	    # row > TB_NROWS was taken care of by tbswer1.
	    if (row == TB_NROWS(tp) + 1) {
		call tbxwer (tp, row)
		TB_NROWS(tp) = row
	    }

	    # Get the offset from BOF to first element to put, and go there.
	    eoffset = tbeoff (tp, cp, row, first)
	    call seek (TB_FILE(tp), eoffset)

	    dtype = COL_DTYPE(cp)
	    switch (dtype) {
	    case TBL_TY_REAL:
		call write (TB_FILE(tp), buffer, nelem * SZ_REAL)
	    case TBL_TY_DOUBLE:
		do i = 1, nelem {		# put each element individually
		    if (IS_INDEFR (buffer[i]))
			dbuf = TBL_INDEFD
		    else
			dbuf = buffer[i]
		    call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		}
	    case TBL_TY_INT:
		do i = 1, nelem {
		    if (IS_INDEFR (buffer[i]) || abs (buffer[i]) > MAX_INT)
			ibuf = INDEFI
		    else
			ibuf = nint (buffer[i])
		    if (SZ_INT != SZ_INT32)
			call ipak32 (ibuf, ibuf, 1)
		    call write (TB_FILE(tp), ibuf, SZ_INT32)
		}
	    case TBL_TY_SHORT:
		do i = 1, nelem {
		    if (IS_INDEFR (buffer[i]) || abs (buffer[i]) > MAX_SHORT)
			sbuf = INDEFS
		    else
			sbuf = nint (buffer[i])
		    call write (TB_FILE(tp), sbuf, SZ_SHORT)
		}
	    case TBL_TY_BOOL:
		do i = 1, nelem {
		    if (IS_INDEFR (buffer[i]))
			bbuf = false
		    else
			bbuf = (buffer[i] != real(NO))
		    call write (TB_FILE(tp), bbuf, SZ_BOOL)
		}
	    default:
		if (dtype < 0 || dtype == TY_CHAR) {
		    call smark (sp)
		    call salloc (cbuf, SZ_FNAME, TY_CHAR)
		    do i = 1, nelem {
			eoffset = tbeoff (tp, cp, row, first+i-1)
			call sprintf (Memc[cbuf], SZ_FNAME, "%.7g")
			    call pargr (buffer[i])
			call tbxppt (tp, cp, eoffset, Memc[cbuf], SZ_FNAME, 1)
		    }
		    call sfree (sp)
		} else {
		    call error (ER_TBCOLBADTYP,
			"tbaptr:  bad data type; table or memory corrupted?")
		}
	    }
	}
end

procedure tbxapi (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	row		# i: row number
int	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
long	eoffset		# offset from BOF to element to put
long	roffset		# offset from BOF
long	offset		# offset of element from beginning of row
int	rowlen		# length of a row in char
int	dtype		# data type of column
int	ntotal		# total number of elements in array
int	nchar		# number of char to write (= nelem * SZ_INT32)
int	i
pointer cbuf		# scratch for character buffer
double	dbuf		# buffer for writing double precision elements
real	rbuf
short	sbuf
bool	bbuf
long	tbeoff(), tbxoff()
errchk	seek, write, tbxppt, tbxwer

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	if (ntotal < first+nelem-1)
	    call error (1, "tbapti:  attempt to put too many elements")
	nchar = nelem * SZ_INT32

	if (row == TB_NROWS(tp)+1 && dtype == TBL_TY_INT) {

	    # We're writing the next row after the last.

	    rowlen = TB_ROWLEN(tp)
	    roffset = tbxoff (tp, row)		# from BOF to beginning of row
	    offset = COL_OFFSET(cp)		# from beginning of row

	    call seek (TB_FILE(tp), roffset)
	    if (offset > 0)
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)], offset)
	    call write (TB_FILE(tp), buffer, nchar)
	    call seek (TB_FILE(tp), roffset+offset+nchar)
	    if (offset+nchar < rowlen) {
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)+offset+nchar],
		    rowlen-(offset+nchar))
	    }

	    TB_NROWS(tp) = row

	} else {

	    # row > TB_NROWS was taken care of by tbswer1.
	    if (row == TB_NROWS(tp) + 1) {
		call tbxwer (tp, row)
		TB_NROWS(tp) = row
	    }

	    # Get the offset from BOF to first element to put, and go there.
	    eoffset = tbeoff (tp, cp, row, first)
	    call seek (TB_FILE(tp), eoffset)

	    dtype = COL_DTYPE(cp)
	    switch (dtype) {
	    case TBL_TY_REAL:
		do i = 1, nelem {		# put each element individually
		    if (IS_INDEFI (buffer[i]))
			rbuf = INDEFR
		    else
			rbuf = buffer[i]
		    call write (TB_FILE(tp), rbuf, SZ_REAL)
		}
	    case TBL_TY_DOUBLE:
		do i = 1, nelem {		# put each element individually
		    if (IS_INDEFI (buffer[i]))
			dbuf = TBL_INDEFD
		    else
			dbuf = buffer[i]
		    call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		}
	    case TBL_TY_INT:
		call write (TB_FILE(tp), buffer, nelem * SZ_INT32)
	    case TBL_TY_SHORT:
		do i = 1, nelem {
		    if (IS_INDEFI (buffer[i]) || abs (buffer[i]) > MAX_SHORT)
			sbuf = INDEFS
		    else
			sbuf = buffer[i]
		    call write (TB_FILE(tp), sbuf, SZ_SHORT)
		}
	    case TBL_TY_BOOL:
		do i = 1, nelem {
		    if (IS_INDEFI (buffer[i]))
			bbuf = false
		    else
			bbuf = (buffer[i] != NO)
		    call write (TB_FILE(tp), bbuf, SZ_BOOL)
		}
	    default:
		if (dtype < 0 || dtype == TY_CHAR) {
		    call smark (sp)
		    call salloc (cbuf, SZ_FNAME, TY_CHAR)
		    do i = 1, nelem {
			eoffset = tbeoff (tp, cp, row, first+i-1)
			call sprintf (Memc[cbuf], SZ_FNAME, "%d")
			    call pargi (buffer[i])
			call tbxppt (tp, cp, eoffset, Memc[cbuf], SZ_FNAME, 1)
		    }
		    call sfree (sp)
		} else {
		    call error (ER_TBCOLBADTYP,
			"tbapti:  bad data type; table or memory corrupted?")
		}
	    }
	}
end

procedure tbxaps (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	row		# i: row number
short	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
long	eoffset		# offset from BOF to element to put
long	roffset		# offset from BOF
long	offset		# offset of element from beginning of row
int	rowlen		# length of a row in char
int	dtype		# data type of column
int	ntotal		# total number of elements in array
int	nchar		# number of char to write (= nelem * SZ_SHORT)
int	i
pointer cbuf		# scratch for character buffer
double	dbuf		# buffer for writing double precision elements
real	rbuf
int	ibuf
bool	bbuf
long	tbeoff(), tbxoff()
errchk	seek, write, tbxppt, tbxwer

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	if (ntotal < first+nelem-1)
	    call error (1, "tbapts:  attempt to put too many elements")
	nchar = nelem * SZ_SHORT

	if (row == TB_NROWS(tp)+1 && dtype == TBL_TY_SHORT) {

	    # We're writing the next row after the last.

	    rowlen = TB_ROWLEN(tp)
	    roffset = tbxoff (tp, row)		# from BOF to beginning of row
	    offset = COL_OFFSET(cp)		# from beginning of row

	    call seek (TB_FILE(tp), roffset)
	    if (offset > 0)
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)], offset)
	    call write (TB_FILE(tp), buffer, nchar)
	    call seek (TB_FILE(tp), roffset+offset+nchar)
	    if (offset+nchar < rowlen) {
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)+offset+nchar],
		    rowlen-(offset+nchar))
	    }

	    TB_NROWS(tp) = row

	} else {

	    # row > TB_NROWS was taken care of by tbswer1.
	    if (row == TB_NROWS(tp) + 1) {
		call tbxwer (tp, row)
		TB_NROWS(tp) = row
	    }

	    # Get the offset from BOF to first element to put, and go there.
	    eoffset = tbeoff (tp, cp, row, first)
	    call seek (TB_FILE(tp), eoffset)

	    dtype = COL_DTYPE(cp)
	    switch (dtype) {
	    case TBL_TY_REAL:
		do i = 1, nelem {		# put each element individually
		    if (IS_INDEFS (buffer[i]))
			rbuf = INDEFR
		    else
			rbuf = buffer[i]
		    call write (TB_FILE(tp), rbuf, SZ_REAL)
		}
	    case TBL_TY_DOUBLE:
		do i = 1, nelem {		# put each element individually
		    if (IS_INDEFS (buffer[i]))
			dbuf = TBL_INDEFD
		    else
			dbuf = buffer[i]
		    call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		}
	    case TBL_TY_INT:
		do i = 1, nelem {
		    if (IS_INDEFS (buffer[i]))
			ibuf = INDEFI
		    else
			ibuf = buffer[i]
		    if (SZ_INT != SZ_INT32)
			call ipak32 (ibuf, ibuf, 1)
		    call write (TB_FILE(tp), ibuf, SZ_INT32)
		}
	    case TBL_TY_SHORT:
		call write (TB_FILE(tp), buffer, nelem * SZ_SHORT)
	    case TBL_TY_BOOL:
		do i = 1, nelem {
		    if (IS_INDEFS (buffer[i]))
			bbuf = false
		    else
			bbuf = (buffer[i] != NO)
		    call write (TB_FILE(tp), bbuf, SZ_BOOL)
		}
	    default:
		if (dtype < 0 || dtype == TY_CHAR) {
		    call smark (sp)
		    call salloc (cbuf, SZ_FNAME, TY_CHAR)
		    do i = 1, nelem {
			eoffset = tbeoff (tp, cp, row, first+i-1)
			call sprintf (Memc[cbuf], SZ_FNAME, "%d")
			    call pargs (buffer[i])
			call tbxppt (tp, cp, eoffset, Memc[cbuf], SZ_FNAME, 1)
		    }
		    call sfree (sp)
		} else {
		    call error (ER_TBCOLBADTYP,
			"tbapts:  bad data type; table or memory corrupted?")
		}
	    }
	}
end

procedure tbxapb (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	row		# i: row number
bool	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
long	eoffset		# offset from BOF to element to put
long	roffset		# offset from BOF
long	offset		# offset of element from beginning of row
int	rowlen		# length of a row in char
int	dtype		# data type of column
int	ntotal		# total number of elements in array
int	nchar		# number of char to write (= nelem * SZ_BOOL)
int	i
pointer cbuf		# scratch for character buffer
double	dbuf		# buffer for writing double precision elements
real	rbuf
int	ibuf
short	sbuf
long	tbeoff(), tbxoff()
errchk	seek, write, tbxppt, tbxwer

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	if (ntotal < first+nelem-1)
	    call error (1, "tbaptb:  attempt to put too many elements")
	nchar = nelem * SZ_BOOL

	if (row == TB_NROWS(tp)+1 && dtype == TBL_TY_BOOL) {

	    # We're writing the next row after the last.

	    rowlen = TB_ROWLEN(tp)
	    roffset = tbxoff (tp, row)		# from BOF to beginning of row
	    offset = COL_OFFSET(cp)		# from beginning of row

	    call seek (TB_FILE(tp), roffset)
	    if (offset > 0)
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)], offset)
	    call write (TB_FILE(tp), buffer, nchar)
	    call seek (TB_FILE(tp), roffset+offset+nchar)
	    if (offset+nchar < rowlen) {
		call write (TB_FILE(tp), Memc[TB_INDEF(tp)+offset+nchar],
		    rowlen-(offset+nchar))
	    }

	    TB_NROWS(tp) = row

	} else {

	    # row > TB_NROWS was taken care of by tbswer1.
	    if (row == TB_NROWS(tp) + 1) {
		call tbxwer (tp, row)
		TB_NROWS(tp) = row
	    }

	    # Get the offset from BOF to first element to put, and go there.
	    eoffset = tbeoff (tp, cp, row, first)
	    call seek (TB_FILE(tp), eoffset)

	    dtype = COL_DTYPE(cp)
	    switch (dtype) {
	    case TBL_TY_REAL:
		do i = 1, nelem {		# put each element individually
		    if (buffer[i])
			rbuf = real(YES)
		    else
			rbuf = real(NO)
		    call write (TB_FILE(tp), rbuf, SZ_REAL)
		}
	    case TBL_TY_DOUBLE:
		do i = 1, nelem {		# put each element individually
		    if (buffer[i])
			dbuf = double(YES)
		    else
			dbuf = double(NO)
		    call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		}
	    case TBL_TY_INT:
		do i = 1, nelem {
		    if (buffer[i])
			ibuf = YES
		    else
			ibuf = NO
		    if (SZ_INT != SZ_INT32)
			call ipak32 (ibuf, ibuf, 1)
		    call write (TB_FILE(tp), ibuf, SZ_INT32)
		}
	    case TBL_TY_SHORT:
		do i = 1, nelem {
		    if (buffer[i])
			sbuf = YES
		    else
			sbuf = NO
		    call write (TB_FILE(tp), sbuf, SZ_SHORT)
		}
	    case TBL_TY_BOOL:
		call write (TB_FILE(tp), buffer, nelem * SZ_BOOL)
	    default:
		if (dtype < 0 || dtype == TY_CHAR) {
		    call smark (sp)
		    call salloc (cbuf, SZ_FNAME, TY_CHAR)
		    do i = 1, nelem {
			eoffset = tbeoff (tp, cp, row, first+i-1)
			call sprintf (Memc[cbuf], SZ_FNAME, "%-3b")
			    call pargb (buffer[i])
			call tbxppt (tp, cp, eoffset, Memc[cbuf], SZ_FNAME, 1)
		    }
		    call sfree (sp)
		} else {
		    call error (ER_TBCOLBADTYP,
			"tbaptb:  bad data type; table or memory corrupted?")
		}
	    }
	}
end

procedure tbxapt (tp, cp, row, cbuf, maxch, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	row		# i: row number
char	cbuf[maxch,ARB]	# i: values to write to table
int	maxch		# i: size of first dimension of cbuf
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
long	offset		# offset of first element in entry
int	dtype		# data type of column
int	ntotal		# total number of elements in array
int	i
double	dbuf		# buffer for writing double precision elements
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
int	nscan()
long	tbeoff()
errchk	tbxppt, tbxwer

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	if (ntotal < first+nelem-1)
	    call error (1, "tbaptt:  attempt to put too many elements")

	if (row > TB_NROWS(tp)) {
	    call tbxwer (tp, row)
	    TB_NROWS(tp) = row
	}

	offset = tbeoff (tp, cp, row, first)

	if (dtype < 0 || dtype == TBL_TY_CHAR) {

	    call tbxppt (tp, cp, offset, cbuf, maxch, nelem)

	} else {

	    call seek (TB_FILE(tp), offset)

	    dtype = COL_DTYPE(cp)
	    switch (dtype) {
	    case TBL_TY_REAL:
		do i = 1, nelem {		# put each element individually
		    call sscan (cbuf[1,i])
			call gargd (dbuf)
		    if (nscan() < 1)
			rbuf = TBL_INDEFD
		    else if (IS_INDEFD (dbuf) || abs (dbuf) > MAX_REAL)
			rbuf = INDEFR
		    else
			rbuf = dbuf
		    call write (TB_FILE(tp), rbuf, SZ_REAL)
		}
	    case TBL_TY_DOUBLE:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargd (dbuf)
		    if (nscan() < 1)
			dbuf = TBL_INDEFD
		    else if (IS_INDEFD (dbuf))
			dbuf = TBL_INDEFD
		    call write (TB_FILE(tp), dbuf, SZ_DOUBLE)
		}
	    case TBL_TY_INT:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargd (dbuf)
		    if (nscan() < 1 || abs (dbuf) > MAX_INT)
			ibuf = INDEFI
		    else
			ibuf = nint (dbuf)
		    if (SZ_INT != SZ_INT32)
			call ipak32 (ibuf, ibuf, 1)
		    call write (TB_FILE(tp), ibuf, SZ_INT32)
		}
	    case TBL_TY_SHORT:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargd (dbuf)
		    if (nscan() < 1 || abs (dbuf) > MAX_SHORT)
			sbuf = INDEFS
		    else
			sbuf = nint (dbuf)
		    call write (TB_FILE(tp), sbuf, SZ_SHORT)
		}
	    case TBL_TY_BOOL:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargb (bbuf)
		    if (nscan() < 1)
			bbuf = false
		    call write (TB_FILE(tp), bbuf, SZ_BOOL)
		}
	    }
	}
end

# tbxppt -- primitive put array text

procedure tbxppt (tp, cp, offset, cbuf, maxch, nelem)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
long	offset			# i: offset in char to first location
char	cbuf[maxch,nelem]	# i: buffer containing values
int	maxch			# i: size of each element of array
int	nelem			# i: number of elements to put
#--
char	buffer[SZ_LINE]		# buffer for packed string
long	eoffset			# location in char for writing
int	nchar			# number of char to write
int	i
int	tbeszt()
errchk	seek, write

begin
	nchar = min (tbeszt (cp), SZ_LINE)	# size of each element
	eoffset = offset			# an initial value

	do i = 1, nelem {			# do for each element

	    call strpak (cbuf[1,i], buffer, SZ_LINE)	# pack the string

	    call seek (TB_FILE(tp), eoffset)
	    call write (TB_FILE(tp), buffer, nchar)

	    eoffset = eoffset + nchar
	}
end
