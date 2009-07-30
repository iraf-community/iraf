include <mach.h>		# for MAX_INT, MAX_SHORT, and MAX_REAL
include <tbset.h>
include "tbtables.h"

# This file contains tbxag[tbirds] as well as tbxgpt for getting an
# array of elements from a row ordered table.
#
# Phil Hodge, 12-Sep-1994  Subroutines created.
# Phil Hodge,  2-Jun-1997  Replace IS_INDEFD with TBL_IS_INDEFD.
# Phil Hodge,  5-Aug-1999  Use COL_NELEM instead of tbalen to get array length.

long procedure tbxagd (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
long	row		# i: row number
double	buffer[ARB]	# o: values read from table
long	first		# i: number of first array element to read
long	nelem		# i: maximum number of elements to read
#--
size_t	sz_val
long	l_val
pointer sp
pointer buf		# scratch for local string buffer
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
long	offset		# offset of first element in entry
int	dtype		# data type of column
long	ntotal		# total number of elements in array
long	nret		# actual number of elements to read
size_t	nchar		# number of char to read
long	i		# loop index
int	nscan()
long	tbeoff(), read()
string	CANNOTREAD	"tbagtd:  unexpected end of file"
errchk	seek, read, tbxgpt

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)
	offset = tbeoff (tp, cp, row, first)
	call seek (TB_FILE(tp), offset)

	if (dtype == TBL_TY_DOUBLE) {

	    nchar = nret * SZ_DOUBLE
	    if (read (TB_FILE(tp), buffer, nchar) < nchar)
		call error (1, CANNOTREAD)
	    do i = 1, nret {
		if (TBL_IS_INDEFD (buffer[i]))
		    buffer[i] = INDEFD
	    }

	} else {

	    switch (dtype) {
	    case TBL_TY_REAL:
		nchar = SZ_REAL
		do i = 1, nret {
		    if (read (TB_FILE(tp), rbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFR(rbuf))
			buffer[i] = INDEFD
		    else
			buffer[i] = rbuf
		}
	    case TBL_TY_INT:
		nchar = SZ_INT
		do i = 1, nret {
		    if (read (TB_FILE(tp), ibuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFI(ibuf))
			buffer[i] = INDEFD
		    else
			buffer[i] = ibuf
		}
	    case TBL_TY_SHORT:
		nchar = SZ_SHORT
		do i = 1, nret {
		    if (read (TB_FILE(tp), sbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFS(sbuf))
			buffer[i] = INDEFD
		    else
			buffer[i] = sbuf
		}
	    case TBL_TY_BOOL:
		nchar = SZ_BOOL
		do i = 1, nret {
		    if (read (TB_FILE(tp), bbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (bbuf)
			buffer[i] = double(YES)
		    else
			buffer[i] = double(NO)
		}
	    default:
		if (dtype > 0 && dtype != TBL_TY_CHAR)
		    call error (1, "tbagtd:  bad data type")
		call smark (sp)
		sz_val = SZ_LINE
		call salloc (buf, sz_val, TY_CHAR)
		do i = 1, nret {
		    offset = tbeoff (tp, cp, row, first+i-1)
		    l_val = 1
		    call tbxgpt (tp, cp, offset, Memc[buf], SZ_LINE, l_val)
		    call sscan (Memc[buf])
			call gargd (buffer[i])
		    if (nscan() < 1)
			buffer[i] = INDEFD
		}
		call sfree (sp)
	    }
	}

	return (nret)
end

long procedure tbxagr (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
long	row		# i: row number
real	buffer[ARB]	# o: values read from table
long	first		# i: number of first array element to read
long	nelem		# i: maximum number of elements to read
#--
size_t	sz_val
long	l_val
pointer sp
pointer buf		# scratch for local string buffer
double	dbuf
int	ibuf
short	sbuf
bool	bbuf
long	offset		# offset of first element in entry
int	dtype		# data type of column
long	ntotal		# total number of elements in array
long	nret		# actual number of elements to read
size_t	nchar		# number of char to read
long	i		# loop index
int	nscan()
long	tbeoff(), read()
string	CANNOTREAD	"tbagtr:  unexpected end of file"
errchk	seek, read, tbxgpt

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)
	offset = tbeoff (tp, cp, row, first)
	call seek (TB_FILE(tp), offset)

	if (dtype == TBL_TY_REAL) {

	    nchar = nret * SZ_REAL
	    if (read (TB_FILE(tp), buffer, nchar) < nchar)
		call error (1, CANNOTREAD)

	} else {

	    switch (dtype) {
	    case TBL_TY_DOUBLE:
		nchar = SZ_DOUBLE
		do i = 1, nret {
		    if (read (TB_FILE(tp), dbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (TBL_IS_INDEFD (dbuf))
			buffer[i] = INDEFR
		    else
			buffer[i] = dbuf
		}
	    case TBL_TY_INT:
		nchar = SZ_INT
		do i = 1, nret {
		    if (read (TB_FILE(tp), ibuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFI(ibuf))
			buffer[i] = INDEFR
		    else
			buffer[i] = ibuf
		}
	    case TBL_TY_SHORT:
		nchar = SZ_SHORT
		do i = 1, nret {
		    if (read (TB_FILE(tp), sbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFS(sbuf))
			buffer[i] = INDEFR
		    else
			buffer[i] = sbuf
		}
	    case TBL_TY_BOOL:
		nchar = SZ_BOOL
		do i = 1, nret {
		    if (read (TB_FILE(tp), bbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (bbuf)
			buffer[i] = real(YES)
		    else
			buffer[i] = real(NO)
		}
	    default:
		if (dtype > 0 && dtype != TBL_TY_CHAR)
		    call error (1, "tbagtr:  bad data type")
		call smark (sp)
		sz_val = SZ_LINE
		call salloc (buf, sz_val, TY_CHAR)
		do i = 1, nret {
		    offset = tbeoff (tp, cp, row, first+i-1)
		    l_val = 1
		    call tbxgpt (tp, cp, offset, Memc[buf], SZ_LINE, l_val)
		    call sscan (Memc[buf])
			call gargd (dbuf)
		    if (nscan() < 1)
			buffer[i] = INDEFR
		    else if (dabs (dbuf) > MAX_REAL)
			buffer[i] = INDEFR
		    else
			buffer[i] = dbuf
		}
		call sfree (sp)
	    }
	}

	return (nret)
end

long procedure tbxagi (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
long	row		# i: row number
int	buffer[ARB]	# o: values read from table
long	first		# i: number of first array element to read
long	nelem		# i: maximum number of elements to read
#--
size_t	sz_val
long	l_val
pointer sp
pointer buf		# scratch for local string buffer
double	dbuf
real	rbuf
short	sbuf
bool	bbuf
long	offset		# offset of first element in entry
int	dtype		# data type of column
long	ntotal		# total number of elements in array
long	nret		# actual number of elements to read
size_t	nchar		# number of char to read
long	i		# loop index
int	nscan(), inint()
long	tbeoff(), read()
real	aabs()
string	CANNOTREAD	"tbagti:  unexpected end of file"
errchk	seek, read, tbxgpt

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)
	offset = tbeoff (tp, cp, row, first)
	call seek (TB_FILE(tp), offset)

	if (dtype == TBL_TY_INT) {

	    nchar = nret * SZ_INT
	    if (read (TB_FILE(tp), buffer, nchar) < nchar)
		call error (1, CANNOTREAD)

	} else {

	    switch (dtype) {
	    case TBL_TY_DOUBLE:
		nchar = SZ_DOUBLE
		do i = 1, nret {
		    if (read (TB_FILE(tp), dbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (TBL_IS_INDEFD (dbuf) || dabs (dbuf) > MAX_INT)
			buffer[i] = INDEFI
		    else
			buffer[i] = idnint (dbuf)
		}
	    case TBL_TY_REAL:
		nchar = SZ_REAL
		do i = 1, nret {
		    if (read (TB_FILE(tp), rbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFR(rbuf) || aabs (rbuf) > MAX_INT)
			buffer[i] = INDEFI
		    else
			buffer[i] = inint (rbuf)
		}
	    case TBL_TY_SHORT:
		nchar = SZ_SHORT
		do i = 1, nret {
		    if (read (TB_FILE(tp), sbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFS(sbuf))
			buffer[i] = INDEFI
		    else
			buffer[i] = sbuf
		}
	    case TBL_TY_BOOL:
		nchar = SZ_BOOL
		do i = 1, nret {
		    if (read (TB_FILE(tp), bbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (bbuf)
			buffer[i] = YES
		    else
			buffer[i] = NO
		}
	    default:
		if (dtype > 0 && dtype != TBL_TY_CHAR)
		    call error (1, "tbagti:  bad data type")
		call smark (sp)
		sz_val = SZ_LINE
		call salloc (buf, sz_val, TY_CHAR)
		do i = 1, nret {
		    offset = tbeoff (tp, cp, row, first+i-1)
		    l_val = 1
		    call tbxgpt (tp, cp, offset, Memc[buf], SZ_LINE, l_val)
		    call sscan (Memc[buf])
			call gargd (dbuf)
		    if (nscan() < 1 || dabs (dbuf) > MAX_INT)
			buffer[i] = INDEFI
		    else
			buffer[i] = idnint (dbuf)
		}
		call sfree (sp)
	    }
	}

	return (nret)
end

long procedure tbxags (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
long	row		# i: row number
short	buffer[ARB]	# o: values read from table
long	first		# i: number of first array element to read
long	nelem		# i: maximum number of elements to read
#--
size_t	sz_val
long	l_val
pointer sp
pointer buf		# scratch for local string buffer
double	dbuf
real	rbuf
int	ibuf
bool	bbuf
long	offset		# offset of first element in entry
int	dtype		# data type of column
long	ntotal		# total number of elements in array
long	nret		# actual number of elements to read
size_t	nchar		# number of char to read
long	i		# loop index
int	nscan()
long	tbeoff(), read()
short	snint(), sdnint()
real	aabs()
string	CANNOTREAD	"tbagts:  unexpected end of file"
errchk	seek, read, tbxgpt

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)
	offset = tbeoff (tp, cp, row, first)
	call seek (TB_FILE(tp), offset)

	if (dtype == TBL_TY_SHORT) {

	    nchar = nret * SZ_SHORT
	    if (read (TB_FILE(tp), buffer, nchar) < nchar)
		call error (1, CANNOTREAD)

	} else {

	    switch (dtype) {
	    case TBL_TY_DOUBLE:
		nchar = SZ_DOUBLE
		do i = 1, nret {
		    if (read (TB_FILE(tp), dbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (TBL_IS_INDEFD (dbuf) || dabs (dbuf) > MAX_SHORT)
			buffer[i] = INDEFS
		    else
			buffer[i] = sdnint (dbuf)
		}
	    case TBL_TY_REAL:
		nchar = SZ_REAL
		do i = 1, nret {
		    if (read (TB_FILE(tp), rbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFR(rbuf) || aabs (rbuf) > MAX_SHORT)
			buffer[i] = INDEFS
		    else
			buffer[i] = snint (rbuf)
		}
	    case TBL_TY_INT:
		nchar = SZ_INT
		do i = 1, nret {
		    if (read (TB_FILE(tp), ibuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFI(ibuf) || iabs (ibuf) > MAX_SHORT)
			buffer[i] = INDEFS
		    else
			buffer[i] = ibuf
		}
	    case TBL_TY_BOOL:
		nchar = SZ_BOOL
		do i = 1, nret {
		    if (read (TB_FILE(tp), bbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (bbuf)
			buffer[i] = YES
		    else
			buffer[i] = NO
		}
	    default:
		if (dtype > 0 && dtype != TBL_TY_CHAR)
		    call error (1, "tbagts:  bad data type")
		call smark (sp)
		sz_val = SZ_LINE
		call salloc (buf, sz_val, TY_CHAR)
		do i = 1, nret {
		    offset = tbeoff (tp, cp, row, first+i-1)
		    l_val = 1
		    call tbxgpt (tp, cp, offset, Memc[buf], SZ_LINE, l_val)
		    call sscan (Memc[buf])
			call gargd (dbuf)
		    if (nscan() < 1 || dabs (dbuf) > MAX_SHORT)
			buffer[i] = INDEFS
		    else
			buffer[i] = sdnint (dbuf)
		}
		call sfree (sp)
	    }
	}

	return (nret)
end

long procedure tbxagb (tp, cp, row, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
long	row		# i: row number
bool	buffer[ARB]	# o: values read from table
long	first		# i: number of first array element to read
long	nelem		# i: maximum number of elements to read
#--
size_t	sz_val
long	l_val
pointer sp
pointer buf		# scratch for local string buffer
double	dbuf
real	rbuf
int	ibuf
short	sbuf
long	offset		# offset of first element in entry
int	dtype		# data type of column
long	ntotal		# total number of elements in array
long	nret		# actual number of elements to read
size_t	nchar		# number of char to read
long	i		# loop index
int	nscan()
long	tbeoff(), read()
string	CANNOTREAD	"tbagtb:  unexpected end of file"
errchk	seek, read, tbxgpt

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)
	offset = tbeoff (tp, cp, row, first)
	call seek (TB_FILE(tp), offset)

	if (dtype == TBL_TY_BOOL) {

	    nchar = nret * SZ_BOOL
	    if (read (TB_FILE(tp), buffer, nchar) < nchar)
		call error (1, CANNOTREAD)

	} else {

	    switch (dtype) {
	    case TBL_TY_DOUBLE:
		nchar = SZ_DOUBLE
		do i = 1, nret {
		    if (read (TB_FILE(tp), dbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (TBL_IS_INDEFD (dbuf))
			buffer[i] = false
		    else
			buffer[i] = (dbuf != double(NO))
		}
	    case TBL_TY_REAL:
		nchar = SZ_REAL
		do i = 1, nret {
		    if (read (TB_FILE(tp), rbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFR(rbuf))
			buffer[i] = false
		    else
			buffer[i] = (rbuf != real(NO))
		}
	    case TBL_TY_INT:
		nchar = SZ_INT
		do i = 1, nret {
		    if (read (TB_FILE(tp), ibuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFI(ibuf))
			buffer[i] = false
		    else
			buffer[i] = (ibuf != NO)
		}
	    case TBL_TY_SHORT:
		nchar = SZ_SHORT
		do i = 1, nret {
		    if (read (TB_FILE(tp), sbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (IS_INDEFS(sbuf))
			buffer[i] = false
		    else
			buffer[i] = (sbuf != NO)
		}
	    default:
		if (dtype > 0 && dtype != TBL_TY_CHAR)
		    call error (1, "tbagtb:  bad data type")
		call smark (sp)
		sz_val = SZ_LINE
		call salloc (buf, sz_val, TY_CHAR)
		do i = 1, nret {
		    offset = tbeoff (tp, cp, row, first+i-1)
		    l_val = 1
		    call tbxgpt (tp, cp, offset, Memc[buf], SZ_LINE, l_val)
		    call sscan (Memc[buf])
			call gargb (buffer[i])
		    if (nscan() < 1)
			buffer[i] = false
		}
		call sfree (sp)
	    }
	}

	return (nret)
end

long procedure tbxagt (tp, cp, row, cbuf, maxch, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
long	row		# i: row number
char	cbuf[maxch,ARB]	# o: values read from table
int	maxch		# i: size of first dimension of cbuf
long	first		# i: number of first array element to read
long	nelem		# i: maximum number of elements to read
#--
size_t	sz_val
pointer sp
pointer buf		# scratch for local string buffer
double	dbuf
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
char	pformat[SZ_COLFMT]	# print format for column
long	offset		# offset of first element in entry
int	dtype		# data type of column
long	ntotal		# total number of elements in array
long	nret		# actual number of elements to read
long	i		# loop index
size_t	nchar
long	tbeoff(), read()
string	CANNOTREAD	"tbagtt:  unexpected end of file"
errchk	seek, read, tbxgpt

begin
	dtype = COL_DTYPE(cp)
	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)
	offset = tbeoff (tp, cp, row, first)

	if (dtype < 0 || dtype == TBL_TY_CHAR) {

	    call tbxgpt (tp, cp, offset, cbuf[1,1], maxch, nret)

	} else {

	    call smark (sp)
	    sz_val = SZ_LINE+maxch
	    call salloc (buf, sz_val, TY_CHAR)
	    call tbcigt (cp, TBL_COL_FMT, pformat, SZ_COLFMT)

	    call seek (TB_FILE(tp), offset)

	    do i = 1, nret {

		switch (dtype) {
		case TBL_TY_REAL:
		    nchar = SZ_REAL
		    if (read (TB_FILE(tp), rbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    call sprintf (Memc[buf], SZ_LINE+maxch, pformat)
			call pargr (rbuf)
		case TBL_TY_DOUBLE:
		    nchar = SZ_DOUBLE
		    if (read (TB_FILE(tp), dbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    if (TBL_IS_INDEFD (dbuf)) {
			call strcpy ("INDEF", Memc[buf], SZ_LINE)
		    } else {
			call sprintf (Memc[buf], SZ_LINE+maxch, pformat)
			    call pargd (dbuf)
		    }
		case TBL_TY_INT:
		    nchar = SZ_INT
		    if (read (TB_FILE(tp), ibuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    call sprintf (Memc[buf], SZ_LINE+maxch, pformat)
			call pargi (ibuf)
		case TBL_TY_SHORT:
		    nchar = SZ_SHORT
		    if (read (TB_FILE(tp), sbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    call sprintf (Memc[buf], SZ_LINE+maxch, pformat)
			call pargs (sbuf)
		case TBL_TY_BOOL:
		    nchar = SZ_BOOL
		    if (read (TB_FILE(tp), bbuf, nchar) < nchar)
			call error (1, CANNOTREAD)
		    call sprintf (Memc[buf], SZ_LINE+maxch, pformat)
			call pargb (bbuf)
		default:
		    call error (1, "tbagtt:  bad data type")
		}

		call strcpy (Memc[buf], cbuf[1,i], maxch)
	    }

	    call sfree (sp)
	}

	return (nret)
end

# tbxgpt -- array get primitive text

procedure tbxgpt (tp, cp, offset, cbuf, maxch, nelem)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
long	offset			# i: offset in char to first location
char	cbuf[maxch,nelem]	# o: buffer to receive values
int	maxch			# i: size of each element of array
long	nelem			# i: number of elements to get
#--
char	buffer[SZ_LINE]		# buffer for reading from table
long	eoffset			# offset to location for reading
size_t	nchar			# size of each element in table
long	i
size_t	sz_val
long	read(), tbeszt()
errchk	seek, read

begin
	nchar = min (tbeszt (cp), SZ_LINE)	# size of each element
	eoffset = offset			# an initial value

	do i = 1, nelem {			# do for each element

	    call seek (TB_FILE(tp), eoffset)
	    if (read (TB_FILE(tp), buffer, nchar) < nchar)
		call error (1, "tbxgpt:  unexpected end of file")

	    # It may be that no EOS was read from the element in the table.
	    buffer[nchar+1] = EOS
	    sz_val = maxch
	    call strupk (buffer, cbuf[1,i], sz_val)

	    eoffset = eoffset + nchar
	}
end
