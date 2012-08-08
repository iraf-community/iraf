include <mach.h>		# for MAX_INT and MAX_SHORT
include "tbtables.h"
include "tblerr.h"

# tbyrpb -- Y putrow Boolean
# Write column values to a row.  This is for data type Boolean and
# column-ordered SDAS tables.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge,  7-Mar-1989  Eliminate TB_MODSIZE.
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  tbyrpt:  call sscan as a subroutine, not a function.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge,  5-Mar-1998  Remove calls to tbytsz, and don't update TB_NROWS,
#			as these are taken care of at a higher level.

procedure tbyrpb (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: Pointer to table descriptor
pointer colptr[numcols]		# i: Array of pointers to column descriptors
bool	buffer[numcols]		# i: Array of values to be put into table
int	numcols			# i: Number of columns
int	rownum			# i: Row number; may be beyond end of file
#--
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		if (buffer[k])
		    realbuf = real(YES)
		else
		    realbuf = real(NO)
		call write (TB_FILE(tp), realbuf, SZ_REAL)
	    case TY_DOUBLE:
		if (buffer[k])
		    dblbuf = double(YES)
		else
		    dblbuf = double(NO)
		call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
	    case TY_INT:
		if (buffer[k])
		    intbuf = YES
		else
		    intbuf = NO
                if (SZ_INT != SZ_INT32)
                    call ipak32 (intbuf, intbuf, 1)
		call write (TB_FILE(tp), intbuf, SZ_INT32)
	    case TY_SHORT:
		if (buffer[k])
		    shortbuf = YES
		else
		    shortbuf = NO
		call write (TB_FILE(tp), shortbuf, SZ_SHORT)
	    case TY_BOOL:
		call write (TB_FILE(tp), buffer[k], SZ_BOOL)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call sprintf (charbuf, SZ_LINE, "%-3b")
			call pargb (buffer[k])
		    call strpak (charbuf, charbuf, SZ_LINE)
		    call write (TB_FILE(tp), charbuf, COL_LEN(colptr[k]))
		} else {
		    call error (ER_TBCOLBADTYP, "tbrptb:  invalid data type")
		}
	    }
	}
end


# tbyrpd -- Y putrow double
# Write column values to a row.  This is for data type double and
# column-ordered SDAS tables.

procedure tbyrpd (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: Pointer to table descriptor
pointer colptr[numcols]		# i: Array of pointers to column descriptors
double	buffer[numcols]		# i: Array of values to be put into table
int	numcols			# i: Number of columns
int	rownum			# i: Row number; may be beyond end of file
#--
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		if (IS_INDEFD (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		call write (TB_FILE(tp), realbuf, SZ_REAL)
	    case TY_DOUBLE:
		if (IS_INDEFD (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
	    case TY_INT:
		if (IS_INDEFD (buffer[k]) || abs (buffer[k]) > MAX_INT)
		    intbuf = INDEFI
		else
		    intbuf = nint (buffer[k])
                if (SZ_INT != SZ_INT32)
                    call ipak32 (intbuf, intbuf, 1)
		call write (TB_FILE(tp), intbuf, SZ_INT32)
	    case TY_SHORT:
		if (IS_INDEFD (buffer[k]) || abs (buffer[k]) > MAX_SHORT)
		    shortbuf = INDEFS
		else
		    shortbuf = nint (buffer[k])
		call write (TB_FILE(tp), shortbuf, SZ_SHORT)
	    case TY_BOOL:
		if (IS_INDEFD (buffer[k]) || abs (buffer[k]) > MAX_INT)
		    boolbuf = false
		else
		    boolbuf = (nint(buffer[k]) != NO)
		call write (TB_FILE(tp), boolbuf, SZ_BOOL)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call sprintf (charbuf, SZ_LINE, "%-25.17g")
			call pargd (buffer[k])
		    call strpak (charbuf, charbuf, SZ_LINE)
		    call write (TB_FILE(tp), charbuf, COL_LEN(colptr[k]))
		} else {
		    call error (ER_TBCOLBADTYP, "tbrptd:  invalid data type")
		}
	    }
	}
end


# tbyrpr -- Y putrow real
# Write column values to a row.  This is for data type real and
# column-ordered SDAS tables.

procedure tbyrpr (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: Pointer to table descriptor
pointer colptr[numcols]		# i: Array of pointers to column descriptors
real	buffer[numcols]		# i: Array of values to be put into table
int	numcols			# i: Number of columns
int	rownum			# i: Row number; may be beyond end of file
#--
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
# buffers for copying elements of various types
double	dblbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		call write (TB_FILE(tp), buffer[k], SZ_REAL)
	    case TY_DOUBLE:
		if (IS_INDEFR(buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
	    case TY_INT:
		if (IS_INDEFR(buffer[k]) || abs (buffer[k]) > MAX_INT)
		    intbuf = INDEFI
		else
		    intbuf = nint (buffer[k])
                if (SZ_INT != SZ_INT32)
                    call ipak32 (intbuf, intbuf, 1)
		call write (TB_FILE(tp), intbuf, SZ_INT32)
	    case TY_SHORT:
		if (IS_INDEFR(buffer[k]) || abs (buffer[k]) > MAX_SHORT)
		    shortbuf = INDEFS
		else
		    shortbuf = nint (buffer[k])
		call write (TB_FILE(tp), shortbuf, SZ_SHORT)
	    case TY_BOOL:
		if (IS_INDEFR (buffer[k]) || abs (buffer[k]) > MAX_INT)
		    boolbuf = false
		else
		    boolbuf = (nint(buffer[k]) != NO)
		call write (TB_FILE(tp), boolbuf, SZ_BOOL)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call sprintf (charbuf, SZ_LINE, "%-15.7g")
			call pargr (buffer[k])
		    call strpak (charbuf, charbuf, SZ_LINE)
		    call write (TB_FILE(tp), charbuf, COL_LEN(colptr[k]))
		} else {
		    call error (ER_TBCOLBADTYP, "tbrptr:  invalid data type")
		}
	    }
	}
end


# tbyrpi -- Y putrow integer
# Write column values to a row.  This is for data type integer and
# column-ordered SDAS tables.

procedure tbyrpi (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: Pointer to table descriptor
pointer colptr[numcols]		# i: Array of pointers to column descriptors
int	buffer[numcols]		# i: Array of values to be put into table
int	numcols			# i: Number of columns
int	rownum			# i: Row number; may be beyond end of file
#--
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		if (IS_INDEFI (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		call write (TB_FILE(tp), realbuf, SZ_REAL)
	    case TY_DOUBLE:
		if (IS_INDEFI (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
	    case TY_INT:
                if (SZ_INT != SZ_INT32)
                    call ipak32 (buffer[k], buffer[k], 1)
		call write (TB_FILE(tp), buffer[k], SZ_INT32)
	    case TY_SHORT:
		if (IS_INDEFI (buffer[k]) || abs (buffer[k]) > MAX_SHORT)
		    shortbuf = INDEFS
		else
		    shortbuf = buffer[k]
		call write (TB_FILE(tp), shortbuf, SZ_SHORT)
	    case TY_BOOL:
		if (IS_INDEFI (buffer[k]) || (buffer[k] == NO))
		    boolbuf = false
		else
		    boolbuf = true
		call write (TB_FILE(tp), boolbuf, SZ_BOOL)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call sprintf (charbuf, SZ_LINE, "%-11d")
			call pargi (buffer[k])
		    call strpak (charbuf, charbuf, SZ_LINE)
		    call write (TB_FILE(tp), charbuf, COL_LEN(colptr[k]))
		} else {
		    call error (ER_TBCOLBADTYP, "tbrpti:  invalid data type")
		}
	    }
	}
end


# tbyrps -- Y putrow short
# Write column values to a row.  This is for data type short integer and
# column-ordered SDAS tables.

procedure tbyrps (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: Pointer to table descriptor
pointer colptr[numcols]		# i: Array of pointers to column descriptors
short	buffer[numcols]		# i: Array of values to be put into table
int	numcols			# i: Number of columns
int	rownum			# i: Row number; may be beyond end of file
#--
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
# buffers for copying elements of various types
double	dblbuf
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]
long	tbyoff()
errchk	seek, write

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		if (IS_INDEFS (buffer[k]))
		    realbuf = INDEFR
		else
		    realbuf = buffer[k]
		call write (TB_FILE(tp), realbuf, SZ_REAL)
	    case TY_DOUBLE:
		if (IS_INDEFS (buffer[k]))
		    dblbuf = TBL_INDEFD
		else
		    dblbuf = buffer[k]
		call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
	    case TY_INT:
		if (IS_INDEFS (buffer[k]))
		    intbuf = INDEFI
		else
		    intbuf = buffer[k]
                if (SZ_INT != SZ_INT32)
                    call ipak32 (intbuf, intbuf, 1)
		call write (TB_FILE(tp), intbuf, SZ_INT32)
	    case TY_SHORT:
		call write (TB_FILE(tp), buffer[k], SZ_SHORT)
	    case TY_BOOL:
		if (IS_INDEFS (buffer[k]) || (buffer[k] == NO))
		    boolbuf = false
		else
		    boolbuf = true
		call write (TB_FILE(tp), boolbuf, SZ_BOOL)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call sprintf (charbuf, SZ_LINE, "%-11d")
			call pargs (buffer[k])
		    call strpak (charbuf, charbuf, SZ_LINE)
		    call write (TB_FILE(tp), charbuf, COL_LEN(colptr[k]))
		} else {
		    call error (ER_TBCOLBADTYP, "tbrpts:  invalid data type")
		}
	    }
	}
end


# tbyrpt -- Y putrow text
# Write column values to a row.  This is for character strings and
# column-ordered SDAS tables.

procedure tbyrpt (tp, colptr, buffer, lenstring, numcols, rownum)

pointer tp			# i: Pointer to table descriptor
pointer colptr[numcols]		# i: Array of pointers to column descriptors
char	buffer[lenstring, numcols] # i: Array of values to be put into table
int	lenstring		# i: Length of each string in array buffer
int	numcols			# i: Number of columns
int	rownum			# i: Row number; may be beyond end of file
#--
long	offset			# Offset of column entry from BOF
int	k			# Loop index
int	datatype		# Data type of element in table
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
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    call seek (TB_FILE(tp), offset)
	    switch (datatype) {
	    case TY_REAL:
		call sscan (buffer[1,k])
		    call gargr (realbuf)
		    if (nscan() < 1)
			realbuf = INDEFR
		call write (TB_FILE(tp), realbuf, SZ_REAL)
	    case TY_DOUBLE:
		call sscan (buffer[1,k])
		    call gargd (dblbuf)
		    if (nscan() < 1)
			dblbuf = TBL_INDEFD
		    else if (IS_INDEFD (dblbuf))
			dblbuf = TBL_INDEFD
		call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
	    case TY_INT:
		call sscan (buffer[1,k])
		    call gargi (intbuf)
		    if (nscan() < 1)
			intbuf = INDEFI
                if (SZ_INT != SZ_INT32)
                    call ipak32 (intbuf, intbuf, 1)
		call write (TB_FILE(tp), intbuf, SZ_INT32)
	    case TY_SHORT:
		call sscan (buffer[1,k])
		    call gargs (shortbuf)
		    if (nscan() < 1)
			shortbuf = INDEFS
		call write (TB_FILE(tp), shortbuf, SZ_SHORT)
	    case TY_BOOL:
		call sscan (buffer[1,k])
		    call gargb (boolbuf)
		    if (nscan() < 1)
			boolbuf = false
		call write (TB_FILE(tp), boolbuf, SZ_BOOL)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call strpak (buffer[1,k], charbuf, lenstring)
		    call write (TB_FILE(tp), charbuf, COL_LEN(colptr[k]))
		} else {
		    call error (ER_TBCOLBADTYP, "tbrptt:  invalid data type")
		}
	    }
	}
end
