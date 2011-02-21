include <mach.h>		# for MAX_INT and MAX_SHORT
include "tbtables.h"
include "tblerr.h"

# Write column values into a row.  Values from more than one column may be
# written in one call.  These routines are for row-ordered tables.
# This file contains the tbxrp[tbirds] routines plus tbbcpy.
#
# Phil Hodge, 10-Nov-1987  Pass Memi instead of Memr to tbbeqd.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  6-Mar-1989  Allow COL_DTYPE < 0 for character columns.
# Phil Hodge,  7-Mar-1989  Eliminate TB_OFFSET, TB_CURROW, TB_MODSIZE.
# Phil Hodge, 26-Jun-1989  Use tbbcpy to copy to indef record buffer, which
#				was too large by the factor SZ_REAL.
# Phil Hodge, 30-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  tbxrpt:  call sscan as a subroutine, not a function.
# Phil Hodge, 14-Sep-1994  Use tbeszt for length of string; in tbxrpt, use
#				gargd (dblbuf) and then nint for int & short.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge,  4-Mar-1998  Remove call to tbxwsk, use tbxwer & tbxoff instead.
# Phil Hodge, 27-Aug-2002  In tbxrpi and tbxrps, include an explicit test
#	for INDEF, rather than relying on a test on abs (dblbuf).

# tbxrpb -- X putrow Boolean

procedure tbxrpb (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
bool	buffer[numcols]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	rownum			# i: row number; may be beyond end of file
#--
int	k			# Loop index
int	datatype		# Data type of element in table
long	roffset			# Offset of beginning of row from BOF
long	offset			# Offset of column entry from BOF
pointer sp, eofbuf
int	nrows, rowlen
double	dblbuf			# Buffer used when type conversion is needed
real	realbuf
int	intbuf
short	shortbuf
char	charbuf[SZ_LINE]	# Buffer for character columns
long	tbxoff()
int	tbeszt()
errchk	tbxwer, seek, write

begin
	nrows = TB_NROWS(tp)

	if (rownum == nrows+1) {

	    # Write at EOF.
	    rowlen = TB_ROWLEN(tp)	# unit = SZ_CHAR
	    call smark (sp)
	    call salloc (eofbuf, rowlen, TY_CHAR)
	    call amovc (Memc[TB_INDEF(tp)], Memc[eofbuf], rowlen)
	    do k = 1, numcols {

		datatype = COL_DTYPE(colptr[k])
		# This offset is from beginning of record in units of SZ_CHAR.
		offset = COL_OFFSET(colptr[k])

		switch (datatype) {
		case TY_REAL:
		    if (buffer[k])
			call tbbeqr (real(YES), Memc[eofbuf+offset])
		    else
			call tbbeqr (real(NO), Memc[eofbuf+offset])
		case TY_DOUBLE:
		    if (buffer[k])
			call tbbeqd (double(YES), Memc[eofbuf+offset])
		    else
			call tbbeqd (double(NO), Memc[eofbuf+offset])
		case TY_INT:
		    if (buffer[k])
			call tbbeqi (YES, Memc[eofbuf+offset])
		    else
			call tbbeqi (NO, Memc[eofbuf+offset])
		case TY_SHORT:
		    if (buffer[k])
			shortbuf = YES
		    else
			shortbuf = NO
		    call tbbeqs (shortbuf, Memc[eofbuf+offset])
		case TY_BOOL:
		    call tbbeqb (buffer[k], Memc[eofbuf+offset])
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-3b")
			    call pargb (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call tbbcpy (charbuf, Memc[eofbuf+offset],
				    tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptb:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	    # This is the offset (unit=SZ_CHAR) to the beginning of the row.
	    roffset = tbxoff (tp, rownum)
	    call seek (TB_FILE(tp), roffset)
	    call write (TB_FILE(tp), Memc[eofbuf], rowlen)
	    TB_NROWS(tp) = rownum
	    call sfree (sp)

	} else {

	    # If we are seeking beyond EOF, write fill records.
	    if (rownum > TB_NROWS(tp)) {
		call tbxwer (tp, rownum)
		TB_NROWS(tp) = rownum
	    }

	    # Get the offset to the row to which we will write.
	    roffset = tbxoff (tp, rownum)

	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		offset = roffset + COL_OFFSET(colptr[k])	# unit = SZ_CHAR
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
			call write (TB_FILE(tp), charbuf, tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptb:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
end


# tbxrpd -- X putrow double
# Write column values to a row.  This is for data type double and
# row-ordered SDAS tables.

procedure tbxrpd (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
double	buffer[numcols]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	rownum			# i: row number; may be beyond end of file
#--
int	k			# Loop index
int	datatype		# Data type of element in table
long	roffset			# Offset of beginning of row from BOF
long	offset			# Offset of column entry from BOF
pointer sp, eofbuf
int	nrows, rowlen
double	dblbuf			# Buffer used when type conversion is needed
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]	# Buffer for character columns
long	tbxoff()
int	tbeszt()
errchk	tbxwer, seek, write

begin
	nrows = TB_NROWS(tp)

	if (rownum == nrows+1) {

	    # Write at EOF.
	    rowlen = TB_ROWLEN(tp)
	    call smark (sp)
	    call salloc (eofbuf, rowlen, TY_CHAR)
	    call amovc (Memc[TB_INDEF(tp)], Memc[eofbuf], rowlen)
	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		# This offset is from beginning of record in units of SZ_CHAR.
		offset = COL_OFFSET(colptr[k])

		switch (datatype) {
		case TY_REAL:
		    if (IS_INDEFD (buffer[k]))
			realbuf = INDEFR
		    else
			realbuf = buffer[k]
		    call tbbeqr (realbuf, Memc[eofbuf+offset])
		case TY_DOUBLE:
		    if (IS_INDEFD (buffer[k]))
			dblbuf = TBL_INDEFD
		    else
			dblbuf = buffer[k]
		    call tbbeqd (dblbuf, Memc[eofbuf+offset])
		case TY_INT:
		    if (IS_INDEFD (buffer[k]) || (abs (buffer[k]) > MAX_INT))
			intbuf = INDEFI
		    else
			intbuf = nint (buffer[k])
		    call tbbeqi (intbuf, Memc[eofbuf+offset])
		case TY_SHORT:
		    if (IS_INDEFD (buffer[k]) || (abs (buffer[k]) > MAX_SHORT))
			shortbuf = INDEFS
		    else
			shortbuf = nint (buffer[k])
		    call tbbeqs (shortbuf, Memc[eofbuf+offset])
		case TY_BOOL:
		    if (IS_INDEFD (buffer[k]) || (nint(buffer[k]) == NO))
			call tbbeqb (false, Memc[eofbuf+offset])
		    else
			call tbbeqb (true, Memc[eofbuf+offset])
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-25.16g")
			    call pargd (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call tbbcpy (charbuf, Memc[eofbuf+offset],
				    tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptd:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	    # This is the offset (unit=SZ_CHAR) to the beginning of the row.
	    roffset = tbxoff (tp, rownum)
	    call seek (TB_FILE(tp), roffset)
	    call write (TB_FILE(tp), Memc[eofbuf], rowlen)
	    TB_NROWS(tp) = rownum
	    call sfree (sp)

	} else {

	    # If we are seeking beyond EOF, write fill records.
	    if (rownum > TB_NROWS(tp)) {
		call tbxwer (tp, rownum)
		TB_NROWS(tp) = rownum
	    }

	    # Get the offset to the row to which we will write.
	    roffset = tbxoff (tp, rownum)

	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		offset = roffset + COL_OFFSET(colptr[k])	# unit = SZ_CHAR
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
		    if (IS_INDEFD (buffer[k]) || (abs (buffer[k]) > MAX_INT))
			intbuf = INDEFI
		    else
			intbuf = nint (buffer[k])
                    if (SZ_INT != SZ_INT32)
                        call ipak32 (intbuf, intbuf, 1)
		    call write (TB_FILE(tp), intbuf, SZ_INT32)
		case TY_SHORT:
		    if (IS_INDEFD (buffer[k]) || (abs (buffer[k]) > MAX_SHORT))
			shortbuf = INDEFS
		    else
			shortbuf = nint (buffer[k])
		    call write (TB_FILE(tp), shortbuf, SZ_SHORT)
		case TY_BOOL:
		    if (IS_INDEFD (buffer[k]) || (nint(buffer[k]) == NO))
			boolbuf = false
		    else
			boolbuf = true
		    call write (TB_FILE(tp), boolbuf, SZ_BOOL)
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-25.17g")
			    call pargd (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call write (TB_FILE(tp), charbuf, tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptd:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
end


# tbxrpr -- X putrow real
# Write column values to a row.  This is for data type real and
# row-ordered SDAS tables.

procedure tbxrpr (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
real	buffer[numcols]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	rownum			# i: row number; may be beyond end of file
#--
int	k			# Loop index
int	datatype		# Data type of element in table
long	roffset			# Offset of beginning of row from BOF
long	offset			# Offset of column entry from BOF
pointer sp, eofbuf
int	nrows, rowlen
double	dblbuf			# Buffer used when type conversion is needed
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]	# Buffer for character columns
long	tbxoff()
int	tbeszt()
errchk	tbxwer, seek, write

begin
	nrows = TB_NROWS(tp)

	if (rownum == nrows+1) {

	    # Write at EOF.
	    rowlen = TB_ROWLEN(tp)
	    call smark (sp)
	    call salloc (eofbuf, rowlen, TY_CHAR)
	    call amovc (Memc[TB_INDEF(tp)], Memc[eofbuf], rowlen)
	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		# This offset is from beginning of record in units of SZ_CHAR.
		offset = COL_OFFSET(colptr[k])

		switch (datatype) {
		case TY_REAL:
		    call tbbeqr (buffer[k], Memc[eofbuf+offset])
		case TY_DOUBLE:
		    if (IS_INDEFR (buffer[k]))
			dblbuf = TBL_INDEFD
		    else
			dblbuf = buffer[k]
		    call tbbeqd (dblbuf, Memc[eofbuf+offset])
		case TY_INT:
		    if (IS_INDEFR (buffer[k]) || (abs (buffer[k]) > MAX_INT))
			intbuf = INDEFI
		    else
			intbuf = nint (buffer[k])
		    call tbbeqi (intbuf, Memc[eofbuf+offset])
		case TY_SHORT:
		    if (IS_INDEFR (buffer[k]) || (abs (buffer[k]) > MAX_SHORT))
			shortbuf = INDEFS
		    else
			shortbuf = nint (buffer[k])
		    call tbbeqs (shortbuf, Memc[eofbuf+offset])
		case TY_BOOL:
		    if (IS_INDEFR (buffer[k]) || (nint(buffer[k]) == NO))
			call tbbeqb (false, Memc[eofbuf+offset])
		    else
			call tbbeqb (true, Memc[eofbuf+offset])
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-15.7g")
			    call pargr (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call tbbcpy (charbuf, Memc[eofbuf+offset],
				    tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptr:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	    # This is the offset (unit=SZ_CHAR) to the beginning of the row.
	    roffset = tbxoff (tp, rownum)
	    call seek (TB_FILE(tp), roffset)
	    call write (TB_FILE(tp), Memc[eofbuf], rowlen)
	    TB_NROWS(tp) = rownum
	    call sfree (sp)

	} else {

	    # If we are seeking beyond EOF, write fill records.
	    if (rownum > TB_NROWS(tp)) {
		call tbxwer (tp, rownum)
		TB_NROWS(tp) = rownum
	    }

	    # Get the offset to the row to which we will write.
	    roffset = tbxoff (tp, rownum)

	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		offset = roffset + COL_OFFSET(colptr[k])	# unit = SZ_CHAR
		call seek (TB_FILE(tp), offset)
		switch (datatype) {
		case TY_REAL:
		    call write (TB_FILE(tp), buffer[k], SZ_REAL)
		case TY_DOUBLE:
		    if (IS_INDEFR (buffer[k]))
			dblbuf = TBL_INDEFD
		    else
			dblbuf = buffer[k]
		    call write (TB_FILE(tp), dblbuf, SZ_DOUBLE)
		case TY_INT:
		    if (IS_INDEFR (buffer[k]) || (abs (buffer[k]) > MAX_INT))
			intbuf = INDEFI
		    else
			intbuf = nint (buffer[k])
                    if (SZ_INT != SZ_INT32)
                        call ipak32 (intbuf, intbuf, 1)
		    call write (TB_FILE(tp), intbuf, SZ_INT32)
		case TY_SHORT:
		    if (IS_INDEFR (buffer[k]) || (abs (buffer[k]) > MAX_SHORT))
			shortbuf = INDEFS
		    else
			shortbuf = nint (buffer[k])
		    call write (TB_FILE(tp), shortbuf, SZ_SHORT)
		case TY_BOOL:
		    if (IS_INDEFR (buffer[k]) || (nint(buffer[k]) == NO))
			boolbuf = false
		    else
			boolbuf = true
		    call write (TB_FILE(tp), boolbuf, SZ_BOOL)
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-15.7g")
			    call pargr (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call write (TB_FILE(tp), charbuf, tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptr:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
end


# tbxrpi -- X putrow integer
# Write column values to a row.  This is for data type integer and
# row-ordered SDAS tables.

procedure tbxrpi (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
int	buffer[numcols]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	rownum			# i: row number; may be beyond end of file
#--
int	k			# Loop index
int	datatype		# Data type of element in table
long	roffset			# Offset of beginning of row from BOF
long	offset			# Offset of column entry from BOF
pointer sp, eofbuf
int	nrows, rowlen
double	dblbuf			# Buffer used when type conversion is needed
real	realbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]	# Buffer for character columns
long	tbxoff()
int	tbeszt()
errchk	tbxwer, seek, write

begin
	nrows = TB_NROWS(tp)

	if (rownum == nrows+1) {

	    # Write at EOF.
	    rowlen = TB_ROWLEN(tp)
	    call smark (sp)
	    call salloc (eofbuf, rowlen, TY_CHAR)
	    call amovc (Memc[TB_INDEF(tp)], Memc[eofbuf], rowlen)
	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		# This offset is from beginning of record in units of SZ_CHAR.
		offset = COL_OFFSET(colptr[k])

		switch (datatype) {
		case TY_REAL:
		    if (IS_INDEFI (buffer[k]))
			realbuf = INDEFR
		    else
			realbuf = buffer[k]
		    call tbbeqr (realbuf, Memc[eofbuf+offset])
		case TY_DOUBLE:
		    if (IS_INDEFI (buffer[k]))
			dblbuf = TBL_INDEFD
		    else
			dblbuf = buffer[k]
		    call tbbeqd (dblbuf, Memc[eofbuf+offset])
		case TY_INT:
		    call tbbeqi (buffer[k], Memc[eofbuf+offset])
		case TY_SHORT:
		    if (IS_INDEFI (buffer[k]) || (abs (buffer[k]) > MAX_SHORT))
			shortbuf = INDEFS
		    else
			shortbuf = buffer[k]
		    call tbbeqs (shortbuf, Memc[eofbuf+offset])
		case TY_BOOL:
		    if (IS_INDEFI (buffer[k]) || (buffer[k] == NO))
			call tbbeqb (false, Memc[eofbuf+offset])
		    else
			call tbbeqb (true, Memc[eofbuf+offset])
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-11d")
			    call pargi (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call tbbcpy (charbuf, Memc[eofbuf+offset],
				    tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrpti:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	    # This is the offset (unit=SZ_CHAR) to the beginning of the row.
	    roffset = tbxoff (tp, rownum)
	    call seek (TB_FILE(tp), roffset)
	    call write (TB_FILE(tp), Memc[eofbuf], rowlen)
	    TB_NROWS(tp) = rownum
	    call sfree (sp)

	} else {

	    # If we are seeking beyond EOF, write fill records.
	    if (rownum > TB_NROWS(tp)) {
		call tbxwer (tp, rownum)
		TB_NROWS(tp) = rownum
	    }

	    # Get the offset to the row to which we will write.
	    roffset = tbxoff (tp, rownum)

	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		offset = roffset + COL_OFFSET(colptr[k])	# unit = SZ_CHAR
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
		    if (IS_INDEFI (buffer[k]) || (abs (buffer[k]) > MAX_SHORT))
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
			call write (TB_FILE(tp), charbuf, tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrpti:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
end


# tbxrps -- X putrow short integer
# Write column values to a row.  This is for data type short integer and
# row-ordered SDAS tables.

procedure tbxrps (tp, colptr, buffer, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
short	buffer[numcols]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	rownum			# i: row number; may be beyond end of file
#--
int	k			# Loop index
int	datatype		# Data type of element in table
long	roffset			# Offset of beginning of row from BOF
long	offset			# Offset of column entry from BOF
pointer sp, eofbuf
int	nrows, rowlen
double	dblbuf			# Buffer used when type conversion is needed
real	realbuf
int	intbuf
bool	boolbuf
char	charbuf[SZ_LINE]	# Buffer for character columns
long	tbxoff()
int	tbeszt()
errchk	tbxwer, seek, write

begin
	nrows = TB_NROWS(tp)

	if (rownum == nrows+1) {

	    # Write at EOF.
	    rowlen = TB_ROWLEN(tp)
	    call smark (sp)
	    call salloc (eofbuf, rowlen, TY_CHAR)
	    call amovc (Memc[TB_INDEF(tp)], Memc[eofbuf], rowlen)
	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		# This offset is from beginning of record in units of SZ_CHAR.
		offset = COL_OFFSET(colptr[k])

		switch (datatype) {
		case TY_REAL:
		    if (IS_INDEFS (buffer[k]))
			realbuf = INDEFR
		    else
			realbuf = buffer[k]
		    call tbbeqr (realbuf, Memc[eofbuf+offset])
		case TY_DOUBLE:
		    if (IS_INDEFS (buffer[k]))
			dblbuf = TBL_INDEFD
		    else
			dblbuf = buffer[k]
		    call tbbeqd (dblbuf, Memc[eofbuf+offset])
		case TY_INT:
		    if (IS_INDEFS (buffer[k]))
			intbuf = INDEFI
		    else
			intbuf = buffer[k]
		    call tbbeqi (intbuf, Memc[eofbuf+offset])
		case TY_SHORT:
		    call tbbeqs (buffer[k], Memc[eofbuf+offset])
		case TY_BOOL:
		    if (IS_INDEFS (buffer[k]) || (buffer[k] == NO))
			call tbbeqb (false, Memc[eofbuf+offset])
		    else
			call tbbeqb (true, Memc[eofbuf+offset])
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call sprintf (charbuf, SZ_LINE, "%-11d")
			    call pargs (buffer[k])
			call strpak (charbuf, charbuf, SZ_LINE)
			call tbbcpy (charbuf, Memc[eofbuf+offset],
				    tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrpts:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	    # This is the offset (unit=SZ_CHAR) to the beginning of the row.
	    roffset = tbxoff (tp, rownum)
	    call seek (TB_FILE(tp), roffset)
	    call write (TB_FILE(tp), Memc[eofbuf], rowlen)
	    TB_NROWS(tp) = rownum
	    call sfree (sp)

	} else {

	    # If we are seeking beyond EOF, write fill records.
	    if (rownum > TB_NROWS(tp)) {
		call tbxwer (tp, rownum)
		TB_NROWS(tp) = rownum
	    }

	    # Get the offset to the row to which we will write.
	    roffset = tbxoff (tp, rownum)

	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		offset = roffset + COL_OFFSET(colptr[k])	# unit = SZ_CHAR
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
			call write (TB_FILE(tp), charbuf, tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrpts:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
end


# tbxrpt -- X putrow text
# Write column values to a row.  This is for character strings and
# row-ordered SDAS tables.

procedure tbxrpt (tp, colptr, buffer, lenstring, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
char	buffer[lenstring, numcols]	# i: array of values to be put
int	lenstring		# i: length of each string in array buffer
int	numcols			# i: number of columns
int	rownum			# i: row number; may be beyond end of file
#--
int	k			# Loop index
int	datatype		# Data type of element in table
long	roffset			# Offset of beginning of row from BOF
long	offset			# Offset of column entry from BOF
pointer sp, eofbuf
int	nrows, rowlen
double	dblbuf			# Buffer used when type conversion is needed
real	realbuf
int	intbuf
short	shortbuf
bool	boolbuf
char	charbuf[SZ_LINE]	# Buffer for character columns
long	tbxoff()
int	tbeszt()
int	nscan()
errchk	tbxwer, seek, write

begin
	nrows = TB_NROWS(tp)

	if (rownum == nrows+1) {

	    # Write at EOF.
	    rowlen = TB_ROWLEN(tp)
	    call smark (sp)
	    call salloc (eofbuf, rowlen, TY_CHAR)
	    call amovc (Memc[TB_INDEF(tp)], Memc[eofbuf], rowlen)
	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		# This offset is from beginning of record in units of SZ_CHAR.
		offset = COL_OFFSET(colptr[k])

		switch (datatype) {
		case TY_REAL:
		    call sscan (buffer[1,k])
			call gargr (realbuf)
		    if (nscan() < 1)
			realbuf = INDEFR
		    call tbbeqr (realbuf, Memc[eofbuf+offset])
		case TY_DOUBLE:
		    call sscan (buffer[1,k])
			call gargd (dblbuf)
		    if (nscan() < 1)
			dblbuf = TBL_INDEFD
		    else if (IS_INDEFD (dblbuf))
			dblbuf = TBL_INDEFD
		    call tbbeqd (dblbuf, Memc[eofbuf+offset])
		case TY_INT:
		    call sscan (buffer[1,k])
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				abs (dblbuf) > MAX_INT) {
			intbuf = INDEFI
		    } else {
			intbuf = nint (dblbuf)
		    }
		    call tbbeqi (intbuf, Memc[eofbuf+offset])
		case TY_SHORT:
		    call sscan (buffer[1,k])
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				abs (dblbuf) > MAX_SHORT) {
			shortbuf = INDEFS
		    } else {
			shortbuf = nint (dblbuf)
		    }
		    call tbbeqs (shortbuf, Memc[eofbuf+offset])
		case TY_BOOL:
		    call sscan (buffer[1,k])
			call gargb (boolbuf)
		    if (nscan() < 1)
			boolbuf = false
		    call tbbeqb (boolbuf, Memc[eofbuf+offset])
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call strpak (buffer[1,k], charbuf, SZ_LINE)
			call tbbcpy (charbuf, Memc[eofbuf+offset],
				    tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptt:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	    # This is the offset (unit=SZ_CHAR) to the beginning of the row.
	    roffset = tbxoff (tp, rownum)
	    call seek (TB_FILE(tp), roffset)
	    call write (TB_FILE(tp), Memc[eofbuf], rowlen)
	    TB_NROWS(tp) = rownum
	    call sfree (sp)

	} else {

	    # If we are seeking beyond EOF, write fill records.
	    if (rownum > TB_NROWS(tp)) {
		call tbxwer (tp, rownum)
		TB_NROWS(tp) = rownum
	    }

	    # Get the offset to the row to which we will write.
	    roffset = tbxoff (tp, rownum)

	    do k = 1, numcols {
		datatype = COL_DTYPE(colptr[k])
		offset = roffset + COL_OFFSET(colptr[k])	# unit = SZ_CHAR
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
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				abs (dblbuf) > MAX_INT) {
			intbuf = INDEFI
		    } else {
			intbuf = nint (dblbuf)
		    }
                    if (SZ_INT != SZ_INT32)
                        call ipak32 (intbuf, intbuf, 1)
		    call write (TB_FILE(tp), intbuf, SZ_INT32)
		case TY_SHORT:
		    call sscan (buffer[1,k])
			call gargd (dblbuf)
		    if (nscan() < 1 || IS_INDEFD(dblbuf) ||
				abs (dblbuf) > MAX_SHORT) {
			shortbuf = INDEFS
		    } else {
			shortbuf = nint (dblbuf)
		    }
		    call write (TB_FILE(tp), shortbuf, SZ_SHORT)
		case TY_BOOL:
		    call sscan (buffer[1,k])
			call gargb (boolbuf)
		    if (nscan() < 1)
			boolbuf = false
		    call write (TB_FILE(tp), boolbuf, SZ_BOOL)
		default:
		    if (datatype < 0 || datatype == TY_CHAR) {
			call strpak (buffer[1,k], charbuf, SZ_LINE)
			call write (TB_FILE(tp), charbuf, tbeszt (colptr[k]))
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrptt:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
end

# tbbcpy -- string copy
# This routine differs from strcpy in that nothing will be written beyond
# maxch in the output string.  In particular, if the input string has maxch
# characters before the EOS, the output string will NOT have an EOS.

procedure tbbcpy (in, out, maxch)

char	in[ARB]		# i: input string
char	out[ARB]	# o: output string
int	maxch		# i: maximum number of char to assign in output
#--
int	k

begin
	do k = 1, maxch {
	    out[k] = in[k]
	    if (in[k] == EOS)
		break
	}
end
