include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Write column values to a row.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  5-Feb-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Include check on row number > 0.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 23-Jun-1995  Modify for FITS tables.
# Phil Hodge,  3-Mar-1998  Call tbswer1, to allow for row selector.

# tbrptd -- putrow double
# Write column values to a row.  This is for data type double.

procedure tbrptd (tp, cp, buffer, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
double	buffer[ARB]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# loop index for column number
errchk	tbswer1, tbxrpd, tbyrpd, tbzptd, tbfapd

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# (But note use of tbswer1, especially for row-ordered table.)
	call tbswer1 (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxrpd (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyrpd (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    do k = 1, numcols
		call tbzptd (tp, cp[k], rownum, buffer[k])
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    do k = 1, numcols
		call tbfapd (tp, cp[k], rownum, buffer[k], 1, 1)
	else
	    call error (ER_TBCORRUPTED, "tbrptd:  table type is messed up")

	TB_MODIFIED(tp) = true
end


# tbrptr -- putrow real
# Write column values to a row.  This is for data type real.

procedure tbrptr (tp, cp, buffer, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
real	buffer[ARB]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# loop index for column number
errchk	tbswer1, tbxrpr, tbyrpr, tbzptr, tbfapr

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer1 (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxrpr (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyrpr (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    do k = 1, numcols
		call tbzptr (tp, cp[k], rownum, buffer[k])
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    do k = 1, numcols
		call tbfapr (tp, cp[k], rownum, buffer[k], 1, 1)
	else
	    call error (ER_TBCORRUPTED, "tbrptr:  table type is messed up")

	TB_MODIFIED(tp) = true
end


# tbrpti -- putrow integer
# Write column values to a row.  This is for data type integer.

procedure tbrpti (tp, cp, buffer, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
int	buffer[ARB]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# loop index for column number
errchk	tbswer1, tbxrpi, tbyrpi, tbzpti, tbfapi

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer1 (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxrpi (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyrpi (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    do k = 1, numcols
		call tbzpti (tp, cp[k], rownum, buffer[k])
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    do k = 1, numcols
		call tbfapi (tp, cp[k], rownum, buffer[k], 1, 1)
	else
	    call error (ER_TBCORRUPTED, "tbrpti:  table type is messed up")

	TB_MODIFIED(tp) = true
end


# tbrpts -- putrow short
# Write column values to a row.  This is for data type short integer.

procedure tbrpts (tp, cp, buffer, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
short	buffer[ARB]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# loop index for column number
errchk	tbswer1, tbxrps, tbyrps, tbzpts, tbfaps

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer1 (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxrps (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyrps (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    do k = 1, numcols
		call tbzpts (tp, cp[k], rownum, buffer[k])
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    do k = 1, numcols
		call tbfaps (tp, cp[k], rownum, buffer[k], 1, 1)
	else
	    call error (ER_TBCORRUPTED, "tbrpts:  table type is messed up")

	TB_MODIFIED(tp) = true
end


# tbrptb -- putrow Boolean
# This is for data type Boolean.

procedure tbrptb (tp, cp, buffer, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
bool	buffer[ARB]		# i: array of values to be put into table
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# loop index for column number
errchk	tbswer1, tbxrpb, tbyrpb, tbzptb, tbfapb

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer1 (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxrpb (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyrpb (tp, cp, buffer, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    do k = 1, numcols
		call tbzptb (tp, cp[k], rownum, buffer[k])
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    do k = 1, numcols
		call tbfapb (tp, cp[k], rownum, buffer[k], 1, 1)
	else
	    call error (ER_TBCORRUPTED, "tbrptb:  table type is messed up")

	TB_MODIFIED(tp) = true
end


# tbrptt -- putrow text
# Write column values to a row.  This is for character strings.

procedure tbrptt (tp, cp, buffer, lenstr, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
char	buffer[lenstr,ARB]	# i: array of values to be put into table
int	lenstr			# i: length of each string in array buffer
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# loop index for column number
errchk	tbswer1, tbxrpt, tbyrpt, tbzptt, tbfapt

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer1 (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxrpt (tp, cp, buffer, lenstr, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyrpt (tp, cp, buffer, lenstr, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    do k = 1, numcols
		call tbzptt (tp, cp[k], rownum, buffer[1,k])
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    do k = 1, numcols
		call tbfapt (tp, cp[k], rownum, buffer[1,k], lenstr, 1, 1)
	else
	    call error (ER_TBCORRUPTED, "tbrptt:  table type is messed up")

	TB_MODIFIED(tp) = true
end
