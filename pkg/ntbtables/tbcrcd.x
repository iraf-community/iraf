include <mach.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbcrcd -- read column descriptor
# This procedure reads a column descriptor from the table file.
# The same routine is used for both row-ordered and column-ordered tables.
#
# Note that it is assumed that SZ_COLNAME is larger than SZ_CD_COLNAME, etc.
#
# Phil Hodge, 21-Jun-1995  Check for text or FITS tables; check for TY_CHAR.
# Phil Hodge, 14-Apr-1998  Change calling sequence;
#		change SZ_COLSTRUCT to SZ_COLDEF;
#		EOS may be absent in table, to allow one more char.
# Phil Hodge,  5-Aug-1999  Assign a value to COL_NELEM;
#		include tbalen in this file, since nothing else calls it.
# Phil Hodge, 23-Jun-2000  Assign values to COL_TDTYPE, COL_TSCAL, COL_TZERO.

procedure tbcrcd (tp, cp, colnum)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	colnum			# i: column number
#--
pointer sp
pointer coldef			# column descriptor read from table
pointer pformat			# scratch for print format
pointer temp			# scratch 
long	offset			# location of column descriptor in table file
int	stat			# status from read operation
int	read()
int	tbalen()

errchk	seek, read

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT || TB_TYPE(tp) == TBL_TYPE_FITS)
	    call error (1, "tbcrcd:  internal error")

	call smark (sp)
	call salloc (coldef, LEN_COLDEF, TY_STRUCT)
	call salloc (pformat, SZ_COLFMT, TY_CHAR)
	call salloc (temp, SZ_COLNAME, TY_CHAR)

	offset = SZ_SIZINFO +
		TB_MAXPAR(tp) * SZ_PACKED_REC +
		(colnum-1) * SZ_COLDEF + 1
        call seek (TB_FILE(tp), offset)

	if (SZ_INT == SZ_INT32) {

	    stat = read (TB_FILE(tp), Memi[coldef], SZ_COLDEF)
	    if (stat == EOF)
	        call error (ER_TBCINFMISSING,
			"tbcrcd:  EOF while reading column info for table")

	    # Copy the column definition that we just read from the file into
	    # the column descriptor in memory.
	    COL_NUMBER(cp) = CD_COL_NUMBER(coldef)
	    COL_OFFSET(cp) = CD_COL_OFFSET(coldef)
	    COL_LEN(cp)    = CD_COL_LEN(coldef)
	    COL_DTYPE(cp)  = CD_COL_DTYPE(coldef)

	    COL_NELEM(cp)  = tbalen (cp)
	    # COL_TDTYPE, COL_TSCAL, COL_TZERO are only relevant for FITS tables
	    COL_TDTYPE(cp) = COL_DTYPE(cp)
	    COL_TSCAL(cp)  = 1.d0
	    COL_TZERO(cp)  = 0.d0

	    # Check for and correct data type TY_CHAR.
	    if (COL_DTYPE(cp) == TBL_TY_CHAR)
	        COL_DTYPE(cp) = -COL_LEN(cp) * SZB_CHAR

	    call tbbncp1 (CD_COL_NAME(coldef), COL_NAME(cp),
		SZ_CD_COLNAME / SZB_CHAR)
	    call strupk (COL_NAME(cp), COL_NAME(cp), SZ_COLNAME)

	    call tbbncp1 (CD_COL_UNITS(coldef), COL_UNITS(cp),
		SZ_CD_COLUNITS / SZB_CHAR)
	    call strupk (COL_UNITS(cp), COL_UNITS(cp), SZ_COLUNITS)

	    # include a leading '%' in the print format
	    Memc[pformat] = '%'
	    call tbbncp1 (CD_COL_FMT(coldef), Memc[pformat+1],
		SZ_CD_COLFMT / SZB_CHAR)
	    call strupk (Memc[pformat+1], Memc[pformat+1], SZ_COLFMT-1)
	    call strcpy (Memc[pformat], COL_FMT(cp), SZ_COLFMT)

	} else {
	    # Read the first four int values.
	    stat = read (TB_FILE(tp), Memi[coldef], 4 * SZ_INT32)
	    call iupk32 (Memi[coldef], Memi[coldef], 4 * SZ_INT32)

	    # Copy the column definition that we just read from the file into
	    # the column descriptor in memory.
	    COL_NUMBER(cp) = CD_COL_NUMBER(coldef)
	    COL_OFFSET(cp) = CD_COL_OFFSET(coldef)
	    COL_LEN(cp)    = CD_COL_LEN(coldef)
	    COL_DTYPE(cp)  = CD_COL_DTYPE(coldef)

	    COL_NELEM(cp)  = tbalen (cp)
	    COL_TDTYPE(cp) = COL_DTYPE(cp)
	    COL_TSCAL(cp)  = 1.d0
	    COL_TZERO(cp)  = 0.d0

	    # Check for and correct data type TY_CHAR.
	    if (COL_DTYPE(cp) == TBL_TY_CHAR)
	        COL_DTYPE(cp) = -COL_LEN(cp) * SZB_CHAR

	    call aclrc (Memc[temp], SZ_COLNAME)
	    call aclrc (COL_NAME(cp), SZ_COLNAME)
	    stat = read (TB_FILE(tp), Memc[temp], SZ_CD_COLNAME/SZB_CHAR)
	    call strupk (Memc[temp], COL_NAME(cp), SZ_COLNAME)

	    call aclrc (Memc[temp], SZ_COLUNITS)
	    call aclrc (COL_UNITS(cp), SZ_COLUNITS)
	    stat = read (TB_FILE(tp), Memc[temp], SZ_CD_COLUNITS/SZB_CHAR)
	    call strupk (Memc[temp], COL_UNITS(cp), SZ_COLUNITS)

	    call aclrc (Memc[temp], SZ_COLFMT)
	    call aclrc (Memc[pformat], SZ_COLFMT)
	    call aclrc (COL_FMT(cp), SZ_COLFMT)
	    # include a leading '%' in the print format
	    Memc[pformat] = '%'
	    stat = read (TB_FILE(tp), Memc[temp], SZ_CD_COLFMT/SZB_CHAR)
	    call strupk (Memc[temp], Memc[temp], SZ_COLFMT)
	    call strcpy ("%", COL_FMT(cp), SZ_COLFMT)
	    call strcat (Memc[temp], COL_FMT(cp), SZ_COLFMT)
	}

	call sfree (sp)
end

# tbbncp1 -- string copy
# This routine just copies ncopy characters to the output string.  It is
# used because some of the strings to be copied are macros that would not
# allow using a subscript.
#
# Note that exactly ncopy characters are copied, regardless of whether
# there's an EOS or not.  An end-of-string will be added at ncopy+1; this
# distinguishes tbbncp1 from tbbncp0.

procedure tbbncp1 (in, out, ncopy)

char	in[ARB]		# i: input string
char	out[ARB]	# o: output string
int	ncopy		# i: number of char to copy to out
#--
int	k

begin
	do k = 1, ncopy
	    out[k] = in[k]

	out[ncopy+1] = EOS
end

# tbalen -- number of elements in array
# This routine returns the number of elements in a table entry.

int procedure tbalen (cptr)

pointer cptr		# i: pointer to column descriptor
#--
int	clen		# length in char of entire entry
int	value		# this will be returned
int	tbeszt()	# size in char of one element of type text

begin
	clen = COL_LEN(cptr)

	switch (COL_DTYPE(cptr)) {
	case TBL_TY_REAL:
	    if (clen > SZ_REAL)
		value = clen / SZ_REAL
	    else
		value = 1

	case TBL_TY_DOUBLE:
	    if (clen > SZ_DOUBLE)
		value = clen / SZ_DOUBLE
	    else
		value = 1

	case TBL_TY_INT:
	    if (clen > SZ_INT32)
		value = clen / SZ_INT32
	    else
		value = 1

	case TBL_TY_SHORT:
	    if (clen > SZ_SHORT)
		value = clen / SZ_SHORT
	    else
		value = 1

	case TBL_TY_BOOL:
	    if (clen > SZ_BOOL)
		value = clen / SZ_BOOL
	    else
		value = 1

	default:
	    value = clen / tbeszt (cptr)	# char string
	}

	return (value)
end
