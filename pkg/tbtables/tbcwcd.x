include <mach.h>
include <tbset.h>
include "tbtables.h"

define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbcwcd -- write column descriptor
# This procedure writes the column descriptor into the table file.
#
# Note that it is assumed that SZ_COLNAME is larger than SZ_CD_COLNAME, etc.
#
# Phil Hodge,  3-Feb-1992  Add check for text table type.
# Phil Hodge, 21-Jun-1995  Modify for FITS tables
# Phil Hodge,  5-Mar-1998  Include tp in call to tbcnum.
# Phil Hodge, 14-Apr-1998  Change calling sequence;
#			change SZ_COLSTRUCT to SZ_COLDEF;
#			EOS may be absent in table, to allow one more char.

procedure tbcwcd (tp, cp)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
#--
pointer sp
pointer coldef			# column descriptor read from table
pointer temp			# scratch for strings
long	offset			# location of column descriptor in table file
int	colnum			# column number

errchk	seek, write

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    return				# nothing to do

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfwcd (tp, cp)
	    return
	}

	call smark (sp)
	call salloc (coldef, LEN_COLDEF, TY_STRUCT)

	# This assumes SZ_COLUNITS & SZ_COLFMT are no longer than SZ_COLNAME.
	call salloc (temp, SZ_COLNAME, TY_CHAR)

	# Copy the column descriptor from memory into the buffer that
	# we'll write to the file.
	CD_COL_NUMBER(coldef) = COL_NUMBER(cp)
	CD_COL_OFFSET(coldef) = COL_OFFSET(cp)
	CD_COL_LEN(coldef)    = COL_LEN(cp)
	CD_COL_DTYPE(coldef)  = COL_DTYPE(cp)

	colnum = COL_NUMBER(cp)
	offset = SZ_SIZINFO +
		TB_MAXPAR(tp) * SZ_PACKED_REC +
		(colnum-1) * SZ_COLDEF + 1

	call strpak (COL_NAME(cp), Memc[temp], SZ_COLNAME)
	call tbbncp0 (Memc[temp], CD_COL_NAME(coldef), SZ_CD_COLNAME/SZB_CHAR)

	call strpak (COL_UNITS(cp), Memc[temp], SZ_COLNAME)
	call tbbncp0 (Memc[temp], CD_COL_UNITS(coldef), SZ_CD_COLUNITS/SZB_CHAR)

	call strcpy (COL_FMT(cp), Memc[temp], SZ_COLNAME)

	# skip over the leading '%' in the print format
	call strpak  (Memc[temp+1], Memc[temp+1], SZ_COLNAME-1)
	call tbbncp0 (Memc[temp+1], CD_COL_FMT(coldef), SZ_CD_COLFMT/SZB_CHAR)

	if (SZ_INT == SZ_INT32) {
	    call seek (TB_FILE(tp), offset)
	    call write (TB_FILE(tp), Memi[coldef], SZ_COLDEF)

	} else {
	    # Write first four members of the struct.
	    call ipak32 (Memi[coldef], Memi[coldef], 4 * SZ_INT)
	    call write (TB_FILE(tp), Memi[coldef], 4 * SZ_INT32)

	    call aclrc (Memc[temp], SZ_COLNAME)
	    call strpak (COL_NAME(cp), Memc[temp], SZ_CD_COLNAME)
	    call write (TB_FILE(tp), Memc[temp],   SZ_CD_COLNAME/SZB_CHAR)

	    call aclrc (Memc[temp], SZ_COLNAME)
	    call strpak (COL_UNITS(cp), Memc[temp], SZ_CD_COLUNITS)
	    call write (TB_FILE(tp), Memc[temp], SZ_CD_COLUNITS/SZB_CHAR)

	    call aclrc (Memc[temp], SZ_COLNAME)
	    call strcpy (COL_FMT(cp), Memc[temp], SZ_COLNAME)
	    call strpak (Memc[temp+1], Memc[temp+1], SZ_CD_COLFMT)
	    call write (TB_FILE(tp), Memc[temp+1], SZ_CD_COLFMT/SZB_CHAR)
	}

	call sfree (sp)
end

# tbbncp0 -- string copy
# This routine just copies ncopy characters to the output string.  It is
# used because some of the strings to be copied are macros that would not
# allow using a subscript.
#
# Note that exactly ncopy characters are copied, regardless of whether
# there's an EOS or not.  Note that this routine does not add an EOS after
# copying ncopy elements; this distinguishes tbbncp0 from tbbncp1.

procedure tbbncp0 (in, out, ncopy)

char	in[ARB]		# i: input string
char	out[ARB]	# o: output string
int	ncopy		# i: number of char to copy to out
#--
int	k

begin
	do k = 1, ncopy
	    out[k] = in[k]
end
