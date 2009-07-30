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
size_t	sz_val
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
	sz_val = LEN_COLDEF
	call salloc (coldef, sz_val, TY_STRUCT)

	# This assumes SZ_COLUNITS & SZ_COLFMT are no longer than SZ_COLNAME.
	sz_val = SZ_COLNAME
	call salloc (temp, sz_val, TY_CHAR)

	# Copy the column descriptor from memory into the buffer that
	# we'll write to the file.
	CD_COL_NUMBER(coldef) = COL_NUMBER(cp)
	CD_COL_OFFSET(coldef) = COL_OFFSET(cp)
	CD_COL_LEN(coldef) = COL_LEN(cp)
	CD_COL_DTYPE(coldef) = COL_DTYPE(cp)

	sz_val = SZ_COLNAME
	call strpak (COL_NAME(cp), Memc[temp], sz_val)
	call tbbncp0 (Memc[temp], CD_COL_NAME(coldef), SZ_CD_COLNAME/SZB_CHAR)

	sz_val = SZ_COLNAME
	call strpak (COL_UNITS(cp), Memc[temp], sz_val)
	call tbbncp0 (Memc[temp], CD_COL_UNITS(coldef), SZ_CD_COLUNITS/SZB_CHAR)

	call strcpy (COL_FMT(cp), Memc[temp], SZ_COLNAME)
	# skip over the leading '%' in the print format
	sz_val = SZ_COLNAME-1
	call strpak  (Memc[temp+1], Memc[temp+1], sz_val)
	call tbbncp0 (Memc[temp+1], CD_COL_FMT(coldef), SZ_CD_COLFMT/SZB_CHAR)

	colnum = COL_NUMBER(cp)
	offset = SZ_SIZINFO +
		TB_MAXPAR(tp) * SZ_PACKED_REC +
		(colnum-1) * SZ_COLDEF + 1
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_COLDEF
	call write (TB_FILE(tp), Memc[P2C(coldef)], sz_val)

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
