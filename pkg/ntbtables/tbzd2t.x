include <mach.h>		# for MAX_DIGITS, NDIGITS_DP and SZB_CHAR
include "tbtables.h"

# tbzd2t -- convert column of type double to text
# When reading a text table into memory, if non-numeric text is found in
# a column of type double, this routine may be called to convert the
# data type to text.
#
# Phil Hodge,  7-Jun-1994  Subroutine created.
# Phil Hodge, 10-Aug-1994  Update COL_LEN.

procedure tbzd2t (tp, cp, width, precision, fmt_code)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	width		# i: the maximum width for this column
int	precision	# i: the precision for writing this column
char	fmt_code	# i: format code for print format
#--
pointer sp
pointer pform		# scratch for print format for this column
pointer new		# pointer to new memory for column data
int	wid, prec	# width & precision, modified for print format
int	row		# row number
int	op		# offset in new char array
errchk	calloc

begin
	call smark (sp)
	call salloc (pform, SZ_FNAME, TY_CHAR)

	# Assign the print format for writing previous values into
	# the text strings.  Left justify.
	Memc[pform] = '%'
	Memc[pform+1] = '-'

	wid = min (width, MAX_DIGITS)
	prec = precision
	if (fmt_code == 'g')
	    prec = max (prec, wid-2) 		# maximum precision
	prec = min (prec, NDIGITS_DP)
	call sprintf (Memc[pform+2], SZ_FNAME-2, "%d.%d%c")
	    call pargi (wid)
	    call pargi (prec)
	    call pargc (fmt_code)

	# Allocate memory for the array of strings.  Note that we use
	# width rather than wid.
	call calloc (new, (width+1) * TB_ALLROWS(tp), TY_CHAR)

	op = 0					# initial value

	# Copy each row.
	do row = 1, TB_NROWS(tp) {

	    if (IS_INDEFD(Memd[COL_OFFSET(cp)+row-1])) {
		Memc[new+op] = EOS
	    } else {
		call sprintf (Memc[new+op], width, Memc[pform])
		    call pargd (Memd[COL_OFFSET(cp) + row - 1])
	    }

	    op = op + width + 1			# add one for EOS
	}

	# Free the old memory, and save the new pointer.
	call mfree (COL_OFFSET(cp), TY_DOUBLE)
	COL_OFFSET(cp) = new

	# Specify the new data type and width.
	COL_DTYPE(cp) = -width
	COL_LEN(cp) = (width + SZB_CHAR-1) / SZB_CHAR

	call sfree (sp)
end
