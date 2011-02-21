include "tbtables.h"

# tbzt2t -- increase column width
# When reading a text table into memory, if the word in a column of type
# text is longer than the allocated space, this routine may be called to
# increase the column width.
#
# Phil Hodge,  7-Jun-1994  Subroutine created.

procedure tbzt2t (tp, cp, width)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	width		# i: the new max width for this column
#--
pointer sp
pointer message		# scratch for possible error message
pointer new		# pointer to new memory for column data
int	oldwidth	# previous value of column width
int	row		# row number
int	ip, op		# offsets in char array
errchk	calloc

begin
	oldwidth = -COL_DTYPE(cp)

	if (width <= oldwidth)
	    return				# it's OK as is

	if (width > SZ_LINE-1) {
	    call smark (sp)
	    call salloc (message, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[message], SZ_LINE,
		"string is too long for a table; the maximum is %s")
		call pargi (SZ_LINE-1)
	    call error (1, Memc[message])
	}

	# Allocate memory for the new, wider, array of strings.
	# (add one to width for EOS)
	call calloc (new, (width+1) * TB_ALLROWS(tp), TY_CHAR)

	ip = 0					# initial values
	op = 0

	do row = 1, TB_NROWS(tp) {

	    call strcpy (Memc[COL_OFFSET(cp)+ip], Memc[new+op], width)

	    ip = ip + oldwidth + 1		# add one for EOS
	    op = op + width + 1
	}

	# Free the old memory, and save the new pointer.
	call mfree (COL_OFFSET(cp), TY_CHAR)
	COL_OFFSET(cp) = new

	# Specify the new data type.
	COL_DTYPE(cp) = -width
end
