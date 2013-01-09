include "tbtables.h"
include "tbltext.h"

# tbznew -- Create a new table
# This opens the file for a new table of type text file.
# It also allocates memory for each column for storing the column values
# while the table is open.
#
# Phil Hodge, 14-Jan-1992  Subroutine created.
# Phil Hodge,  5-Mar-1993  Check if comment buffer is already allocated.
# Phil Hodge,  7-Jun-1999  Allocate space for the list of keywords.

procedure tbznew (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer cp		# pointer to a column descriptor
int	colnum		# column number
int	fd		# fd for table file
int	open()
errchk	open, tbzadd

begin
	# This was split into two lines so that if the open fails,
	# TB_FILE(tp) will be unchanged (should be NULL).
	fd = open (TB_NAME(tp), TB_IOMODE(tp), TEXT_FILE)
	TB_FILE(tp) = fd

	# If a size hasn't been set for the number of rows, assign a default.
	if (TB_ALLROWS(tp) < 1)
	    TB_ALLROWS(tp) = 100			# default value

	# Allocate memory for each column.
	do colnum = 1, TB_NCOLS(tp) {
	    cp = TB_COLINFO(tp,colnum)
	    call tbzadd (tp, cp)
	}

	# Allocate space for the list of keywords.
	if (TB_MAXPAR(tp) <= 0)
	    TB_MAXPAR(tp) = INCR_N_KEYWORDS
	call calloc (TB_KEYLIST_PTR(tp), TB_MAXPAR(tp), TY_POINTER)

	# Allocate space for the comment buffer.
	if (TB_COMMENT(tp) == NULL) {
	    call calloc (TB_COMMENT(tp), SZ_LINE, TY_CHAR)
	    TB_SZ_COMMENT(tp) = SZ_LINE
	    Memc[TB_COMMENT(tp)] = EOS
	}
end
