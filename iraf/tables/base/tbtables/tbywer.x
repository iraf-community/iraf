include "tbtables.h"

define	FRAC_INCR	1.2		# fractional increase in allrows
define	DEFNUMROWS	100		# minimum increase in allrows

# tbywer -- write empty rows
# The purpose of this routine is to allocate more space for rows for a
# column-ordered table.  If the specified row is within the range of
# existing rows, the table itself will not be modified.
#
# If rownum is greater than TB_ALLROWS then tbtchs will be called to
# rewrite the table and increase the allocated number of rows by a default
# amount.
#
# Note that TB_NROWS will not be updated.
#
# Phil Hodge,  4-Mar-1998  Subroutine created, extracted from tbtwer.

procedure tbywer (tp, rownum)

pointer tp		# i: pointer to table descriptor
long	rownum		# i: (actual) row number in table
#--
long	allrows		# allocated number of rows
long	l_val
errchk	tbtchs

begin
	if (rownum > TB_ALLROWS(tp)) {
	    allrows = rownum * FRAC_INCR + DEFNUMROWS
	    l_val = -1
	    call tbtchs (tp, -1, -1, l_val, allrows)	# change table size
	}
end
