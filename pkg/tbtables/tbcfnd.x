include <tbset.h>
include "tbtables.h"

# tbcfnd -- find columns
# Get column-descriptor pointers from column names.  For each column that
# is not found, the corresponding colptr will be NULL.
# For a text table, a column name should be "c" followed by the column
# number.  If just the number was specified, we will return the pointer
# to that column rather than requiring the name to begin with "c".
#
# Phil Hodge,  1-Jun-1989  Find columns without regard to case.
# Phil Hodge, 24-Jun-1992  For text table, name can be cN or just N.
# Phil Hodge,  2-Mar-1993  For text table, if the name is just a number,
#			check that nothing follows the number.
# Phil Hodge, 14-Apr-1998  Use strcpy instead of strupk.

procedure tbcfnd (tp, colname, colptr, numcols)

pointer tp				# i: pointer to table descriptor
char	colname[SZ_COLNAME,numcols]	# i: array of column names
pointer colptr[numcols]			# o: array of ptrs to column descriptors
int	numcols				# i: length of arrays colname & colptr
#--
pointer cp				# pointer to column descriptor
char	icname[SZ_COLNAME]		# a column name to be found
char	tcname[SZ_COLNAME]		# a name of a column in the table
int	j, k, col			# loop indexes; current column number
int	cnum				# column number if name is a number
int	ip, ctoi()
bool	streq()

begin
	col = 1				# start searching with first column

	do j = 1, numcols {		# do for each column to be found

	    colptr[j] = NULL		# not found yet

	    call strcpy (colname[1,j], icname, SZ_COLNAME)
	    call strlwr (icname)

	    # For a text table, if the column name is just a number,
	    # return the pointer for that column number.
	    if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
		ip = 1
		if (ctoi (icname, ip, cnum) > 0) {
		    if (icname[ip] == EOS) {	# nothing follows number
			colptr[j] = TB_COLINFO(tp,cnum)
			next
		    }
		}
	    }

	    do k = 1, TB_NCOLS(tp) {	# do for each column in the table
		if (col > TB_NCOLS(tp))
		    col = 1
		cp = TB_COLINFO(tp,col)
		# Copy column name from column descriptor to scratch.
		call strcpy (COL_NAME(cp), tcname, SZ_COLNAME)
		call strlwr (tcname)
		if (streq (icname, tcname)) {
		    colptr[j] = cp	# found it
		    break
		} else {
		    col = col + 1
		}
	    }
	}
end
