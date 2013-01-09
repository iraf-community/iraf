include <tbset.h>
include "tbtables.h"

# tbcftl -- length of print format
# This procedure reads from the column print format the number of char
# necessary to print an element from the column using that format.
#
# Phil Hodge, 14-Apr-1998  Use strcpy instead of strupk; remove pformat.

procedure tbcftl (colptr, lenfmt)

pointer colptr			# i: pointer to column descriptor
int	lenfmt			# o: number of char to print using colfmt
#--
int	ip, ival		# stuff for using ctoi to get lenfmt
int	ctoi()

begin
	ip = 2			# set ip to skip over the leading '%'
	if (ctoi (COL_FMT(colptr), ip, ival) <= 0)
	    ival = 25				# a default value
	lenfmt = abs (ival)			# be careful of e.g. "%-12s"
end
