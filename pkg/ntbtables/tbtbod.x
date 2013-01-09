include <mach.h>
include <tbset.h>
include "tbtables.h"
define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbtbod -- beginning of data
# This function returns the offset (in char) of the first element in the
# data portion of a table relative to the beginning of the file.  The offset
# includes a size-information record, maxpar records that may contain user
# parameters, and maxcols records that may contain column descriptors.
# The input arguments maxpar and maxcols would normally be TB_MAXPAR(tp)
# and TB_MAXCOLS(tp).
#
# Phil Hodge, 14-Apr-1998  Change SZ_COLSTRUCT to SZ_COLDEF.

long procedure tbtbod (maxpar, maxcols)

int	maxpar		# i: current maximum number of header parameters
int	maxcols		# i: current maximum number of columns
#--
long	offset

begin
	offset = SZ_SIZINFO +
		maxpar * SZ_PACKED_REC +
		maxcols * SZ_COLDEF + 1
	return (offset)
end
