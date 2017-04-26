# Multicolumn table structure.

# Pointer Mem
define	MEMP	Memi

# Magic number
define	MAGIC		1989

# Growing factor computation. This formula is used to compute
# the increment is memory allocation in terms of the memory
# already allocated, but avoiding a zero or a large factor.
# The real number in the formula represents the fraction to grow,
# and it should be greater than zero and less than one, although
# the formula takes care of out of range values.
define	GROWFACTOR	min (max (int ($1 * 0.5), 1), $1)	# 50%

# Multi-column table structure
define	LEN_MCTABLE	10			# structure size
define	MCT_MAGIC	Memi[$1+0]		# magic number
define	MCT_TYPE	Memi[$1+1]		# table data type
define	MCT_MAXROW	Memi[$1+2]		# max number of rows (growing)
define	MCT_MAXCOL	Memi[$1+3]		# max number of columns (fixed)
define	MCT_INCROWS	Memi[$1+4]		# row growing increment
define	MCT_NPROWS	Memi[$1+5]		# highest row entered
define	MCT_NPCOLS	Memi[$1+6]		# highest column entered
define	MCT_NGROWS	Memi[$1+7]		# highest row gotten
define	MCT_NGCOLS	Memi[$1+8]		# highest column gotten
define	MCT_DATA	MEMP[$1+9]		# data buffer pointer
