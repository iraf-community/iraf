# Detection parameter structure.
define	SPT_LEN		10		# Length of parameter structure

define	SPT_NEIGHBORS	Memi[$1]	# Neighbor type
define	SPT_SPLITMAX	Memr[$1+1]	# Maximum convolved sigma for splitting
define	SPT_SPLITSTEP	Memr[$1+2]	# Minimum split step in convolved sigma
define	SPT_SPLITTHRESH	Memr[$1+3]	# Transition convolved sigma
define	SPT_MINPIX	Memi[$1+4]	# Minimum number of pixels
define	SPT_SIGAVG	Memr[$1+5]	# Minimum average above sky in sigma
define	SPT_SIGPEAK	Memr[$1+6]	# Minimum peak above sky in sigma
define	SPT_SMINPIX	Memi[$1+7]	# Minimum number of split pixels
define	SPT_SSIGAVG	Memr[$1+8]	# Minimum split avg above sky in sigma
define	SPT_SSIGPEAK	Memr[$1+9]	# Minimum split peak above sky in sigma
