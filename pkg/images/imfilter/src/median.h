# Definitions file for the MEDIAN task.

define	LEN_MEDIAN_STRUCT	15

define	MED_XBOX	Memi[$1]	# the x width of the filtering window
define	MED_YBOX	Memi[$1+1]	# the y width of the filtering window
define	MED_NPTS	Memi[$1+2]	# the number of points in window
define	MED_NPTSP1	Memi[$1+3]	# the number of points in window + 1
define	MED_MP		Memi[$1+4]	# the median pointer
define	MED_START	Memi[$1+5]	# index of the first elememt
define	MED_FINISH	Memi[$1+6]	# index of the last elememt
define	MED_ZLOW	Memr[P2R($1+7)]	# the low pixel cutoff value
define	MED_ZHIGH	Memr[P2R($1+8)]	# the high pixel cutoff value
define	MED_NLOW	Memi[$1+9]	# the number of low side rejected pts
define	MED_NHIGH	Memi[$1+10]	# the number of high side rejected pts
