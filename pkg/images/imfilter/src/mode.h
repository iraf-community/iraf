# Definitions file for the MODE task.

define	LEN_MODE_STRUCT	15

define	MOD_XBOX	Memi[$1]	  # the x width of the filtering window
define	MOD_YBOX	Memi[$1+1]	  # the y width of the filtering window
define	MOD_NPTS	Memi[$1+2]	  # the number of points in window
define	MOD_NPTSP1	Memi[$1+3]	  # the number of points in window + 1
define	MOD_MP		Memi[$1+4]	  # the median pointer
define	MOD_START	Memi[$1+5]	  # index of the first elememt
define	MOD_FINISH	Memi[$1+6]	  # index of the last elememt
define	MOD_ZLOW	Memr[P2R($1+7)]	  # the low pixel cutoff value
define	MOD_ZHIGH	Memr[P2R($1+8)]	  # the high pixel cutoff value
define	MOD_NLOW	Memi[$1+9]	  # the number of low side rejected pts
define	MOD_NHIGH	Memi[$1+10]	  # the number of high side rejected pts
define	MOD_SUM		Memr[P2R($1+11)]  # running sum used for computing mean 
