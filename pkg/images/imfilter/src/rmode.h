# Structure definition for the RMODE task

define	LEN_RMODE_STRUCT	10

define	RMOD_NRING	Memi[$1]    # the number of elements in the filter
define  RMOD_NLOW	Memi[$1+1]  # number of low rejected pixels
define  RMOD_NHIGH	Memi[$1+2]  # number of high rejected pixels
define  RMOD_ZLOW	Memr[$1+3]  # data low side rejection parameter
define  RMOD_ZHIGH	Memr[$1+4]  # data high side rejection parameter
