# Structure definition for the RMEDIAN task

define	LEN_RMEDIAN_STRUCT	10

define	RMED_NRING	Memi[$1]    	 # the number of elements in the filter
define  RMED_NLOW	Memi[$1+1]  	 # number of low rejected pixels
define  RMED_NHIGH	Memi[$1+2]  	 # number of high rejected pixels
define  RMED_ZLOW	Memr[P2R($1+3)]  # data low side rejection parameter
define  RMED_ZHIGH	Memr[P2R($1+4)]  # data high side rejection parameter
