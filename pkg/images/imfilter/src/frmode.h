# Structure definition for the FRMODE task

define	LEN_FRMODE_STRUCT	15

define	FRMOD_NRING	Memi[$1]    	  # the number of elements in the filter
define	FRMOD_MAP	Memi[$1+1]  	  # map image to histogram
define	FRMOD_HMIN	Memi[$1+2]  	  # histogram minimum
define	FRMOD_HMAX	Memi[$1+3]  	  # histogram maximum
define  FRMOD_HLOW      Memi[$1+4]  	  # histogram low side rejection param
define  FRMOD_HHIGH     Memi[$1+5]  	  # histogram high side rejection param
define	FRMOD_UNMAP	Memi[$1+6] 	  # rescale the quantizied values
define	FRMOD_ZMIN	Memr[P2R($1+7)]   # the data minimum
define	FRMOD_ZMAX	Memr[P2R($1+8)]   # the data maximum
define	FRMOD_Z1	Memr[P2R($1+9)]   # the requested data minimum
define	FRMOD_Z2	Memr[P2R($1+10)]  # the requested data maximum
define  FRMOD_ZLOW      Memr[P2R($1+11)]  # data low side rejection parameter
define  FRMOD_ZHIGH     Memr[P2R($1+12)]  # data high side rejection parameter
