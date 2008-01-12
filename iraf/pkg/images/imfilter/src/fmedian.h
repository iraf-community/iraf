# Structure definition for the FMEDIAN task

define	LEN_FMEDIAN_STRUCT	20

define	FMED_XBOX	Memi[P2I($1)]    # x median filtering window
define	FMED_YBOX	Memi[P2I($1+1)]  # y median filtering window
define	FMED_MAP	Memi[P2I($1+2)]  # map image to histogram
define	FMED_HMIN	Memi[P2I($1+3)]  # histogram minimum
define	FMED_HMAX	Memi[P2I($1+4)]  # histogram maximum
define	FMED_HLOW	Memi[P2I($1+5)]  # histogram low side rejection parameter
define	FMED_HHIGH	Memi[P2I($1+6)]  # histogram high side rejection parameter
define	FMED_NHLOW	Memi[P2I($1+7)]  # number of low rejected pixels
define	FMED_NHHIGH	Memi[P2I($1+8)]  # number of high rejected pixels
define	FMED_MEDIAN	Memi[P2I($1+9)]  # the current median
define	FMED_NMEDIAN	Memi[P2I($1+10)]  # number less than the median
define	FMED_NLTMEDIAN	Memi[P2I($1+11)]  # number less than the current median
define	FMED_UNMAP	Memi[P2I($1+12)] # rescale the quantizied values
define	FMED_ZMIN	Memr[P2R($1+13)] # the data minimum
define	FMED_ZMAX	Memr[P2R($1+14)] # the data maximum
define	FMED_Z1		Memr[P2R($1+15)] # the requested data minimum
define	FMED_Z2		Memr[P2R($1+16)] # the requested data maximum
define	FMED_ZLOW	Memr[P2R($1+17)] # data low side rejection parameter
define	FMED_ZHIGH	Memr[P2R($1+18)] # data high side rejection parameter
