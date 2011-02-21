# Structure definition for the FRMEDIAN task

define	LEN_FRMEDIAN_STRUCT	15

define	FRMED_NRING	Memi[$1]    # the number of elements in the filter
define	FRMED_MAP	Memi[$1+1]  # map image pixel to histogram scale
define	FRMED_HMIN	Memi[$1+2]  # the minimum histogram bin
define	FRMED_HMAX	Memi[$1+3]  # the maximum histogram bin
define  FRMED_HLOW      Memi[$1+4]  # histogram low side rejection parameter
define  FRMED_HHIGH     Memi[$1+5]  # histogram high side rejection parameter
define	FRMED_UNMAP	Memi[$1+6]  # rescale the quantizied values
define	FRMED_ZMIN	Memr[P2R($1+7)]   # the data minimum
define	FRMED_ZMAX	Memr[P2R($1+8)]   # the data maximum
define	FRMED_Z1	Memr[P2R($1+9)]   # the requested data minimum
define	FRMED_Z2	Memr[P2R($1+10)]  # the requested data maximum
define  FRMED_ZLOW      Memr[P2R($1+11)]  # data low side rejection parameter
define  FRMED_ZHIGH     Memr[P2R($1+12)]  # data high side rejection parameter
