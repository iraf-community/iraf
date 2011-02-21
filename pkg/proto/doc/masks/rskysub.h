# Define the sky subtraction structure


define	LEN_RSKYSUB	20 + 5 * SZ_FNAME

define	RS_LOWER	Memr[P2R($1)]		# lower good data limit
define	RS_UPPER	Memr[P2R($1+1)]		# upper good data limit
define	RS_LNSIGREJ	Memr[P2R($1+2)]         # low side clipping factor
define	RS_UNSIGREJ	Memr[P2R($1+3)]         # high side clipping factor
define	RS_BINWIDTH	Memr[P2R($1+4)]         # histogram binwidth
define	RS_BLANK	Memr[P2R($1+5)]		# undefined pixel value
define	RS_RESCALE	Memi[$1+6]		# recompute scaling factor ?
define	RS_RESUBTRACT	Memi[$1+7]		# compute the subtracted image
define	RS_NCOMBINE	Memi[$1+8]		# number of images to combine
define	RS_NMIN		Memi[$1+9]		# min images to combine
define	RS_MAXITER	Memi[$1+11]             # maximum number of iterations
define	RS_COMBINE	Memi[$1+12]		# combining method
define	RS_NLOREJ	Memi[$1+13]		# low side pixels to reject
define	RS_NHIREJ	Memi[$1+14]		# high side pixels to reject
define	RS_KYFSCALE	Memc[P2C($1+15)]             # scaling factor keyword 
define	RS_ISCALES	Memc[P2C($1+15+SZ_FNAME)]    # scaling method
define	RS_STATSEC	Memc[P2C($1+15+2*SZ_FNAME)]  # statistics section
define	RS_KYSKYSUB	Memc[P2C($1+15+3*SZ_FNAME)]  # sky subtraction keyword 
define	RS_KYHMASK	Memc[P2C($1+15+4*SZ_FNAME)]  # holes mask keyword 


# Define the sky combining options

define	RS_COMBINESTR	"|average|median|"

define	RS_MEAN		1
define	RS_MEDIAN	2
