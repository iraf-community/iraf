# Detection parameter structure.
define	DET_LEN		62		 # Length of parameter structure
define	DET_STRLEN	99		 # Length of strings in structure

define	DET_CNV		P2C($1)		 # Convolution string
define	DET_HSIG	Memr[P2R($1+51)] # High detection sigma
define	DET_LSIG	Memr[P2R($1+52)] # Low detection sigma
define	DET_HDETECT	Memi[$1+53]	 # Detect above sky?
define	DET_LDETECT	Memi[$1+54]	 # Detect below sky?
define	DET_NEIGHBORS	Memi[$1+55]	 # Neighbor type
define	DET_MINPIX	Memi[$1+56]	 # Minimum number of pixels
define	DET_SIGAVG	Memr[P2R($1+57)] # Minimum average above sky in sigma
define	DET_SIGPEAK	Memr[P2R($1+58)] # Minimum peak above sky in sigma
define	DET_FRAC2	Memr[P2R($1+59)] # Fraction of difference relative to 2
define	DET_BPVAL	Memi[$1+60]	 # Output bad pixel value
define	DET_SKB		Memi[$1+61]	 # Parameters for sky update
