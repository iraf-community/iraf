define	THETA_X			0	# Orientation angle of x axis label
define	THETA_Y			90	# Orientation angle of y axis label
define	STEP			10	# Tick marks are placed every 10 pixels
define	LABEL			100	# Labelled ticks are multiples of 100
define	SZ_LABEL  		5	# Max number of characters in label
define	TEXT1			0.15
define	HGRAMS			0.50
define	TEXT2			0.10
define	TEXT3			0.10
define	SPACE			0.03
define	NBINS			256	# Number of bins in intensity histogram
define	SAMPLE_SIZE		1000
define	STARTPT			0.0E0
define	ENDPT			4095.0E0
define	SZ_BUF			4096

define	CRT_XS			0.210
define	CRT_XE			0.810
define	CRT_YS			0.076
define	CRT_YE			0.950

define	LEN_CLPAR 		(15 + 40 + 40 + 20)
define	FILL			Memi[$1]
define	REPLICATE		Memi[$1+1]
define	NSAMPLE_LINES		Memi[$1+2]
define	LUT			Memi[$1+3]
define	PERIM			Memi[$1+4]
define	XMAG			Memr[P2R($1+5)]
define	YMAG			Memr[P2R($1+6)]
define	CONTRAST		Memr[P2R($1+7)]
define	Z1			Memr[P2R($1+8)]
define	Z2			Memr[P2R($1+9)]
define	GREYSCALE_FRACTION	Memr[P2R($1+10)]
define	IMAGE_FRACTION		Memr[P2R($1+11)]
define	GRAPHICS_FRACTION	Memr[P2R($1+12)]
define	X_BA			Memr[P2R($1+13)]
define	Y_BA			Memr[P2R($1+14)]
define	UFILE			Memc[P2C($1+15)]
define	ZTRANS			Memc[P2C($1+55)]
define	DEVICE			Memc[P2C($1+95)]

define	NSTEPS 			16
define	SZ_LABEL		5
