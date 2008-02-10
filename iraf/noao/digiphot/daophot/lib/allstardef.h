# ALLSTAR Structure

define	LEN_ALLSTARSTRUCT (80)

define	DP_ISCACHE	Memi[P2I($1)]	# is data cached (default no)
define	DP_ALLCACHE	Memi[P2I($1+2)]	# is all the data cached ? (yes)
define	DP_CACHE	Memi[P2I($1+3+$2-1)] # which data is cached ?
define	DP_SZCACHE	Memz[P2Z($1+6)]	# current working set size
define	DP_SZOLDCACHE	Memz[P2Z($1+7)]	# old working set size
define	DP_DATA		Memp[$1+8]	# pointer to data
define	DP_SUBT		Memp[$1+9]	# pointer to subt
define	DP_WEIGHTS	Memp[$1+10]	# pointer to weights

define	DP_SBUF		Memp[$1+13]	# local subt buffer
define	DP_SNX		Memi[P2I($1+14)]	# subt x dimension
define	DP_SNY		Memi[P2I($1+15)]	# subt y dimension
define	DP_SLX		Memi[P2I($1+16)]	# subt lower left x coord
define	DP_SMX		Memi[P2I($1+17)]	# subt lower right x coord
define	DP_SLY		Memi[P2I($1+18)]	# subt lower left y coord
define	DP_SMY		Memi[P2I($1+19)]	# subt lower right y coord
define	DP_SXOFF	Memi[P2I($1+20)]	# subt lower left x offset
define	DP_SYOFF	Memi[P2I($1+21)]	# subt lower left y offset

define	DP_WBUF		Memp[$1+23]	# local weight buffer
define	DP_WNX		Memi[P2I($1+24)]	# weight x dimension
define	DP_WNY		Memi[P2I($1+25)]	# weight y dimension
define	DP_WLX		Memi[P2I($1+26)]	# weight lower left x coord
define	DP_WMX		Memi[P2I($1+27)]	# weight lower right x coord
define	DP_WLY		Memi[P2I($1+28)]	# weight lower left y coord
define	DP_WMY		Memi[P2I($1+29)]	# weight lower right y coord
define	DP_WXOFF	Memi[P2I($1+30)]	# weight lower left x offset
define	DP_WYOFF	Memi[P2I($1+31)]	# weight lower left y offset

define	DP_DBUF		Memp[$1+33]	# local weight buffer
define	DP_DNX		Memi[P2I($1+34)]	# weight x dimension
define	DP_DNY		Memi[P2I($1+35)]	# weight y dimension
define	DP_DLX		Memi[P2I($1+36)]	# weight lower left x coord
define	DP_DMX		Memi[P2I($1+37)]	# weight lower right x coord
define	DP_DLY		Memi[P2I($1+38)]	# weight lower left y coord
define	DP_DMY		Memi[P2I($1+39)]	# weight lower right y coord
define	DP_DXOFF	Memi[P2I($1+40)]	# weight lower left x offset
define	DP_DYOFF	Memi[P2I($1+41)]	# weight lower left y offset

define	DP_ANUMER	Memp[$1+55]	# pointer to the anumer1 directory
define	DP_ADENOM	Memp[$1+57]	# pointer to the adenom1 directory
define	DP_ARPIXSQ	Memp[$1+59]	# pointer to the rpixsq directory
define	DP_ASUMWT	Memp[$1+60]	# pointer to the sumwt directory
define	DP_ASKIP	Memp[$1+61]	# pointer to the skip directory
define	DP_ALAST	Memp[$1+62]	# pointer to the last directory
define	DP_AXOLD	Memp[$1+63]	# pointer to the xold array
define	DP_AYOLD	Memp[$1+64]	# pointer to the yold array
define	DP_AXCLAMP	Memp[$1+65]	# pointer to the xclamp array
define	DP_AYCLAMP	Memp[$1+66]	# pointer to the yclamp array
define	DP_AX		Memp[$1+67]	# pointer to the x array
define	DP_AV		Memp[$1+68]	# pointer to the v array
define	DP_AC		Memp[$1+69]	# pointer to the c array
define	DP_ANPIX	Memp[$1+70]	# pointer to the npix array
define	DP_AIER		Memp[$1+71]	# pointer to the error array

define	A_SUBT		1
define	A_WEIGHT	2
define	A_DCOPY		3

# Separation criterion

define	FRACTION_MINSEP	0.14
define	CLAMP_FRACTION	0.25

# Limit on N/S ** 2 for two stars to merge.

define	WCRIT0		0.0
define	WCRIT5		1.0
define	WCRIT10		1.5
define	WCRIT15		2.0

# Minimum and maximum sharpness limits.

define	MIN_SHARP	-99.99
define	MAX_SHARP	99.99

# Number of output files columns

define	ALL_NOUTCOLUMN	11

# Define the fitting contstants and constraints

define	RADIUS_FRACTION	     0.95      # % decrease in radius for regrouping
define	DENSE_RADIUS1	     1.2       # 1st limiting radius for regrouping
define	DENSE_RADIUS2	     0.8       # 2nd limiting radius for regrouping
define	MIN_ITER	     4	       # minimum number of iterations
define	MAX_RELERR	     100.0     # maximum relative error	
define	MAX_RSQ		     0.999999  # maximum values of ratio of radii ** 2
define	MAX_RHOSQ	     36.0      # limit on core radius ** 2 for sharp	
define	CHI_NORM	     1.2533141 # sqrt (PI / 2.0)
define	MIN_SUMWT	     3.0       # min value for radial weight sum
define	MIN_NPIX	     4         # min pixels per star for a fit
define	MIN_XYCLAMP	     0.001     # min value of x-y clamp
define	MIN_XYCLAMP_FRACTION 0.5       # min change in value of x-y clamp
define	MAX_XYCLAMP_FRACTION 1.2       # max change in value of x-y clamp
define	MAX_DELTA_FAINTER    0.84      # max permitted brightness decrease
define	MAX_DELTA_BRIGHTER   5.25      # max permitted brightness increase
define	MAX_NEW_ERRMAG	     0.10      # 1st convergence check on mag error
define	MAX_NEW_RELBRIGHT2   0.0005    # 2nd convergenge check on mag
define	MAX_PIXERR2	     4.0e-6    # 2nd convergence check on x/y 
define	MIN_REL_BRIGHT	     1.0e-5    # min relative brightness

# List of ALLSTAR error codes

define	ALLERR_OK		0
define	ALLERR_BIGGROUP		1
define	ALLERR_INDEFSKY		2
define	ALLERR_NOPIX		3
define	ALLERR_SINGULAR		4
define	ALLERR_FAINT		5
define	ALLERR_MERGE		6
define	ALLERR_OFFIMAGE		7
