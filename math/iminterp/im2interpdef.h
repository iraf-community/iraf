# Internal definitions for the 2D interpolator structure

define	LEN_MSISTRUCT	14

define	MSI_TYPE	Memi[$1]	# interpolant type
define	MSI_NSINC	Memi[$1+1]	# interpolant type
define	MSI_NXINCR	Memi[$1+2]	# interpolant type
define	MSI_NYINCR	Memi[$1+3]	# interpolant type
define	MSI_XSHIFT	Memr[P2R($1+4)]	# x shift
define	MSI_YSHIFT	Memr[P2R($1+5)]	# y shift
define	MSI_XPIXFRAC	Memr[P2R($1+6)]	# x pixel fraction for drizzle
define	MSI_YPIXFRAC	Memr[P2R($1+7)]	# y pixel fraction for drizzle
define	MSI_NXCOEFF	Memi[$1+8]	# x dimension of coefficient array
define	MSI_NYCOEFF	Memi[$1+9]	# y dimension of coefficient array
define	MSI_COEFF	Memi[$1+10]	# pointer to coefficient array
define	MSI_FSTPNT	Memi[$1+11]	# offset to first data point in coeff
define	MSI_LTABLE	Memi[$1+12]	# offset to first data point in coeff
define	MSI_BADVAL	Memr[P2R($1+13)]# undefined pixel value for drizzle

# Definitions for msisave and msirestore

define	MSI_SAVETYPE		$1[1]
define	MSI_SAVENSINC		$1[2]
define	MSI_SAVENXINCR		$1[3]
define	MSI_SAVENYINCR		$1[4]
define	MSI_SAVEXSHIFT		$1[5]
define	MSI_SAVEYSHIFT		$1[6]
define	MSI_SAVEXPIXFRAC	$1[7]
define	MSI_SAVEYPIXFRAC	$1[8]
define	MSI_SAVENXCOEFF		$1[9]
define	MSI_SAVENYCOEFF		$1[10]
define	MSI_SAVEFSTPNT		$1[11]
define	MSI_SAVEBADVAL		$1[12]
define	MSI_SAVECOEFF		12

# Array element definitions
# TEMP and DIAG for spline only

define	COEFF		Memr[P2P($1)]	# element of coefficient array
define	LTABLE		Memr[P2P($1)]	# element of look-up array
define	TEMP		Memr[P2P($1)]	# element of temporary array
define	DIAG		Memr[P2P($1)]	# element of diagonal

# The since function truncation length, taper, and precision definitions
# These should be identical to those in im1interpdef.h except for the DY
# definition.

define	NSINC		15
define	NINCR		20
define	DX		0.001
define	DY		0.001
define	PIXFRAC		1.0
define	MIN_PIXFRAC	0.001
define	BADVAL		0.0

# miscellaneous defintions

define	FNROWS		5		# maximum number or rows involved in
					# boundary extension low side
define	LNROWS		7		# maximum number of rows involved in
					# high side boundary extension
define	SPLPTS		16		# number of points for spline in mrieval
define	MAX_NTERMS	6		# maximun number of terms in polynomials
