# Internal definitions for the 2D interpolator structure

define	LEN_MSISTRUCT	10

define	MSI_TYPE	Memi[$1]	# interpolant type
define	MSI_NXCOEFF	Memi[$1+1]	# x dimension of coefficient array
define	MSI_NYCOEFF	Memi[$1+2]	# y dimension of coefficient array
define	MSI_COEFF	Memi[$1+3]	# pointer to coefficient array
define	MSI_FSTPNT	Memi[$1+4]	# offset to first data point in coeff

# Definitions for msisave and msirestore

define	MSI_SAVETYPE	$1[1]
define	MSI_SAVENXCOEFF	$1[2]
define	MSI_SAVENYCOEFF	$1[3]
define	MSI_SAVEFSTPNT	$1[4]
define	MSI_SAVECOEFF	4

# Array element definitions
# TEMP and DIAG for spline only

define	COEFF		Memr[$1]	# element of coefficient array
define	TEMP		Memr[$1]	# element of temporary array
define	DIAG		Memr[$1]	# element of diagonal

# miscellaneous defintions

define	FNROWS		5		# maximum number or rows involved in
					# boundary extension low side
define	LNROWS		7		# maximum number of rows involved in
					# high side boundary extension
define	SPLPTS		16		# number of points for spline in mrieval
define	MAX_NTERMS	6		# maximun number of terms in polynomials
