# Include file for the Filter structure.  A pointer is allocated in
# the main RV structure into this one.  This sub-structure contains the
# parameters used for data filter while in Fourier space as well as
# the option flags for filter function types

define	SZ_FILTSTRUCT	     10

define	RVF_FILTTYPE	     Memi[RV_FILTP($1)]		# Filter type code
define  RVF_CUTOFF	     Memi[RV_FILTP($1)+1]	# Cuton wavenumber
define  RVF_CUTON	     Memi[RV_FILTP($1)+2]	# Cuton wavenumber
define  RVF_FULLOFF	     Memi[RV_FILTP($1)+3]	# Fulloff wavenumber
define  RVF_FULLON	     Memi[RV_FILTP($1)+4]	# Fullon wavenumber

define  RVF_LASTKEY	     Memi[RV_FILTP($1)+5]	# Last fftmode comm.
 
######################  END  OF  STRUCTURE  DEFINITIONS  ######################

#  Filter function flags
define	RV_FTYPES	"|square|ramp|welch|hanning|"
define  SQUARE			1		# Step function
define	RAMP			2		# Ramp function
define	WELCH			3		# Welch function
define	HANNING			4		# Hanning function
