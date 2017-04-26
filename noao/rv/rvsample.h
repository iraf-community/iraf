# Correlation sample regions data structure definition file.  The pointers
# into this structure are pre-allocated and defined in "rvpackage.h".

define	SZ_SAMPSTRUCT		10		# Size of the sample structure
define	MAX_SAMPLES	        64		# Max number of samples

# Sample regions structure definiton.
define  SR_UNITS	Memi[$1]    		# Range specifications units
define	SR_COUNT	Memi[$1+1]		# No. of range sections
define	SR_ERANGE	Memi[$1+2]		# Array of starting points (ptr)
define	SR_SRANGE	Memi[$1+3]		# Array of ending points (ptr)
define	SR_NPSAMP	Memi[$1+4]		# Npts in sample region (ptr)

define	SR_IMTYPE	Memi[$1+5]		# Image type for sample
define	SR_MODIFY	Memi[$1+6]		# Sample was modified
define	SR_PARENT	Memi[$1+7]		# Structure parent pointer
define	SR_W0		Memr[P2R($1+8)]		# Structure W0 value
define	SR_WPC		Memr[P2R($1+9)]		# Structure WPC value


#####################  END OF STRUCTURE DEFINITIONS  ##########################

# De-reference the structure elements into something readable.  Sample
# regions are referenced as (e.g.) "OSRANGE(rv,i)" where 'i' is the sample
# of interest.  We just want to pass a pointer into work routines but we
# also want to address object and template samples individually.

# For these definitions the "$1" is the pointer to the sample struct.
define	SRANGE		Memr[SR_SRANGE($1)+$2-1]    # Start of range
define	ERANGE		Memr[SR_ERANGE($1)+$2-1]    # End of range
define	NPSAMP		Memi[SR_NPSAMP($1)+$2-1]    # NPTS in range

# For these definitions the "$1" is the main rv struct pointer.
define	ORUNITS		SR_UNITS(RV_OSAMPLE($1))    # Object sample units
define	ORCOUNT		SR_COUNT(RV_OSAMPLE($1))    # Object # of samples
define	OSRANGE		SRANGE(RV_OSAMPLE($1),$2)   # Object start of range
define	OERANGE		ERANGE(RV_OSAMPLE($1),$2)   # Object end of range
define	ONPSAMP		NPSAMP(RV_OSAMPLE($1),$2)   # Object npts in sample

define	RRUNITS		SR_UNITS(RV_RSAMPLE($1))    # Temp. sample units
define	RRCOUNT		SR_COUNT(RV_RSAMPLE($1))    # Temp. # of samples
define	RSRANGE		SRANGE(RV_RSAMPLE($1),$2)   # Temp. start of range
define	RERANGE		ERANGE(RV_RSAMPLE($1),$2)   # Temp. end of range
define	RNPSAMP		NPSAMP(RV_RSAMPLE($1),$2)   # Temp. npts in sample
