# Include file for the MWCS structure.  A pointer is allocated in
# the main RV structure into this one.  This sub-structure contains the
# parameters used to hold the MWCS dispersion information for each image.

define	SZ_MWCS_STRUCT	     10

define	RVMW_AP	     	     Memi[RV_MWCSP($1)]		# Aperture array ptr
define  RVMW_DTYPE	     Memi[RV_MWCSP($1)+1]	# Dispersion type ptr
define  RVMW_W1	     	     Memi[RV_MWCSP($1)+2]	# W0 array ptr
define  RVMW_DW	     	     Memi[RV_MWCSP($1)+3]	# WPC array ptr
define  RVMW_NW	     	     Memi[RV_MWCSP($1)+4]	# NPIX array ptr

define	RVMW_NSPEC	     Memi[RV_MWCSP($1)+5]	# No. of spectra
define	RVMW_FORMAT	     Memi[RV_MWCSP($1)+6]	# Format (echelle, etc)

###################  End of structure definitions ##############################


# Useful Macro definitions.  All indexing is done in the macros themselves
# and pointers are assumed to be allocated at startup.

define	RMW_AP		Memi[RVMW_AP($1)+$2-1]      # Aperture array
define	RMW_DTYPE	Memi[RVMW_DTYPE($1)+$2-1]   # DC-FLAG array
define	RMW_W1		Memr[RVMW_W1($1)+$2-1]      # Start wavelength array
define	RMW_DW		Memr[RVMW_DW($1)+$2-1]      # Wavelength inc. array
define	RMW_NW		Memi[RVMW_NW($1)+$2-1]      # NPIX array
