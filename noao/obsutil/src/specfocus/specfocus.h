# Data structures for SPECFOCUS

define	SZ_SFFNAME	79		  # Length of file names
define	LEN_SF		54		  # Length of image data structure

define	SF_IMAGE	Memc[P2C($1)]	  # Image name
define	SF_FOCUS	Memr[P2R($1+40)]  # Focus
define	SF_WIDTH	Memr[P2R($1+41)]  # Width
define	SF_LEVEL	Memr[P2R($1+42)]  # Level of width
define	SF_AXIS		Memi[$1+43]	  # Dispersion axis
define	SF_X1		Memi[$1+44]	  # Start of dispersion sampling
define	SF_DX		Memi[$1+45]	  # Dispersion sampling step
define	SF_NX		Memi[$1+46]	  # Number of dispersion samples
define	SF_Y1		Memi[$1+47]	  # Start of cross-dispersion sampling
define	SF_DY		Memi[$1+48]	  # Cross-dispersion sampling step
define	SF_NY		Memi[$1+49]	  # Number of cross-dispersion samples
define	SF_SFD		Memi[$1+50]	  # Pointer to data structures
define	SF_NSFD		Memi[$1+51]	  # Number of data structures
define	SF_DATA		Memi[$1+52]	  # Pointer to spectrum data 
define	SF_NPIX		Memi[$1+53]	  # Number of pixels per spectrum

define	LEN_SFD		8		  # Length of spectrum data structure

define	SF_X		Memr[P2R($1)]	  # Dispersion axis coordinate
define	SF_Y		Memr[P2R($1+1)]	  # Spatial axis coordinate
define	SF_SPEC		Memi[$1+2]	  # Pointer to spectrum
define	SF_ASI		Memi[$1+3]	  # Pointer to correlation profile
define	SF_FOC		Memr[P2R($1+4)]	  # Focus
define	SF_WID		Memr[P2R($1+5)]	  # Width
define	SF_POS		Memr[P2R($1+6)]	  # Position
define	SF_DEL		Memi[$1+7]	  # Deleted?

define	SFD		Memi[SF_SFD($1)+($3-1)*SF_NX($1)+$2-1]
