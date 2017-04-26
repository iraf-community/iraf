# STARFIND Structure

define	LEN_STARFIND (15)

define	SF_HWHMPSF	Memr[P2R($1)]	 # HWHM of the PSF in pixels
define	SF_FRADIUS	Memr[P2R($1+1)]	 # Fitting radius in HWHM
define	SF_DATAMIN	Memr[P2R($1+2)]	 # Minimum good data limit in ADU
define	SF_DATAMAX	Memr[P2R($1+3)]	 # Maximum good data limit in ADU
define	SF_THRESHOLD	Memr[P2R($1+4)]	 # Detection threshold in ADU
define	SF_SEPMIN	Memr[P2R($1+5)]	 # Minimum separation in HWHM
define	SF_SHARPLO	Memr[P2R($1+6)]	 # Lower sharpness limit
define	SF_SHARPHI	Memr[P2R($1+7)]	 # Upper sharpness limit
define	SF_ROUNDLO	Memr[P2R($1+8)]	 # Lower roundness limit
define	SF_ROUNDHI	Memr[P2R($1+9)]	 # Upper roundness limit
define	SF_MAGLO	Memr[P2R($1+10)] # Lower magnitude limit
define	SF_MAGHI	Memr[P2R($1+11)] # Upper magnitude limit
define	SF_NPIXMIN	Memi[$1+12]	 # Minimum pixels above  threshold


# default values

define	DEF_HWHMPSF	1.0
define	DEF_FRADIUS	1.5
define	DEF_THRESHOLD	0.0
define	DEF_SEPMIN	1.5
define	DEF_DATAMIN	-MAX_REAL
define	DEF_DATAMAX	MAX_REAL
define	DEF_SHARPLO	0.2
define	DEF_SHARPHI	1.0
define	DEF_ROUNDLO	-1.0
define	DEF_ROUNDHI	1.0
define	DEF_MAGLO	-MAX_REAL
define	DEF_MAGHI	MAX_REAL
define	DEF_NPIXMIN	5


# define the gaussian sums structure

define  LEN_GAUSS               10

define  GAUSS_SUMG              1
define  GAUSS_SUMGSQ            2
define  GAUSS_PIXELS            3
define  GAUSS_DENOM             4
define  GAUSS_SGOP              5


# miscellaneous constants

define  HWHM_TO_SIGMA           0.8493218
define	RMIN			2.001
