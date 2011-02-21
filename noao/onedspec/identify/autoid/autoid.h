# AUTOIDENTIFY data structure.

define	AID_SZLINE	99
define	AID_LEN		512

# Algorithm input parameters.
define	AID_REFLIST	Memc[P2C($1)]	  # Reference coordinate list
define	AID_REFSPEC	Memc[P2C($1+50)]  # Reference spectrum
define	AID_CR		Memc[P2C($1+100)] # Coordinate reference value
define	AID_CD		Memc[P2C($1+150)] # Coordinate reference value
define	AID_CP		Memc[P2C($1+200)] # Coordinate reference value
define	AID_CQ		Memc[P2C($1+250)] # Coordinate quad distortion
define	AID_CRS		Memc[P2C($1+300)] # Coordinate reference value
define	AID_CDS		Memc[P2C($1+350)] # Coordinate reference value
define	AID_DEBUG	Memc[P2C($1+400)+ 2-1]  # Debug flags (19 chars)
define	AID_CDDIR	Memi[$1+450]	  # Coordinate direction
define	AID_NTMAX	Memi[$1+451]	  # Maximum number of target lines
define	AID_NRMAX	Memi[$1+452]	  # Maximum number of reference lines
define	AID_ORD		Memi[$1+453]	  # Maximum fitting order
define	AID_MAXNL	Memr[P2R($1+454)] # Maximum non-linearity
define	AID_NB		Memi[$1+455]	  # Number of sub-bins
define	AID_NN		Memi[$1+456]	  # Number of neighbor lines
define	AID_NP		Memi[$1+457]	  # Number of lines in pattern
define	AID_SIG		Memr[P2R($1+458)] # Target line centering sigma
define	AID_NFOUND	Memi[$1+459]	  # Minimum number to be found
define	AID_RMSG	Memr[P2R($1+460)] # Pixel RMS (goal)
define	AID_FMATCHG	Memr[P2R($1+461)] # Frac of unmatched lines (goal)
define	AID_FTMATCHG	Memr[P2R($1+462)] # Frac of unmatched target lines (goal)

define	AID_IDT		Memi[$1+463]	  # Target ID pointer
define	AID_IDR		Memi[$1+464]	  # Reference ID pointer
define	AID_IC1		Memi[$1+465]	  # ICFIT pointer
define	AID_IC2		Memi[$1+466]	  # ICFIT pointer

define	AID_XR		Memi[$1+467]	  # Reference lines (ptr)
define	AID_NR		Memi[$1+468]	  # Number of reference lines
define	AID_XTF		Memi[$1+469]	  # Full target lines sorted by peak
define	AID_NTF		Memi[$1+470]	  # Full number of target lines
define	AID_XT		Memi[$1+471]	  # Target lines to use sorted by pix
define	AID_XTL		Memi[$1+472]	  # Linearized target lines sort by pix
define	AID_NT		Memi[$1+473]	  # Number of target lines to use

define	AID_CDSIGN	Memi[$1+474]	  # Sign of coordinate interval
define	AID_CRVAL	Memd[P2D($1+476)] # Reference coordinate value
define	AID_CDELT	Memd[P2D($1+478)] # Coordinate interval per pixel
define	AID_CRPIX	Memd[P2D($1+480)] # Reference pixel
define	AID_CRQUAD	Memd[P2D($1+482)] # Quadratic distortion
define	AID_CRSEARCH	Memd[P2D($1+484)] # Search radius for ref value
define	AID_CDSEARCH	Memd[P2D($1+486)] # Search radius for coord int
define	AID_CRMIN	Memd[P2D($1+488)] # Min for central coordinate
define	AID_CRMAX	Memd[P2D($1+490)] # Max for central coordinate
define	AID_CDMIN	Memd[P2D($1+492)] # Min for coordinate interval
define	AID_CDMAX	Memd[P2D($1+494)] # Max for coordinate interval

define	AID_MINRATIO	Memr[P2R($1+496)] # Minimum ratio
define	AID_NDMAX	Memi[$1+497]	  # Max number of dispersions to check
define	AID_RMS		Memr[P2R($1+498)] # Pixel RMS (best)
define	AID_FMATCH	Memr[P2R($1+499)] # Fraction of unmatched linelist lines
define	AID_FTMATCH	Memr[P2R($1+500)] # Fraction of unmatched target lines
define	AID_WRMS	Memr[P2R($1+501)] # Weight for RMS
define	AID_WFMATCH	Memr[P2R($1+502)] # Weight for FMATCH
define	AID_WFTMATCH	Memr[P2R($1+503)] # Weight for FTMATCH
define	AID_NBEST	Memi[$1+504]	  # Number of best values < 1 to check
define	AID_BEST	Memr[P2R($1+505)] # Best fit parameter
define	AID_EVS		Memi[$1+506]	  # Evaluate structure

define	AID_SPECR	Memi[$1+507]	  # Reference spectrum (ptr)
define	AID_X1R		Memi[$1+508]	  # First pixel of full ref spectrum
define	AID_X2R		Memi[$1+509]	  # Last pixel of full ref spectrum
define	AID_W1		Memr[P2R($1+510)] # Tentative wavelength of first pixel
define	AID_W2		Memr[P2R($1+511)] # Tentative wavelength of last pixel


# Evaluation structure.
define	AID_EVLEN	8
define	AID_BIN1	Memi[$1]	  # Reference sample bin
define	AID_BIN2	Memi[$1+1]	  # Reference sample bin
define	AID_X		Memi[$1+2]	  # Pixel coordinates
define	AID_Y		Memi[$1+3]	  # Dispersion coordinates
define	AID_N		Memi[$1+4]	  # Number of coordinate pairs
define	AID_A		Memi[$1+5]	  # Trial dispersion start
define	AID_B		Memi[$1+6]	  # Trial dispersion step
define	AID_ND		Memi[$1+7]	  # Number of trial dispersions

# Dispersion direction options.
define	CDDIR	"|sign|increasing|decreasing|unknown|"
define	CDSIGN		1
define	CDINC		2
define	CDDEC		3
define	CDUNKNOWN	4
