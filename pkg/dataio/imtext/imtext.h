# Definitions used for conversions between text files and IRAF images.
# Both tasks rtextimage and wtextimage include this file.

define	LEN_WT		(2+20+20)

define	IRAFTYPE	Memc[P2C($1)]
define	FORM		Memc[P2C($1+20)]

define	UNSET		0	# Flag for unitialized header values
define	INT_FORM	1	# Text file pixels written as integers
define	FP_FORM		2	# Text file pixels written as floating point
define	CPX_FORM	3	# Text file pixels written as complex

define	COL_VALUE	11	# Starting column for FITS keyword values
define	LEN_CARD	80
define	SZ_STRING	20
define	MAX_LENTEXT 	(2*SZ_LINE)
define	NFITS_LINES	10	
define	NCARDS_FITS_BLK	36
define	LEN_STRING	18
define	LEN_KEYWORD	8
