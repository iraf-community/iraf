define	LEN_CARD	  80
define  SZ_EXTTYPE	  20
define	LEN_CARDNL        81
define  SZ_KEYWORD         8
define  FITS_STARTVALUE   10
define  FITS_ENDVALUE     30
define  FITS_BLKSZ_CHAR  1440		# Number of chars per FITS block
define  FITS_BLKSZ_NL    2916		# 36*81
define  FITS_BLOCK_BYTES 2880
define  MEF_SZVALSTR	 68
define  FITS_ORIGIN  "NOAO-IRAF FITS MEF utility Sep99"

define	LEN_MEF		 271

define	MEF_FD           Memi[$1] 		# File descriptor
define	MEF_HOFF         Memi[$1+2] 		# Header offset in chars
define	MEF_ACMODE       Memi[$1+3] 		# Access mode
define	MEF_ENUMBER      Memi[$1+4]		# Absolute extension number
define	MEF_EXTVER       Memi[$1+5]		# Extension version
define	MEF_CGROUP       Memi[$1+6]		# Current group read
define	MEF_HFLAG        Memi[$1+7] 		# Header update flag
define	MEF_HSIZE        Memi[$1+8] 		# Header size
define	MEF_HDRP         Memi[$1+9] 		# Header area pointer
define  MEF_POFF         Memi[$1+10] 		# Offset to pixel area (chars)
define	MEF_NDIM         Memi[$1+11] 		# Unit dimensionality
define	MEF_NAXIS        Memi[$1+$2+12-1] 	# Upto 7 axis
define	MEF_BITPIX     	 Memi[$1+18] 		# Unit datatype 
define	MEF_DATATYPE     Memi[$1+19] 		# Unit datatype 
define	MEF_SKDATA       Memi[$1+20] 		# Has data been skipped?
define	MEF_PCOUNT       Memi[$1+21] 		# Has data been skipped?
define	MEF_KEEPXT       Memi[$1+22] 		# Has data been skipped?
define	MEF_EXTTYPE      Memc[P2C($1+23)] 	# Extension type
define	MEF_FNAME        Memc[P2C($1+63)] 	# Filename
define	MEF_OBJECT       Memc[P2C($1+191)] 	# Object
define	MEF_EXTNAME      Memc[P2C($1+231)]	# Extension name

define	NEW_UNIT	NEW_FILE

define	SIMPLE	1
define  NAXIS	2 
define  NAXISN	3
define  EXTNAME	4
define  EXTVER  5
define  END	6
define  BITPIX	7
define  EXTEND	8
define  OBJECT	9
define  PCOUNT  10
define  GCOUNT  11
define  INHERIT 12
define  FILENAME 13
define  XTENSION 14
