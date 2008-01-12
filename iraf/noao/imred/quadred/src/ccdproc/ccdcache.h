# Definition for image cache structure.

define	CCD_LENCACHE		6

define	CCD_IM		Memi[P2I($1)]	# IMIO pointer
define	CCD_NACCESS	Memi[P2I($1+1)]	# Number of accesses requested
define	CCD_SZDATA	Memi[P2I($1+2)]	# Size of data in cache in chars
define	CCD_DATA	Memi[P2I($1+3)]	# Pointer to data cache
define	CCD_BUFR	Memi[P2I($1+4)]	# Pointer to real image line
define	CCD_BUFS	Memi[P2I($1+5)]	# Pointer to short image line
