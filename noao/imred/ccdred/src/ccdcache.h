# Definition for image cache structure.

define	CCD_LENCACHE		6

define	CCD_IM		Memi[$1]	# IMIO pointer
define	CCD_NACCESS	Memi[$1+1]	# Number of accesses requested
define	CCD_SZDATA	Memi[$1+2]	# Size of data in cache in chars
define	CCD_DATA	Memi[$1+3]	# Pointer to data cache
define	CCD_BUFR	Memi[$1+4]	# Pointer to real image line
define	CCD_BUFS	Memi[$1+5]	# Pointer to short image line
