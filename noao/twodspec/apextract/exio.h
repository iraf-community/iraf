# Definitions for image I/O.

define	MAXBUF		1000000		# Maximum buffer for column access

define	LEN_EX		15

define	EX_IM		Memi[$1]	# IMIO pointer
define	EX_DAXIS	Memi[$1+1]	# Image dispersion axis
define	EX_AAXIS	Memi[$1+2]	# Image aperture axis
define	EX_DLEN		Memi[$1+3]	# Image dispersion axis length
define	EX_ALEN		Memi[$1+4]	# Image aperture axis length

define	EX_MAXBUF	Memi[$1+5]	# Maximum buffer size
define	EX_DATA		Memi[$1+6]	# Column data pointer
define	EX_BUF		Memi[$1+7]	# Buffer
define	EX_NCOLS	Memi[$1+8]	# Number of columns in buffer
define	EX_NLINES	Memi[$1+9]	# Number of lines in buffer
define	EX_COL		Memi[$1+10]	# Column
define	EX_COL1		Memi[$1+11]	# First column of buffer
define	EX_COL2		Memi[$1+12]	# Last column of buffer
define	EX_LINE1	Memi[$1+13]	# First line of data
define	EX_LINE2	Memi[$1+14]	# Last line of data
