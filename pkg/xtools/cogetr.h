# Definitions for the image column procedure.

define	LEN_CO		10

define	CO_IM		Memi[$1]		# IMIO pointer
define	CO_MAXBUF	Memi[$1+1]		# Maximum buffer size
define	CO_DATA		Memi[$1+2]		# Column data pointer
define	CO_BUF		Memi[$1+3]		# Buffer
define	CO_NCOLS	Memi[$1+4]		# Number of columns in buffer
define	CO_NLINES	Memi[$1+5]		# Number of lines in buffer
define	CO_COL1		Memi[$1+6]		# First column of buffer
define	CO_COL2		Memi[$1+7]		# Last column of buffer
define	CO_LINE1	Memi[$1+8]		# First line of data
define	CO_LINE2	Memi[$1+9]		# Last line of data

define	EXTRA	2			# Number of extra lines in buffer
