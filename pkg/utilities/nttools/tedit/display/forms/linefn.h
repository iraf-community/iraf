# LINEFN.H -- Data structure associated with linefn()

define	LEN_LINSTRUCT	4

define	LIN_ROW		Memi[$1]	# Cursor row
define	LIN_COL		Memi[$1+1]	# Cursor column
define	LIN_ICHAR	Memi[$1+2]	# Index to current character
define	LIN_LAST	Memi[$1+3]	# Character which caused return
