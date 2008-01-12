# LINEFN.H -- Data structure associated with linefn()

define	LEN_LINSTRUCT	4

define	LIN_ROW		Memi[P2I($1)]	# Cursor row
define	LIN_COL		Memi[P2I($1+1)]	# Cursor column
define	LIN_ICHAR	Memi[P2I($1+2)]	# Index to current character
define	LIN_LAST	Memi[P2I($1+3)]	# Character which caused return
