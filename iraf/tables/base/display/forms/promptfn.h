# PROMPTFN.H -- Data structure associated with promptfn()

define	LEN_PRSTRUCT	3

define	PR_START	Memi[P2I($1)]	# Start flag
define	PR_ROW		Memi[P2I($1+1)]	# Cursor row
define	PR_COL		Memi[P2I($1+2)]	# Cursor column
