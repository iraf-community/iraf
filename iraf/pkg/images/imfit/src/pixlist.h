# PIXEL LIST descriptor structure

define	LEN_PLSTRUCT	10

define	PRL_NCOLS		Memi[P2I($1)]	# number of columns
define	PRL_NLINES		Memi[P2I($1+1)]	# number of lines
define	PRL_LINES		Memi[P2I($1+2)]	# pointer to the line offsets
define	PRL_LIST		Memi[P2I($1+3)]	# pointer to list of ranges
define	PRL_SZLIST		Memi[P2I($1+4)]	# size of list in INTS
define	PRL_LP			Memi[P2I($1+5)]	# offset to next space in list

