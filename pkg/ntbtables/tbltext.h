# These definitions are for text tables.

define	SZ_TEXTBUF	(4096 + SZ_LINE)	# size of input buffer

# These are possible values for the line type as read by tbzlin:
define	DATA_LINE	1	# column data
define	COMMENT_LINE	2	# a comment (blank or beginning with #)
define	KEYWORD_LINE	3	# a keyword (#k keyword = value)
define	COLDEF_LINE	4	# explicit column definition (#c name etc.)

# This is the initial size (and the increment in size) for the array of
# pointers to header keywords.
define	INCR_N_KEYWORDS	128
