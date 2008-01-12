# PASTE.H -- Tedit paste table descriptor

define	TED_PSTLEN	2		# paste descriptor length

define	TED_PSTPTR	Memi[P2I($1)]	# paste table pointer
define	TED_PSTROWS	Memi[P2I($1+1)]	# number of rows in paste table
