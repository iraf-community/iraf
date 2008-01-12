# FORMFN.H -- Data structure used by formfn()

define	LEN_FMSTRUCT	10

define	FM_FIELD	Memi[P2I($1)]	# Current field
define	FM_NFIELD	Memi[P2I($1+1)]	# Number of fields on form
define	FM_NPAGE	Memi[P2I($1+2)]	# Number of fields in window
define	FM_CHANGE	Memi[P2I($1+3)]	# Has field been changed ?
define	FM_LENNAM	Memi[P2I($1+4)]	# Length of name field
define	FM_LENVAL	Memi[P2I($1+5)]	# Length of value field
define	FM_NAMARY	Memi[P2I($1+6)]	# Array of field names
define	FM_VALARY	Memi[P2I($1+7)]	# Array of field values
define	FM_TTLPTR	Memi[P2I($1+8)]	# Title array
define	FM_TYPARY	Memi[P2I($1+9)]	# Array of field types

define	FM_NAMPTR	FM_NAMARY($1)+(($2)-1)*(FM_LENNAM($1)+1)
define	FM_VALPTR	FM_VALARY($1)+(($2)-1)*(FM_LENVAL($1)+1)

define	FM_TITLE	Memc[FM_TTLPTR($1)]
define	FM_TYPE		Memi[FM_TYPARY($1)+($2)-1]
