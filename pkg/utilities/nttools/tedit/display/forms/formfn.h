# FORMFN.H -- Data structure used by formfn()

define	LEN_FMSTRUCT	10

define	FM_FIELD	Memi[$1]	# Current field
define	FM_NFIELD	Memi[$1+1]	# Number of fields on form
define	FM_NPAGE	Memi[$1+2]	# Number of fields in window
define	FM_CHANGE	Memi[$1+3]	# Has field been changed ?
define	FM_LENNAM	Memi[$1+4]	# Length of name field
define	FM_LENVAL	Memi[$1+5]	# Length of value field
define	FM_NAMARY	Memi[$1+6]	# Array of field names
define	FM_VALARY	Memi[$1+7]	# Array of field values
define	FM_TTLPTR	Memi[$1+8]	# Title array
define	FM_TYPARY	Memi[$1+9]	# Array of field types

define	FM_NAMPTR	FM_NAMARY($1)+(($2)-1)*(FM_LENNAM($1)+1)
define	FM_VALPTR	FM_VALARY($1)+(($2)-1)*(FM_LENVAL($1)+1)

define	FM_TITLE	Memc[FM_TTLPTR($1)]
define	FM_TYPE		Memi[FM_TYPARY($1)+($2)-1]
