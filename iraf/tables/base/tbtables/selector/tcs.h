# TCS.H --Descriptor for a table column array selector

define	MAXDIM		7		  # max dimensions in table array

define	TCS_COLUMN	Memp[$1]	  # table column pointer
define	TCS_DIMEN	Memi[P2I($1+1)]	  # dimensionality of array,
					  # zero for scalars
define	TCS_FIRST	Meml[P2L($1+2 + 3*(($2)-1))]	# first value in array
define	TCS_LAST	Meml[P2L($1+3 + 3*(($2)-1))]	# last value in array
define	TCS_INC		Meml[P2L($1+4 + 3*(($2)-1))]	# increment between values

define  TCS_LENGTH	(2+3*($1))
