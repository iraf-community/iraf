# TCS.H --Descriptor for a table column array selector

define	MAXDIM		7		  # max dimensions in table array

define	TCS_COLUMN	Memi[$1]	  # table column pointer
define	TCS_DIMEN	Memi[$1+1]	  # dimensionality of array,
					  # zero for scalars
define	TCS_FIRST	Memi[3*($2)+$1-1] # first value in array
define	TCS_LAST	Memi[3*($2)+$1]	  # last value in array
define	TCS_INC		Memi[3*($2)+$1+1] # increment between values

define  TCS_LENGTH	(3*($1)+2)
