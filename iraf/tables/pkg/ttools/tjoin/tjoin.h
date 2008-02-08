# TJOIN.H -- Constants and data structures used by tjoin

define	ROWNAME		"row"		# string that indicates row number
					# as join column

# Structure used to hold information about tables

define	LEN_TJSTRUCT	7

define	TJ_TAB		Memp[$1]	# Table descriptor
define	TJ_JNUM		Memi[P2I($1+1)]	# Number of join columns
define	TJ_DNUM		Memi[P2I($1+2)]	# Number of data columns
define	TJ_JPTR		Memp[$1+3]	# Pointer to array of join columns
define	TJ_DPTR		Memp[$1+4]	# Pointer to array of data colomns

define	TJ_JCOL		Memp[TJ_JPTR($1)+$2-1]
define	TJ_DCOL		Memp[TJ_DPTR($1)+$2-1]

# Structure used to hold tolerance vector

define	LEN_TOLSTRUCT	2

define	TOL_NUM		Memi[$1]	# Number of tolerance values
define	TOL_PTR		Memp[$1+1]	# Pointer to array of tolerance values

define	TOL_VAL		Memd[TOL_PTR($1)+$2-1]

