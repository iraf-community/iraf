# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GTY package definitions.

define	MAX_TC_NESTING	8		# max nesting of tc=term references

# Mapping function used to map capcodes ("cm", etc.) into unique integers.
define	ENCODE		($1[1]*128+$1[2])

# TTY descriptor structure.  Full termcap entry is the 'caplist' string.
# The caplist is indexed at open time to permit a binary search for
# capabilities at run time.

define	T_MEMINCR	512		# increment if overflow occurs
define	T_OFFCAP	415		# struct offset to caplist field
define	MAX_CAPS	200		# maximum capabilities
define	LEN_DEFTTY	1024		# initial length of tty structure

define	T_LEN		Memi[$1]	# length of tty structure
define	T_OP		Memi[$1+1]	# offset into caplist
define	T_NCAPS		Memi[$1+11]	# number of capabilities
define	T_CAPLEN	Memi[$1+12]	# length of caplist, chars
			# (extra space)
define	T_CAPCODE	Memi[$1+15]	# cap code array: c1*128+c2
define	T_CAPINDEX	Memi[$1+215]	# cap index array
define	T_CAPLIST	Memc[P2C($1+415)]	# termcap entry
