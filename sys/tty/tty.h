# TTY package definitions.  Requires <chars.h>.

define	TABSIZE		8
define	MAX_TC_NESTING	5		# max nesting of tc=term references

# Include here all special control characters (mapped by driver) which should
# not be used as binary cursor coordinates.  The list must be terminated by
# an EOS; do not count the EOS in NDCHARS.  Note that NUL cannot be included
# in the list because it is the same as EOS.

define	NDCHARS		3
define	DRIVER_CHARS	EOT,HT,LF,EOS

# Mapping function used to map capcodes ("cm", etc.) into unique integers.
define	ENCODE		($1[1]*128+$1[2])

# Types of standout modes defined for terminals.

define	SOSE		1		# use so,se or us,ue capabilities
define	BSOS		2		# backspace and overstrike with _
define	CROS		3		# CR and overstrike with full line
define	TOUP		4		# map standout chars to upper case

# TTY descriptor structure.  Full termcap entry is the 'caplist' string.
# The caplist is indexed at open time to permit a binary search for
# capabilities at run time.

define	T_MEMINCR	512		# increment if overflow occurs
define	T_OFFCAP	215		# struct offset to caplist field
define	MAX_CAPS	100		# maximum capabilities
define	MAX_COORDS	7		# maximum coords for ttysubi
define	SZ_CTRLSTR	50		# buffer size for control strings
define	LEN_DEFTTY	(256+1024)	# initial length of tty structure

define	T_LEN		Memi[$1]	# length of tty structure
define	T_OP		Memi[$1+1]	# offset into caplist
define	T_PADCHAR	Memi[$1+2]	# pad character for delays
define	T_TABCHAR	Memi[$1+3]	# tab character, if HW tabs ok
define	T_BSOK		Memi[$1+4]	# terminal backspaces with BS
define	T_HTOK		Memi[$1+5]	# term has HT (horiz tab) in hardware
define	T_AM		Memi[$1+6]	# term has automargin advance
define	T_SOTYPE	Memi[$1+7]	# type of standout mode (ttyputline)
define	T_BAUD		Memi[$1+8]	# baud rate for delays
define	T_NLINES	Memi[$1+9]	# nlines on terminal at open
define	T_NCOLS		Memi[$1+10]	# ncols on terminal at open
define	T_NCAPS		Memi[$1+11]	# number of capabilities
define	T_CAPLEN	Memi[$1+12]	# length of caplist, chars
			# (extra space)
define	T_CAPCODE	Memi[$1+15]	# cap code array: c1*128+c2
define	T_CAPINDEX	Memi[$1+115]	# cap index array
define	T_CAPLIST	Memc[($1+215-1)*SZ_STRUCT+1]	# termcap entry
