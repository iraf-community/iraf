# EVVEXPR.H -- Expression evaluation definitions; operand structure.

define	LEN_OPERAND	6			# size of operand structure
define	NTYPES		7			# bcsilrd, bool stored as int
define	YYOPLEN		LEN_OPERAND		# for the parser

define	O_TYPE		Memi[($1)]		# operand type
define	O_LEN		Memi[($1)+1]		# operand length, if array
define	O_FLAGS		Memi[($1)+2]		# flag bits
			# align
define	O_VALC		Memc[Memi[($1)+4]]	# string val (in string buffer)
define	O_VALS		Mems[P2S(($1)+4)]	# short int value
define	O_VALI		Memi[($1)+4]		# bool or int value
define	O_VALL		Meml[P2L(($1)+4)]	# long int value
define	O_VALR		Memr[P2R(($1)+4)]	# real value
define	O_VALD		Memd[P2D(($1)+4)]	# double value
define	O_VALP		Memi[($1)+4]		# pointer

# Operand flags.
define	O_FREEVAL	000001			# free data vector
define	O_FREEOP	000002			# free operand struct

# evvexpr flags.
define	EV_RNGCHK	000001			# divzero etc.
