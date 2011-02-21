# EVEXPR.H -- Expression evaluation definitions; operand structure.

define	LEN_OPERAND	3			# size of operand structure
define	YYOPLEN		LEN_OPERAND		# for the parser
define	NTYPES		4			# number of operand datatypes

define	O_TYPE		Memi[($1)]		# operand type
define	O_LEN		Memi[($1)+1]		# operand length, if array
define	O_VALB		Memb[($1)+2]		# bool value (stored as int)
define	O_VALC		Memc[Memi[($1)+2]]	# string val (in string buffer)
define	O_VALI		Memi[($1)+2]		# int value
define	O_VALP		Memi[($1)+2]		# pointer value (same as int)
define	O_VALR		Memr[P2R(($1)+2)]	# real value
