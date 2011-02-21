
# LEXOPER.H -- Operators and identifiers used by the lexical analyzer

# The value of the operator type is also its priority

define	ENDOPR		1
define	SEPOPR		2
define	IMPOPR		3
define	OROPR		4
define	ANDOPR		5
define	EQOPR		6
define	IDOPR		7

# Pseudo-identifiers placed on the id stack

define	NAME		 1	# Any identifier
define	NO_IDENT	 0	# No identifier on stack
define	PHRASE		-1	# idents joined by equals or ands
define	CLAUSE		-2	# idents joined by equals, ands, or ors

# Rule base data structure

define	RB_LENGTH	(SZ_LINE / SZ_INT32 + 5)

define	RB_FILE		Memi[$1]
define  RB_NLINE        Memi[$1+1]
define  RB_INDEX        Memi[$1+2]
define	RB_LINE		Memc[P2C($1+3)]
define  RB_CHARPTR      (P2C($1+3) + RB_INDEX($1) - 1)
