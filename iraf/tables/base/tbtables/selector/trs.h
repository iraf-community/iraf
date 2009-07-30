# TRS.H -- Constants used by trs procedures

define	TRS_MAGIC	5526099

define	MAXDEPTH	32
define	MAXSTACK	8

define	SZ_BUFFER	600
define	SZ_INSTR	6
define	SZ_NODE		5
define	SZ_TOKEN	32

define	LEN_TRSBUF	4

define	TRS_IDENT	Memi[P2I($1)]		# Structure identifier
define	TRS_CODE	Memp[$1+1]		# Code buffer
define	TRS_VALUE	Memp[$1+2]		# Value buffer
define	TRS_ROWS	Memp[$1+3]		# Row set

define	OCODE		0
define	OCOLUMN		1
define	OTJUMP		2
define	OFJUMP		3
define	OLOVAL		4
define	OHIVAL		5

define	CODE		Memi[P2I($1+OCODE)]
define	COLUMN		Memp[$1+OCOLUMN]
define	TJUMP		Memi[P2I($1+OTJUMP)]
define	FJUMP		Memi[P2I($1+OFJUMP)]
define	LOVAL		Memp[$1+OLOVAL]
define	HIVAL		Memp[$1+OHIVAL]

define	TREE_OPER	Memi[P2I($1)]		# operation to be performed
define	TREE_INST	Memi[P2I($1+1)]		# index of op in code buffer
define	TREE_LEFT	Memp[$1+2]		# first argument of op
define	TREE_RIGHT	Memp[$1+3]		# second argument of op
define	TREE_UP		Memp[$1+4]		# back link in tree

define	YDONE		1
define	YRANGE		2
define	YAND		3
define	YOR		4
define	YNOT		5
define	YEQN		6
define	YEQS		7
define	YLEN		8
define	YLES		9
define	YINN		10
define	YINS		11
define	YGEN		12
define	YGES		13
define	YMSK		14

define	YLOGICAL	($1 <= YNOT)
