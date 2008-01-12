# Common for AST_FUNC data.

define	LEN_AST		4
define	AST_STP		Memi[P2I($1)]	# Symbol table
define	AST_TFD		Memi[P2I($1+1)]	# Text file descriptor
define	AST_TBL		Memi[P2I($1+2)]	# Table descriptor
define	AST_IM		Memi[P2I($1+3)]	# IMIO pointer
