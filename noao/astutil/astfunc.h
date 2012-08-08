# Common for AST_FUNC data.

define	LEN_AST		4
define	AST_STP		Memi[$1]	# Symbol table
define	AST_TFD		Memi[$1+1]	# Text file descriptor
define	AST_TBL		Memi[$1+2]	# Table descriptor
define	AST_IM		Memi[$1+3]	# IMIO pointer
