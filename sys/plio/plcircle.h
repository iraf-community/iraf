# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


define	LEN_CIRCLEDES	5
define	C_PL		Memi[$1]	# reference mask
define	C_XCEN		Memr[P2R($1+1)]	# X1 coord of circle
define	C_YCEN		Memr[P2R($1+2)]	# Y1 coord of circle
define	C_RADIUS	Memr[P2R($1+3)]	# X2 coord of circle
define	C_PV		Memi[$1+4]	# pixel value

