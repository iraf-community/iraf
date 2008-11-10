# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	LEN_BOXDES	6
define	B_PL		Memp[$1]	# reference mask
define	B_X1		Meml[P2L($1+1)]	# X1 coord of box
define	B_Y1		Meml[P2L($1+2)]	# Y1 coord of box
define	B_X2		Meml[P2L($1+3)]	# X2 coord of box
define	B_Y2		Meml[P2L($1+4)]	# Y2 coord of box
define	B_PV		Meml[P2L($1+5)]	# pixel value

