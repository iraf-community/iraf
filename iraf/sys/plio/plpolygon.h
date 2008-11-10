# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


define	TOL		0.0001		# pixel units
define	swapi		{tempi=$2;$2=$1;$1=tempi}
define	swapl		{templ=$2;$2=$1;$1=templ}
define	swapr		{tempr=$2;$2=$1;$1=tempr}
define	equal		(abs($1-$2)<TOL)

define	LEN_PGONDES	7
define	P_PL		Memp[$1]	# pointer to X vector
define	P_XP		Memp[$1+1]	# pointer to X vector
define	P_YP		Memp[$1+2]	# pointer to Y vector
define	P_OO		Memp[$1+3]	# pointer to previous range list
define	P_OY		Meml[P2L($1+4)]	# y value of previous range list
define	P_NS		Memi[P2I($1+5)]	# number of line segments
define	P_PV		Memi[P2I($1+6)]	# pixel value
