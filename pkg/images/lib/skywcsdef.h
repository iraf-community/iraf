# The sky coordinates structure

define	LEN_SKYCOOSTRUCT	(30 + SZ_FNAME + 1)

define	SKY_VXOFF	Memd[P2D($1)]	    # logical ra/longitude offset
define	SKY_VYOFF	Memd[P2D($1+2)]	    # logical dec/tatitude offset
define	SKY_VXSTEP	Memd[P2D($1+4)]	    # logical ra/longitude stepsize
define	SKY_VYSTEP	Memd[P2D($1+6)]	    # logical dec/latitude stepsize
define	SKY_EQUINOX	Memd[P2D($1+8)]	    # equinox of ra/dec system (B or J)
define	SKY_EPOCH	Memd[P2D($1+10)]    # epoch of observation (MJD)
define	SKY_CTYPE	Memi[$1+12]	    # celestial coordinate system
define	SKY_RADECSYS	Memi[$1+13]	    # ra/dec system, e.g. FK4
define	SKY_WTYPE	Memi[$1+14]	    # sky projection system
define	SKY_PLNGAX	Memi[$1+15]	    # physical ra/longitude axis
define	SKY_PLATAX	Memi[$1+16]	    # physical dec/latitude axis
define	SKY_XLAX	Memi[$1+17]	    # logical ra/longitude axis
define	SKY_YLAX	Memi[$1+18]	    # latitude dec/latitude axis
define	SKY_PTYPE	Memi[$1+19]	    # iraf wcs system
define	SKY_NLNGAX	Memi[$1+20]	    # length of ra/longitude axis
define	SKY_NLATAX	Memi[$1+21]	    # length of dec/latitude axis
define	SKY_NLNGUNITS	Memi[$1+22]	    # the native ra/longitude units
define	SKY_NLATUNITS	Memi[$1+23]	    # the native dec/latitude units
define	SKY_STATUS	Memi[$1+24]	    # the status (OK or ERR)
define	SKY_COOSYSTEM	Memc[P2C($1+25)]    # the coordinate system

