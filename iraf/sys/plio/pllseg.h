# PLLSEG.H -- Macros for sequentially reading segments of a line list.
#
#	       pll_init (ll, descriptor)
#      npix = pll_nleft (descriptor)
#      val = pll_getseg (ll, descriptor, npix, value)
#
# pll_init	Initialize descriptor for sequential i/o from the linelist LL.
# pll_nleft	Number of pixels left in the current line segment of constant
#		  value.  Zero is returned at the EOL.
# pll_getseg	Read NPIX pixels from the current segment, advancing to the
#		  next segment automatically when the the current segment is
#		  exhausted.
#
# The descriptor is an integer array, the contents of which are hidden from
# the application using these macros.  This package uses the internal
# procedure PLL_NEXTSEG, which is included in PL package library.

# Range list i/o descriptor.
define	LEN_PLLDES	7
define	ld_nleft	$1[1]
define	ld_value	$1[2]
define	ld_x		$1[3]
define	ld_ip		$1[4]
define	ld_hi		$1[5]
define	ld_next_nleft	$1[6]
define	ld_next_value	$1[7]

# PLL_INIT -- Initialize the linelist descriptor.
define	(pll_init, {			# $1=ll $2=des
	# ld_x($2) = 1
	ld_hi($2) = 1
	if (LL_OLDFORMAT($1))
	    ld_ip($2) = OLL_FIRST
	else
	    ld_ip($2) = LL_FIRST($1)
	ld_next_nleft($2) = 0
	ld_nleft($2) = 0
	call pll_nextseg ($1, $2)
})

# PLL_NLEFT -- Number of pixels left in the current segment.
define	pll_nleft	ld_nleft($1)

# PLL_GETSEG -- Read pixels from the current segment.
define	(pll_getseg, {			# $1=ll $2=des $3=npix $4=value
	$4 = ld_value($2)
	# ld_x($2) = ld_x($2) + $3
	ld_nleft($2) = ld_nleft($2) - $3
	if (ld_nleft($2) <= 0)
	    if (ld_next_nleft($2) > 0) {
		ld_nleft($2) = ld_next_nleft($2)
		ld_value($2) = ld_next_value($2)
		ld_next_nleft($2) = 0
	    } else
		call pll_nextseg ($1, $2)
})
