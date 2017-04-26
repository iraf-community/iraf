# PLRSEG.H -- Macros for sequentially reading segments of a range list.
#
#	       plr_init (rl, descriptor)
#      npix = plr_nleft (descriptor)
#      val = plr_getseg (rl, descriptor, npix, value)
#
# plr_init	Initialize descriptor for sequential i/o from the rangelist RL.
# plr_nleft	Number of pixels left in the current line segment of constant
#		  value.  Zero is returned at the EOL.
# plr_getseg	Read NPIX pixels from the current segment, advancing to the
#		  next segment automatically when the the current segment is
#		  exhausted.
#
# The descriptor is an integer array, the contents of which are hidden from
# the application using these macros.

# Range list i/o descriptor.
define	LEN_PLRDES	4
define	rd_nleft	$1[1]
define	rd_value	$1[2]
define	rd_x		$1[3]
define	rd_rn		$1[4]

# PLR_INIT -- Initialize the rangelist descriptor.
define	(plr_init, {			# $1=rl $2=des
	rd_x($2) = 1
	rd_rn($2) = RL_FIRST
	plr_nextseg ($1, $2)
})

# PLR_NLEFT -- Number of pixels left in the current segment.
define	plr_nleft	rd_nleft($1)

# PLR_GETSEG -- Read pixels from the current segment.
define	(plr_getseg, {			# $1=rl $2=des $3=npix $4=value
	$4 = rd_value($2)
	rd_x($2) = rd_x($2) + $3
	rd_nleft($2) = rd_nleft($2) - $3
	if (rd_nleft($2) <= 0)
	    plr_nextseg ($1, $2)
})
	
# PLR_NEXTSEG -- Set up the next segment (internal routine).
define	(plr_nextseg, {			# $1=rl $2=des
	if (rd_rn($2) <= RL_LEN($1)) {
	    if ($1[1,rd_rn($2)] > rd_x($2)) {
		rd_value($2) = 0
		rd_nleft($2) = $1[1,rd_x($2)] - rd_x($2)
	    } else {
		rd_value($2) = $1[3,rd_rn($2)]
		rd_nleft($2) = $1[2,rd_rn($2)]
		rd_rn($2) = rd_rn($2) + 1
	    }
	} else if (rd_x($2) <= RL_AXLEN($1)) {
	    rd_value($2) = 0
	    rd_nleft($2) = RL_AXLEN($1) - rd_x($2) + 1
	}
})
