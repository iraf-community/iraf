include <math.h>
include "skywcsdef.h"
include "skywcs.h"

# SK_ULTRAN -- Transform the sky coordinates from the input coordinate
# system to the output coordinate system using the units conversions as
# appropriate.

procedure sk_ultran (cooin, cooout, ilng, ilat, olng, olat, npts) 

pointer	cooin		#I pointer to the input coordinate system structure
pointer	cooout		#I pointer to the output coordinate system structure
double	ilng[ARB]	#I the input ra/longitude in radians
double	ilat[ARB]	#I the input dec/latitude in radians
double	olng[ARB]	#O the output ra/longitude in radians
double	olat[ARB]	#O the output dec/latitude in radians
int	npts		#I the number of points to be converted

double	tilng, tilat, tolng, tolat
int	i

begin
	do i = 1, npts {

	    switch (SKY_NLNGUNITS(cooin)) {
	    case SKY_HOURS:
		tilng = DEGTORAD(15.0d0 * ilng[i])
	    case SKY_DEGREES:
		tilng = DEGTORAD(ilng[i])
	    case SKY_RADIANS:
		tilng = ilng[i]
	    default:
		tilng = ilng[i]
	    }
	    switch (SKY_NLATUNITS(cooin)) {
	    case SKY_HOURS:
		tilat = DEGTORAD(15.0d0 * ilat[i])
	    case SKY_DEGREES:
		tilat = DEGTORAD(ilat[i])
	    case SKY_RADIANS:
		tilat = ilat[i]
	    default:
		tilat = ilat[i]
	    }

	    call sk_lltran (cooin, cooout, tilng, tilat, INDEFD, INDEFD,
		0.0d0, 0.0d0, tolng, tolat)

	    switch (SKY_NLNGUNITS(cooout)) {
	    case SKY_HOURS:
		olng[i] = RADTODEG(tolng) / 15.0d0
	    case SKY_DEGREES:
		olng[i] = RADTODEG(tolng)
	    case SKY_RADIANS:
		olng[i] = tolng
	    default:
		olng[i] = tolng
	    }
	    switch (SKY_NLATUNITS(cooout)) {
	    case SKY_HOURS:
		olat[i] = RADTODEG(tolat) / 15.0d0
	    case SKY_DEGREES:
		olat[i] = RADTODEG(tolat)
	    case SKY_RADIANS:
		olat[i] = tolat
	    default:
		olat[i] = tolat
	    }
	}
end


# SK_LLTRAN -- Transform the sky coordinate from the input coordinate
# system to the output coordinate system assuming that all the coordinate
# are in radians.

procedure sk_lltran (cooin, cooout, ilng, ilat, ipmlng, ipmlat, px, rv,
	olng, olat)

pointer	cooin		#I pointer to the input coordinate system structure
pointer	cooout		#I pointer to the output coordinate system structure
double	ilng		#I the input ra/longitude in radians
double	ilat		#I the input dec/latitude in radians
double	ipmlng		#I the input proper motion in ra in radians
double	ipmlat		#I the input proper motion in dec in radians
double	px		#I the input parallax in arcseconds
double	rv		#I the input radial velocity in km / second
double	olng		#O the output ra/longitude in radians
double	olat		#O the output dec/latitude in radians

int	pmflag
double	pmr, pmd
double	sl_epj(), sl_epb()

begin
	# Test for the case where the input coordinate system is the
	# same as the output coordinate system.
	if (SKY_CTYPE(cooin) == SKY_CTYPE(cooout)) {

	    switch (SKY_CTYPE(cooin)) {

	    case CTYPE_EQUATORIAL:
		call sk_equatorial (cooin, cooout, ilng, ilat, ipmlng,
		    ipmlat, px, rv, olng, olat)

	    case CTYPE_ECLIPTIC:
		if (SKY_EPOCH(cooin) == SKY_EPOCH(cooout)) {
		    olng = ilng
		    olat = ilat
		} else {
		    call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
		    call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)
		}

	    default:
		olng = ilng
		olat = ilat
	    }

	    return
	}

	# Compute proper motions ?
	if (! IS_INDEFD(ipmlng) && ! IS_INDEFD(ipmlat))
	    pmflag = YES
	else
	    pmflag = NO

	# Cover the remaining cases.
	switch (SKY_CTYPE(cooin)) {

	# The input system is equatorial.
	case CTYPE_EQUATORIAL:

	    switch (SKY_RADECSYS(cooin)) {

	    case EQTYPE_FK4, EQTYPE_FK4NOE:
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epb (SKY_EPOCH(cooin)), sl_epb (SKY_EPOCH(cooout)),
			olng, olat)
	        } else {
		    olng = ilng
		    olat = ilat
	        }
		if (SKY_RADECSYS(cooin) == EQTYPE_FK4)
		    call sl_suet (olng, olat, SKY_EQUINOX(cooin), olng, olat)
		if (SKY_EQUINOX(cooin) != 1950.0d0)
		    call sl_prcs (1, SKY_EQUINOX(cooin), 1950.0d0, olng, olat) 
		call sl_adet (olng, olat, 1950.0d0, olng, olat)
		if (pmflag == YES)
		    call sl_f45z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		        olng, olat)
		else
		    call sl_f45z (olng, olat, sl_epb (SKY_EPOCH(cooin)),
		        olng, olat)

	    case EQTYPE_FK5:
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epj (SKY_EPOCH(cooin)), sl_epj(SKY_EPOCH(cooout)),
			olng, olat)
	        } else {
	            olng = ilng
	            olat = ilat
		}
		if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 

	    case EQTYPE_ICRS:
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epj (SKY_EPOCH(cooin)), sl_epj(SKY_EPOCH(cooout)),
			olng, olat)
	        } else {
	            olng = ilng
	            olat = ilat
		}
		if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_hf5z (olng, olat, 2000.0d0, olng, olat, pmr, pmd)

	    case EQTYPE_GAPPT:
		call sl_amp (ilng, ilat, SKY_EPOCH(cooin), 2000.0d0, olng, olat)

	    }

	    switch (SKY_CTYPE(cooout)) {

	    # The output coordinate system is ecliptic.
	    case CTYPE_ECLIPTIC:
		call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)

	    # The output coordinate system is galactic.
	    case CTYPE_GALACTIC:
		call sl_eqga (olng, olat, olng, olat)

	    # The output coordinate system is supergalactic.
	    case CTYPE_SUPERGALACTIC:
		call sl_eqga (olng, olat, olng, olat)
		call sl_gasu (olng, olat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	# The input coordinate system is ecliptic.
	case CTYPE_ECLIPTIC:

	    call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
	    switch (SKY_CTYPE(cooout)) {

	    # The output coordinate system is equatorial.
	    case CTYPE_EQUATORIAL:

		switch (SKY_RADECSYS(cooout)) {
		case EQTYPE_FK4, EQTYPE_FK4NOE:
		    call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		    if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
		        call sl_adet (olng, olat, SKY_EQUINOX(cooout),
			    olng, olat)

		case EQTYPE_FK5:
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_ICRS:
		    #call sl_f5hz (olng, olat, sl_epj(SKY_EPOCH(cooin)),
		        #olng, olat)
		    call sl_f5hz (olng, olat, 2000.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_GAPPT:
		    call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0,
			2000.0d0, SKY_EPOCH(cooout), olng, olat)
		}

	    # The output coordinate system is galactic.
	    case CTYPE_GALACTIC:
		call sl_eqga (olng, olat, olng, olat)

	    # The output system is supergalactic.
	    case CTYPE_SUPERGALACTIC:
		call sl_eqga (olng, olat, olng, olat)
		call sl_gasu (olng, olat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	# The input coordinate system is galactic.
	case CTYPE_GALACTIC:

	    switch (SKY_CTYPE(cooout)) {

	    # The output coordinate system is equatorial.
	    case CTYPE_EQUATORIAL:
	        call sl_gaeq (ilng, ilat, olng, olat)

		switch (SKY_RADECSYS(cooout)) {
		case EQTYPE_FK4, EQTYPE_FK4NOE:
		    call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		    if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
		        call sl_adet (olng, olat, SKY_EQUINOX(cooout),
			    olng, olat)

		case EQTYPE_FK5:
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_ICRS:
		    call sl_f5hz (olng, olat, 2000.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_GAPPT:
		    call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0,
			2000.0d0, SKY_EPOCH(cooout), olng, olat)
		}

	    # The output coordinate system is ecliptic.
	    case CTYPE_ECLIPTIC:
		call sl_gaeq (ilng, ilat, olng, olat)
		call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)

	    # The output coordinate system is supergalactic.
	    case CTYPE_SUPERGALACTIC:
		call sl_gasu (ilng, ilat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	# The input coordinates are supergalactic.
	case CTYPE_SUPERGALACTIC:

	    switch (SKY_CTYPE(cooout)) {

	    case CTYPE_EQUATORIAL:
		call sl_suga (ilng, ilat, olng, olat)

		switch (SKY_RADECSYS(cooout)) {

		case EQTYPE_FK4:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_f54z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		    call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)

		case EQTYPE_FK4NOE:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_f54z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_FK5:
		    call sl_gaeq (olng, olat, olng, olat)
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_ICRS:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_f5hz (olng, olat, 2000.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 

		case EQTYPE_GAPPT:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0,
			2000.0d0, SKY_EPOCH(cooout), olng, olat)
		}

	    case CTYPE_ECLIPTIC:
		call sl_suga (ilng, ilat, olng, olat)
		call sl_gaeq (olng, olat, olng, olat)
		call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)

	    case CTYPE_GALACTIC:
		call sl_suga (ilng, ilat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	default:
	    olng = ilng
	    olat = ilat
	}
end


# SK_EQUATORIAL -- Convert / precess equatorial coordinates.

procedure sk_equatorial (cooin, cooout, ilng, ilat, ipmlng, ipmlat,
	px, rv, olng, olat)

pointer	cooin		#I the input coordinate system structure
pointer	cooout		#I the output coordinate system structure
double	ilng		#I the input ra in radians
double	ilat		#I the input dec in radians
double	ipmlng		#I the input proper motion in ra in radians
double	ipmlat		#I the input proper motion in dec in radians
double	px		#I the input parallax in arcseconds
double	rv		#I the input radial valocity in km / second
double	olng		#O the output ra in radians
double	olat		#O the output dec in radians

int	pmflag
double	pmr, pmd
double	sl_epb(), sl_epj()

begin
	# Check to see whether or not conversion / precession is necessary.
	if ((SKY_RADECSYS(cooin) == SKY_RADECSYS(cooout)) &&
	    (SKY_EQUINOX(cooin) == SKY_EQUINOX(cooout)) &&
	    (SKY_EPOCH(cooin) == SKY_EPOCH(cooout))) {
	    olng = ilng
	    olat = ilat
	    return
	}

	# Compute proper motions ?
	if (! IS_INDEFD(ipmlng) && ! IS_INDEFD(ipmlat))
	    pmflag = YES
	else
	    pmflag = NO

	switch (SKY_RADECSYS(cooin)) {

	# The input coordinate system is FK4 with or without the E terms.
	case EQTYPE_FK4, EQTYPE_FK4NOE:

	    if (pmflag == YES) {
		call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		    sl_epb (SKY_EPOCH(cooin)), sl_epb (SKY_EPOCH(cooout)),
		    olng, olat)
	    } else {
		olng = ilng
		olat = ilat
	    }
	    if (SKY_RADECSYS(cooin) == EQTYPE_FK4)
		call sl_suet (olng, olat, SKY_EQUINOX(cooin), olng, olat)
	    if (SKY_EQUINOX(cooin) != 1950.0d0)
	        call sl_prcs (1, SKY_EQUINOX(cooin), 1950.0d0, olng, olat) 
	    call sl_adet (olng, olat, 1950.0d0, olng, olat)
	    if (pmflag == YES)
	        call sl_f45z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		    olng, olat)
	    else
	        call sl_f45z (olng, olat, sl_epb (SKY_EPOCH(cooin)),
		    olng, olat)

	    switch (SKY_RADECSYS(cooout)) {

	    # The output coordinate system is FK4 with and without the E terms.
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
		call sl_f54z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		    olng, olat, pmr, pmd)
	        call sl_suet (olng, olat, 1950.0d0, olng, olat)
		if (SKY_EQUINOX(cooout) != 1950.0d0)
	            call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
		        olng, olat) 
		if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
	            call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)

	    # The output coordinate system is FK5.
	    case EQTYPE_FK5:
		if (SKY_EQUINOX(cooout) != 2000.0d0)
		    call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout), olng, olat) 

	    # The output coordinate system is ICRS (Hipparcos).
	    case EQTYPE_ICRS:
		call sl_f5hz (olng, olat, 2000.0d0, olng, olat)
		if (SKY_EQUINOX(cooout) != 2000.0d0)
		    call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout), olng, olat) 

	    # The output coordinate system is geocentric apparent.
	    case EQTYPE_GAPPT:
		call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0,
		    SKY_EPOCH(cooout), olng, olat)
	    }

	# The input coordinate system is FK5 or geocentric apparent.
	case EQTYPE_FK5, EQTYPE_GAPPT:

	    if (SKY_RADECSYS(cooin) == EQTYPE_FK5) {
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epj (SKY_EPOCH(cooin)), sl_epj (SKY_EPOCH(cooout)),
			    olng, olat)
	        } else {
	            olng = ilng
	            olat = ilat
		}
	    } else
	        call sl_amp (ilng, ilat, SKY_EPOCH(cooin), 2000.0d0, olng, olat)

	    switch (SKY_RADECSYS(cooout)) {

	    # The output coordinate system is FK4 with or without the E terms.
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		    olng, olat, pmr, pmd)
		call sl_suet (olng, olat, 1950.0d0, olng, olat)
		if (SKY_EQUINOX(cooout) != 1950.0d0)
		    call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout), olng, olat) 
		if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
	            call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)

	    # The output coordinate system is FK5.
	    case EQTYPE_FK5:
		if (SKY_EQUINOX(cooin) != SKY_EQUINOX(cooout))
		    call sl_prcs (2, SKY_EQUINOX(cooin), SKY_EQUINOX(cooout),
		        olng, olat) 

	    # The output coordinate system is ICRS.
	    case EQTYPE_ICRS:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_f5hz (olng, olat, sl_epj(SKY_EPOCH(cooin)), olng, olat)
	        if (SKY_EQUINOX(cooout) != 2000.0d0)
		    call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout), olng, olat) 

	    # The output coordinate system is geocentric apparent.
	    case EQTYPE_GAPPT:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0,
		    SKY_EPOCH(cooout), olng, olat)
	    }

	# The input coordinate system is ICRS.
	case EQTYPE_ICRS:

	    if (pmflag == YES) {
		call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		    sl_epj (SKY_EPOCH(cooin)), sl_epj (SKY_EPOCH(cooout)),
		    olng, olat)
	    } else {
	        olng = ilng
	        olat = ilat
	    }

	    switch (SKY_RADECSYS(cooout)) {

	    # The output coordinate system is FK4 with or without the E terms.
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_hf5z (olng, olat, 2000.0d0, olng, olat,
		    pmr, pmd)
		call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)), olng, olat,
		    pmr, pmd)
		call sl_suet (olng, olat, 1950.0d0, olng, olat)
		if (SKY_EQUINOX(cooout) != 1950.0d0)
		    call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout), olng, olat) 
		if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
	            call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)

	    # The output coordinate system is FK5.
	    case EQTYPE_FK5:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_hf5z (olng, olat, sl_epj(SKY_EPOCH(cooout)),
		    olng, olat, pmr, pmd)
	        if (SKY_EQUINOX(cooout) != 2000.0d0)
		    call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout), olng, olat) 

	    # The output coordinate system is ICRS.
	    case EQTYPE_ICRS:
		if (SKY_EQUINOX(cooin) != SKY_EQUINOX(cooout))
		    call sl_prcs (2, SKY_EQUINOX(cooin), SKY_EQUINOX(cooout),
		        olng, olat) 

	    # The output coordinate system is geocentric apparent.
	    case EQTYPE_GAPPT:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_hf5z (olng, olat, sl_epj(SKY_EPOCH(cooout)),
		    olng, olat, pmr, pmd)
		call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0,
		    SKY_EPOCH(cooout), olng, olat)

	    }

	}
end
