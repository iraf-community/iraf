include	<error.h>
include	<mach.h>
include	<smw.h>
include	<units.h>
include	"ecidentify.h"

# EC_MAPLL -- Read the line list into memory.

procedure ec_mapll (ec)

pointer	ec		# Echelle pointer

int	fd, nalloc, nlines, open(), fscan(), nscan()
double	value, lastval
pointer	ec_ll
pointer	sp, str, units, un_open()
bool	streq()
errchk	open, fscan, malloc, realloc, un_open

begin
	EC_LL(ec) = NULL

	call xt_stripwhite (Memc[EC_COORDLIST(ec)])
	if (Memc[EC_COORDLIST(ec)] == EOS)
	    return
	iferr (fd = open (Memc[EC_COORDLIST(ec)], READ_ONLY, TEXT_FILE))
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)
	call strcpy ("Angstroms", Memc[units], SZ_LINE)

	lastval = -MAX_DOUBLE
	nalloc = 0
	nlines = 0
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan() != 1)
		next
	    if (Memc[str] == '#') {
		call gargwrd (Memc[str], SZ_LINE)
		call strlwr (Memc[str])
		if (streq (Memc[str], "units")) {
		    call gargstr (Memc[units], SZ_LINE)
		    call xt_stripwhite (Memc[units])
		}
		next
	    }
	    call reset_scan ()

	    call gargd (value)
	    if (nscan() != 1)
		next

	    if (nalloc == 0) {
		nalloc = 100
		call malloc (ec_ll, nalloc, TY_DOUBLE)
	    } else if (nlines == nalloc) {
		nalloc = nalloc + 100
		call realloc (ec_ll, nalloc, TY_DOUBLE)
	    }

	    if (value < lastval) {
		call close (fd)
		call mfree (ec_ll, TY_DOUBLE)
		call error (0, "Line list not sorted in increasing order")
	    }

	    Memd[ec_ll+nlines] = value
	    nlines = nlines + 1
	}
	call close (fd)

	if (nlines > 0) {
	    call realloc (ec_ll, nlines + 1, TY_DOUBLE)
	    Memd[ec_ll+nlines] = INDEFD
	    EC_LL(ec) = ec_ll

	    if (EC_UN(ec) == NULL && Memc[units] != EOS)
		EC_UN(ec) = un_open (Memc[units])
	    call ec_unitsll (ec, Memc[units])
	}

	call sfree (sp)
end


# EC_UNMAPLL -- Unmap the linelist.

procedure ec_unmapll (ec)

pointer	ec		# Line list pointer

begin
	call mfree (EC_LL(ec), TY_DOUBLE)
end


# EC_UNITSLL -- Change the line list units from the input units to the
# units given by EC_UN.  This may involve reversing the order of the list.

procedure ec_unitsll (ec, units)

pointer	ec			# Identify structure
char	units[ARB]		# Input units

int	i, nll
double	value
pointer	un, ll, llend, un_open()
bool	un_compare()
errchk	un_open

begin
	if (EC_LL(ec) == NULL)
	    return
	if (IS_INDEFD(Memd[EC_LL(ec)]))
	    return
	if (units[1] == EOS || EC_UN(ec) == NULL)
	    return
	if (UN_CLASS(EC_UN(ec)) == UN_UNKNOWN)
	    return

	un = un_open (units)
	if (un_compare (un, EC_UN(ec))) {
	    call un_close (un)
	    return
	}

	ll = EC_LL(ec)
	do i = 0, ARB
	    if (IS_INDEFD(Memd[ll+i])) {
		nll = i
		break
	    }
	call un_ctrand (un, EC_UN(ec), Memd[ll], Memd[ll], nll)
	call un_close (un)

	if (Memd[ll] > Memd[ll+nll-1]) {
	    llend = ll + nll - 1
	    do i = 0, nll / 2 - 1 {
		value = Memd[ll+i]
		Memd[ll+i] = Memd[llend-i]
		Memd[llend-i] = value
	    }
	}
end



# EC_MATCH -- Match current feature against a line list.

procedure ec_match (ec, in, out)

pointer	ec			# Echelle pointer
double	in			# Coordinate to be matched
double	out			# Matched coordinate

double	match, alpha, delta, delta1, delta2, out1
pointer	ll

begin
	if (EC_LL(ec) == NULL) {
	    out = in
	    return
	}

	match = EC_MATCH(ec)
	alpha = 1.25
	delta1 = MAX_REAL

	# Find nearest match.
	for (ll=EC_LL(ec); !IS_INDEFD(Memd[ll]); ll = ll + 1) {
	    delta = abs (in - Memd[ll])
	    if (delta < delta1) {
	        delta2 = delta1
		delta1 = delta
	        if (delta1 <= match)
		    out1 = Memd[ll]
	    }
	}

	# Only return match if no other candidate is also possible.
	if (delta1 > match)
	    return
	if (delta2 < alpha * delta1)
	    return

	out = out1
end

# EC_LINELIST -- Add features from a line list.

procedure ec_linelist (ec)

pointer	ec			# Echelle pointer

int	i, line, ap, nfound, nextpix
double	pix, fit, user, peak, minval, match, fit1, fit2
pointer	sp, aps, pixes, fits, users, peaks, ll

double	ec_center(), ec_fittopix(), ec_fitpt(), ec_peak()

begin
	if (EC_LL(ec) == NULL)
	    return

	call smark (sp)
	call salloc (aps, EC_MAXFEATURES(ec), TY_INT)
	call salloc (pixes, EC_MAXFEATURES(ec), TY_DOUBLE)
	call salloc (fits, EC_MAXFEATURES(ec), TY_DOUBLE)
	call salloc (users, EC_MAXFEATURES(ec), TY_DOUBLE)
	call salloc (peaks, EC_MAXFEATURES(ec), TY_DOUBLE)

	nfound = 0
	minval = MAX_REAL

	do line = 1, EC_NLINES(ec) {
	    call ec_gline (ec, line)
	    ap = APS(ec,line)
	    fit1 = min (FITDATA(ec,1), FITDATA(ec,EC_NPTS(ec)))
	    fit2 = max (FITDATA(ec,1), FITDATA(ec,EC_NPTS(ec)))
	    for (ll=EC_LL(ec); !IS_INDEFD(Memd[ll]); ll = ll + 1) {
	        user = Memd[ll]
	        if (user < fit1)
		    next
	        if (user > fit2)
		    break

	        pix = ec_center (ec, ec_fittopix (ec, user), EC_FWIDTH(ec),
		    EC_FTYPE(ec))
	        if (!IS_INDEFD(pix)) {
		    fit = ec_fitpt (ec, ap, pix)
		    match = abs (fit - user)
		    if (match > EC_MATCH(ec))
		        next
			
		    peak = abs (ec_peak (ec, pix))
		    if (nfound < EC_MAXFEATURES(ec)) {
		        nfound = nfound + 1
		        if (peak < minval) {
		    	    nextpix = nfound
			    minval = peak
		        }
		        Memi[aps+nfound-1] = ap
		        Memd[pixes+nfound-1] = pix
		        Memd[fits+nfound-1] = fit
		        Memd[users+nfound-1] = user
		        Memd[peaks+nfound-1] = peak
		    } else if (peak > minval) {
		        Memi[aps+nextpix-1] = ap
		        Memd[pixes+nextpix-1] = pix
		        Memd[fits+nextpix-1] = fit
		        Memd[users+nextpix-1] = user
		        Memd[peaks+nextpix-1] = peak

		        minval = MAX_REAL
		        do i = 1, nfound {
			    peak = Memd[peaks+i-1]
			    if (peak < minval) {
			        nextpix = i
			        minval = peak
			    }
		        }
		    }
	        }
	    }
	}
	call ec_gline (ec, EC_LINE(ec))

	do i = 1, nfound {
	    ap = Memi[aps+i-1]
	    pix = Memd[pixes+i-1]
	    fit = Memd[fits+i-1]
	    user = Memd[users+i-1]
	    call ec_newfeature (ec, ap, pix, fit, user, EC_FWIDTH(ec),
		EC_FTYPE(ec))
	}

	call sfree (sp)
end
