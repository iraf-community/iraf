include	<error.h>
include	<mach.h>
include	<smw.h>
include	"ecidentify.h"

# EC_MAPLL -- Read the line list into memory.

procedure ec_mapll (ec)

pointer	ec		# Echelle pointer

int	fd, nalloc, nlines, open(), fscan(), nscan()
double	value, lastval
pointer	ec_ll
errchk	open, fscan, malloc, realloc

begin
	EC_LL(ec) = NULL

	call xt_stripwhite (Memc[EC_COORDLIST(ec)])
	if (Memc[EC_COORDLIST(ec)] == EOS)
	    return
	iferr (fd = open (Memc[EC_COORDLIST(ec)], READ_ONLY, TEXT_FILE))
	    return

	lastval = -MAX_DOUBLE
	nalloc = 0
	nlines = 0
	while (fscan (fd) != EOF) {
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

	if (nlines == 0)
	    return

	call realloc (ec_ll, nlines + 1, TY_DOUBLE)
	Memd[ec_ll+nlines] = INDEFD
	EC_LL(ec) = ec_ll
end


# EC_UNMAPLL -- Unmap the linelist.

procedure ec_unmapll (ec)

pointer	ec		# Line list pointer

begin
	call mfree (EC_LL(ec), TY_DOUBLE)
end



# EC_MATCH -- Match current feature against a line list.

procedure ec_match (ec, in, out)

pointer	ec			# Echelle pointer
double	in			# Coordinate to be matched
double	out			# Matched coordinate

double	match, delta, deltamin
pointer	ll

begin
	if (EC_LL(ec) == NULL) {
	    out = in
	    return
	}

	match = EC_MATCH(ec)
	deltamin = MAX_REAL

	for (ll=EC_LL(ec); !IS_INDEFD(Memd[ll]); ll = ll + 1) {
	    delta = abs (in - Memd[ll])
	    if (delta < deltamin) {
		deltamin = delta
	        if (deltamin <= match)
		    out = Memd[ll]
	    }
	}
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
