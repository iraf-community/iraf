include	<error.h>
include	<mach.h>
include	"identify.h"

# ID_MAPLL -- Read the line list into memory.

procedure id_mapll (id_ll, coordlist)

pointer	id_ll		# Line list pointer
char	coordlist[ARB]	# Line list file

int	fd, nalloc, nlines

int	open(), fscan(), nscan()
double	value

errchk	open, fscan, malloc, realloc

begin
	id_ll = NULL

	iferr (fd = open (coordlist, READ_ONLY, TEXT_FILE))
	    return

	nalloc = 0
	nlines = 0
	while (fscan (fd) != EOF) {
	    call gargd (value)
	    if (nscan() != 1)
		next

	    if (nalloc == 0) {
		nalloc = 100
		call malloc (id_ll, nalloc, TY_DOUBLE)
	    } else if (nlines == nalloc) {
		nalloc = nalloc + 100
		call realloc (id_ll, nalloc, TY_DOUBLE)
	    }

	    Memd[id_ll+nlines] = value
	    nlines = nlines + 1
	}
	call close (fd)

	if (nlines == 0)
	    return

	call realloc (id_ll, nlines + 1, TY_DOUBLE)
	Memd[id_ll+nlines] = INDEFD
end


# ID_UNMAPLL -- Unmap the linelist.

procedure id_unmapll (id_ll)

pointer	id_ll		# Line list pointer

begin
	if (id_ll == NULL)
	    return
	call mfree (id_ll, TY_DOUBLE)
end



# ID_MATCH -- Match current feature against a line list.
#
# This is extremely inefficient.  It can be greatly improved.

procedure id_match (id_ll, in, out, diff)

pointer	id_ll			# Line list pointer
double	in			# Coordinate to be matched
double	out			# Matched coordinate
real	diff			# Maximum difference

double	delta, deltamin
pointer	ll

begin
	if (id_ll == NULL) {
	    out = in
	    return
	}

	deltamin = MAX_REAL

	for (ll=id_ll; !IS_INDEFD(Memd[ll]); ll = ll + 1) {
	    delta = abs (in - Memd[ll])
	    if (delta < deltamin) {
		deltamin = delta
	        if (deltamin <= diff)
		    out = Memd[ll]
	    }
	}
end

# ID_LINELIST -- Add features from a line list.

procedure id_linelist (id, id_ll)

pointer	id			# ID pointer
pointer	id_ll			# Line list pointer

int	i, nfound, nextpix, cursave
double	pix, fit, fit1, fit2, user, peak, minval, diff
pointer	sp, pixes, fits, users, ll

double	id_center(), fit_to_pix(), id_fitpt(), id_peak()

begin
	if (id_ll == NULL)
	    return

	call smark (sp)
	call salloc (pixes, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (fits, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (users, ID_MAXFEATURES(id), TY_DOUBLE)

	nfound = 0
	minval = MAX_REAL

	fit1 = min (FITDATA(id,1), FITDATA(id,ID_NPTS(id)))
	fit2 = max (FITDATA(id,1), FITDATA(id,ID_NPTS(id)))
	for (ll=id_ll; !IS_INDEFD(Memd[ll]); ll = ll + 1) {
	    user = Memd[ll]
	    if (user < fit1)
		next
	    if (user > fit2)
		break
	
	    pix = id_center (id, fit_to_pix (id, Memd[ll]), ID_FWIDTH(id),
		ID_FTYPE(id))
	    if (!IS_INDEFD(pix)) {
		fit = id_fitpt (id, pix)
		diff = abs (fit - Memd[ll])
		if (diff > ID_MATCH(id))
		    next
			
		peak = abs (id_peak (id, pix))
		if (nfound < ID_MAXFEATURES(id)) {
		    nfound = nfound + 1
		    if (peak < minval) {
		    	nextpix = nfound
			minval = peak
		    }
		    Memd[pixes+nfound-1] = pix
		    Memd[fits+nfound-1] = id_fitpt (id, pix)
		    Memd[users+nfound-1] = Memd[ll]
		} else if (peak > minval) {
		    Memd[pixes+nextpix-1] = pix
		    Memd[fits+nextpix-1] = id_fitpt (id, pix)
		    Memd[users+nextpix-1] = Memd[ll]

		    minval = MAX_REAL
		    do i = 1, nfound {
			pix = Memd[pixes+i-1]
			peak = abs (id_peak (id, pix))
			if (peak < minval) {
			    nextpix = i
			    minval = peak
			}
		    }
		}
	    }
	}

	do i = 1, nfound {
	    pix = Memd[pixes+i-1]
	    fit = Memd[fits+i-1]
	    user = Memd[users+i-1]
	    call id_newfeature (id, pix, fit, user, 1.0D0, ID_FWIDTH(id),
		ID_FTYPE(id))
	    if (i == 1)
		cursave = ID_CURRENT(id)
	}
	ID_CURRENT(id) = cursave

	call sfree (sp)
end
