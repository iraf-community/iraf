include	<error.h>
include	<mach.h>
include	"identify.h"

# ID_MAPLL -- Read the line list into memory.

procedure id_mapll (id)

pointer	id		# Identify structure

int	fd, nalloc, nlines
pointer	ll1, ll2, str

int	open(), fscan(), nscan(), nowhite()
double	value

errchk	open, fscan, malloc, realloc

begin
	ID_LL(id) = NULL

	if (nowhite (Memc[ID_COORDLIST(id)],
	    Memc[ID_COORDLIST(id)], SZ_FNAME) == 0)
	    return
	iferr (fd = open (Memc[ID_COORDLIST(id)], READ_ONLY, TEXT_FILE)) {
	    call erract (EA_WARN)
	    return
	}

	call malloc (str, SZ_LINE, TY_CHAR)
	nalloc = 0
	nlines = 0
	while (fscan (fd) != EOF) {
	    call gargd (value)
	    if (nscan() != 1)
		next

	    if (nalloc == 0) {
		nalloc = 100
		call malloc (ll1, nalloc, TY_DOUBLE)
		call calloc (ll2, nalloc, TY_POINTER)
	    } else if (nlines == nalloc) {
		nalloc = nalloc + 100
		call realloc (ll1, nalloc, TY_DOUBLE)
		call realloc (ll2, nalloc, TY_POINTER)
		call aclri (Memi[ll2+nalloc-100], 100)
	    }

	    Memd[ll1+nlines] = value
	    call gargstr (Memc[str], SZ_LINE)
	    call id_label (Memc[str], Memi[ll2+nlines])

	    nlines = nlines + 1
	}
	call mfree (str, TY_CHAR)
	call close (fd)

	if (nlines == 0)
	    return

	call realloc (ll1, nlines + 1, TY_DOUBLE)
	call realloc (ll2, nlines + 1, TY_POINTER)
	Memd[ll1+nlines] = INDEFD
	call malloc (ID_LL(id), 2, TY_POINTER)
	Memi[ID_LL(id)] = ll1
	Memi[ID_LL(id)+1] = ll2
end


# ID_UNMAPLL -- Unmap the linelist.

procedure id_unmapll (id)

pointer	id		# Identify structure

pointer	ll1, ll2

begin
	if (ID_LL(id) == NULL)
	    return

	ll1 = Memi[ID_LL(id)]
	ll2 = Memi[ID_LL(id)+1]
	while (!IS_INDEFD(Memd[ll1])) {
	    call mfree (Memi[ll2], TY_CHAR)
	    ll1 = ll1 + 1
	    ll2 = ll2 + 1
	}

	call mfree (Memi[ID_LL(id)], TY_DOUBLE)
	call mfree (Memi[ID_LL(id)+1], TY_POINTER)
	call mfree (ID_LL(id), TY_POINTER)
end



# ID_MATCH -- Match current feature against a line list.
#
# This is extremely inefficient.  It can be greatly improved.

procedure id_match (id, in, out, label, diff)

pointer	id			# Identify structure
double	in			# Coordinate to be matched
double	out			# Matched coordinate
pointer	label			# Pointer to label
real	diff			# Maximum difference

double	zin, delta, deltamin, id_zshiftd()
pointer	ll1, ll2, tmp
int	strlen()

begin
	if (ID_LL(id) == NULL) {
	    out = id_zshiftd (id, in, 0)
	    label = NULL
	    return
	}

	zin = id_zshiftd (id, in, 0)
	deltamin = MAX_REAL

	ll1 = Memi[ID_LL(id)]
	ll2 = Memi[ID_LL(id)+1]
	while (!IS_INDEFD(Memd[ll1])) {
	    delta = abs (zin - Memd[ll1])
	    if (delta < deltamin) {
		deltamin = delta
	        if (deltamin <= diff) {
		    out = Memd[ll1]
		    label = Memi[ll2]
		}
	    }
	    ll1 = ll1 + 1
	    ll2 = ll2 + 1
	}

	if (label != NULL) {
	    call malloc (tmp, strlen (Memc[label]), TY_CHAR)
	    call strcpy (Memc[label], Memc[tmp], ARB)
	    label = tmp
	}
end

# ID_LINELIST -- Add features from a line list.

procedure id_linelist (id)

pointer	id			# Identify structure

int	i, nfound, nextpix, lastpix, cursave
double	pix, fit, fit1, fit2, user, peak, minval, diff, diff1
pointer	sp, pixes, fits, users, labels, ll1, ll2, label

double	id_center(), fit_to_pix(), id_fitpt(), id_peak(), id_zshiftd()

begin
	if (ID_LL(id) == NULL)
	    return

	call smark (sp)
	call salloc (pixes, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (fits, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (users, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (labels, ID_MAXFEATURES(id), TY_POINTER)

	nfound = 0
	lastpix = 0
	minval = MAX_REAL

	fit1 = min (FITDATA(id,1), FITDATA(id,ID_NPTS(id)))
	fit2 = max (FITDATA(id,1), FITDATA(id,ID_NPTS(id)))
	ll1 = Memi[ID_LL(id)]
	ll2 = Memi[ID_LL(id)+1]
	while (!IS_INDEFD(Memd[ll1])) {
	    user = id_zshiftd (id, Memd[ll1], 1)
	    label = Memi[ll2]
	    ll1 = ll1 + 1
	    ll2 = ll2 + 1
	    if (user < fit1)
		next
	    if (user > fit2)
		break
	
	    pix = id_center (id, fit_to_pix (id, user), 1, ID_FWIDTH(id),
		ID_FTYPE(id), NO)
	    if (!IS_INDEFD(pix)) {
		fit = id_fitpt (id, pix)
		diff = abs (fit - user)
		if (diff > ID_MATCH(id))
		    next
		if (lastpix > 0) {
		    if (abs (pix - Memd[pixes+lastpix-1]) < 0.01) {
			diff1 = abs (Memd[fits+lastpix-1]-Memd[users+lastpix-1])
			if (diff < diff1) {
			    Memd[pixes+lastpix-1] = pix
			    Memd[fits+lastpix-1] = fit
			    Memd[users+lastpix-1] = id_zshiftd (id, user, 0)
			    Memi[labels+lastpix-1] = label
			}
			next
		    }
		}
			
		peak = abs (id_peak (id, pix))
		if (nfound < ID_MAXFEATURES(id)) {
		    nfound = nfound + 1
		    if (peak < minval) {
		    	nextpix = nfound
			minval = peak
		    }
		    Memd[pixes+nfound-1] = pix
		    Memd[fits+nfound-1] = id_fitpt (id, pix)
		    Memd[users+nfound-1] = id_zshiftd (id, user, 0)
		    Memi[labels+nfound-1] = label
		    lastpix = nfound
		} else if (peak > minval) {
		    Memd[pixes+nextpix-1] = pix
		    Memd[fits+nextpix-1] = id_fitpt (id, pix)
		    Memd[users+nextpix-1] = id_zshiftd (id, user, 0)
		    Memi[labels+nextpix-1] = label
		    lastpix = nextpix

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
	    label = Memi[labels+i-1]
	    call id_newfeature (id, pix, fit, user, 1.0D0, ID_FWIDTH(id),
		ID_FTYPE(id), label)
	    if (i == 1)
		cursave = ID_CURRENT(id)
	}
	ID_CURRENT(id) = cursave

	call sfree (sp)
end
