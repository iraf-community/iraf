include	<error.h>
include	<mach.h>
include	<units.h>
include	"identify.h"

# ID_MAPLL -- Read the line list into memory.
# Convert to desired units.

procedure id_mapll (id)

pointer	id		# Identify structure

int	i, j, fd, nalloc, nlines
pointer	ll, lll, ill
pointer	sp, str, units
double	value

bool	streq(), fp_equald()
int	open(), fscan(), nscan(), nowhite(), id_compare()
pointer	un_open()
errchk	open, fscan, malloc, realloc, un_open
extern	id_compare()

begin
	call id_unmapll (id)

	if (nowhite (ID_COORDLIST(id), ID_COORDLIST(id), ID_LENSTRING) == 0)
	    return
	iferr (fd = open (ID_COORDLIST(id), READ_ONLY, TEXT_FILE)) {
	    call erract (EA_WARN)
	    return
	}

	ID_COORDSPEC(id) = EOS
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)
	call strcpy ("Angstroms", Memc[units], SZ_LINE)
	nalloc = 0
	nlines = 0
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan() != 1)
		next
	    if (Memc[str] == '#') {
		call gargwrd (Memc[str], SZ_LINE)
		call strlwr (Memc[str])
		if (streq (Memc[str], "spectrum"))
		    call gargwrd (ID_COORDSPEC(id), ID_LENSTRING)
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
		call malloc (ll, nalloc, TY_DOUBLE)
		call calloc (lll, nalloc, TY_POINTER)
	    } else if (nlines == nalloc) {
		nalloc = nalloc + 100
		call realloc (ll, nalloc, TY_DOUBLE)
		call realloc (lll, nalloc, TY_POINTER)
		call aclri (Memi[lll+nalloc-100], 100)
	    }

	    Memd[ll+nlines] = value
	    call gargstr (Memc[str], SZ_LINE)
	    call id_label (Memc[str], Memi[lll+nlines])

	    nlines = nlines + 1
	}
	call close (fd)

	# Sort the lines, eliminate identical lines, and convert units.
	if (nlines > 0) {
	    call malloc (ID_LL(id), nlines + 1, TY_DOUBLE)
	    call malloc (ID_LLL(id), nlines + 1, TY_POINTER)

	    call malloc (ill, nlines, TY_INT)
	    do i = 0, nlines-1
		Memi[ill+i] = i
	    call gqsort (Memi[ill], nlines, id_compare, ll)

	    Memd[ID_LL(id)] = Memd[ll+Memi[ill]]
	    Memi[ID_LLL(id)] = Memi[lll+Memi[ill]]
	    j = 1
	    do i = 1, nlines-1 {
		if (fp_equald (Memd[ll+Memi[ill+i]], Memd[ID_LL(id)+j-1]))
		    next
		Memd[ID_LL(id)+j] = Memd[ll+Memi[ill+i]]
		Memi[ID_LLL(id)+j] = Memi[lll+Memi[ill+i]]
		j = j + 1
	    }
	    Memd[ID_LL(id)+j] = INDEFD
	    ID_NLL(id) = j

	    call mfree (ll, TY_DOUBLE)
	    call mfree (lll, TY_POINTER)
	    call mfree (ill, TY_INT)

	    if (ID_UN(id) == NULL && Memc[units] != EOS)
		ID_UN(id) = un_open (Memc[units])
	    call id_unitsll (id, Memc[units])
	}

	call sfree (sp)
end


# ID_UNMAPLL -- Unmap the linelist.

procedure id_unmapll (id)

pointer	id		# Identify structure

pointer	lll

begin
	if (ID_LL(id) == NULL)
	    return

	do lll = ID_LLL(id), ID_LLL(id)+ID_NLL(id)-1
	    call mfree (Memi[lll], TY_CHAR)

	call mfree (ID_LL(id), TY_DOUBLE)
	call mfree (ID_LLL(id), TY_POINTER)
end


# ID_UNITSLL -- Change the line list units from the input units to the
# units given by ID_UN.  This may involve reversing the order of the list.

procedure id_unitsll (id, units)

pointer	id			# Identify structure
char	units[ARB]		# Input units

int	i, nll
double	value
pointer	un, ll, lll, llend, lllend, un_open()
bool	un_compare()
errchk	un_open

begin
	if (ID_LL(id) == NULL)
	    return
	if (ID_NLL(id) < 1)
	    return
	if (units[1] == EOS || ID_UN(id) == NULL)
	    return
	if (UN_CLASS(ID_UN(id)) == UN_UNKNOWN)
	    return

	un = un_open (units)
	if (un_compare (un, ID_UN(id))) {
	    call un_close (un)
	    return
	}

	ll = ID_LL(id)
	lll = ID_LLL(id)
	nll = ID_NLL(id)
	call un_ctrand (un, ID_UN(id), Memd[ll], Memd[ll], nll)
	call un_close (un)

	if (Memd[ll] > Memd[ll+nll-1]) {
	    llend = ll + nll - 1
	    lllend = lll + nll - 1
	    do i = 0, nll / 2 - 1 {
		value = Memd[ll+i]
		Memd[ll+i] = Memd[llend-i]
		Memd[llend-i] = value
		un = Memi[lll+i]
		Memi[lll+i] = Memi[lllend-i]
		Memi[lllend-i] = un
	    }
	}
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

int	i, j, nll
double	delta
pointer	ll
int	strlen()

begin
	call mfree (label, TY_CHAR)

	if (ID_LL(id) == NULL) {
	    out = in
	    return
	}

	if (diff < 0.)
	    delta = abs (diff * (FITDATA(id,1) - FITDATA(id,ID_NPTS(id))) /
		(ID_NPTS(id) - 1))
	else
	    delta = diff

	ll = ID_LL(id)
	nll = ID_NLL(id)
	j = max (1, nint (sqrt (real (nll))))
	for (i = 0; i < nll && in > Memd[ll+i]; i = i + j)
	    ;
	for (i = max (0, min (i-1, nll-1)); i > 0 && in < Memd[ll+i]; i = i - 1)
	    ;

	ll = ll + i
	if (i < nll-1) {
	    if (abs (in - Memd[ll]) > abs (in - Memd[ll+1])) {
		i = i + 1
		ll = ll + 1
	    }
	}

	if (abs (in - Memd[ll]) <= delta) {
	    out = Memd[ll]
	    ll = Memi[ID_LLL(id)+i]
	    if (ll != NULL) {
		call malloc (label, strlen (Memc[ll]), TY_CHAR)
		call strcpy (Memc[ll], Memc[label], ARB)
	    }
	}
end

# ID_LINELIST -- Add features from a line list.

procedure id_linelist (id)

pointer	id			# Identify structure

int	i, nfound, nextpix, lastpix, cursave
double	cd, pix, fit, fit1, fit2, user, peak, minval, diff, diff1
pointer	sp, pixes, fits, users, labels, ll, lll, label

double	id_center(), fit_to_pix(), id_fitpt(), id_peak(), smw_c1trand()

int	ncandidate, nmatch1, nmatch2
common	/llstat/ ncandidate, nmatch1, nmatch2

begin
	if (ID_LL(id) == NULL)
	    return

	call smark (sp)
	call salloc (pixes, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (fits, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (users, ID_MAXFEATURES(id), TY_DOUBLE)
	call salloc (labels, ID_MAXFEATURES(id), TY_POINTER)

	ncandidate = 0
	nmatch1 = 0
	nmatch2 = 0
	nfound = 0
	lastpix = 0
	minval = MAX_REAL

	if (ID_MATCH(id) < 0.)
	    cd = (FITDATA(id,1) - FITDATA(id,ID_NPTS(id))) / (ID_NPTS(id) - 1)
	else
	    cd = 1

	fit1 = min (FITDATA(id,1), FITDATA(id,ID_NPTS(id)))
	fit2 = max (FITDATA(id,1), FITDATA(id,ID_NPTS(id)))
	ll = ID_LL(id)
	lll = ID_LLL(id)
	while (!IS_INDEFD(Memd[ll])) {
	    user = Memd[ll]
	    label = Memi[lll]
	    ll = ll + 1
	    lll = lll + 1
	    if (user < fit1)
		next
	    if (user > fit2)
		break
	
	    ncandidate = ncandidate + 1
	    pix = id_center (id, fit_to_pix (id, user), ID_FWIDTH(id),
		ID_FTYPE(id))
	    if (!IS_INDEFD(pix)) {
		fit = id_fitpt (id, pix)
		diff = abs ((fit - user) / cd)
		if (diff > abs (ID_MATCH(id)))
		    next

		nmatch1 = nmatch1 + 1
		if (lastpix > 0) {
		    if (abs (pix - Memd[pixes+lastpix-1]) < 0.01) {
			diff1 = abs (Memd[fits+lastpix-1]-Memd[users+lastpix-1])
			if (diff < diff1) {
			    Memd[pixes+lastpix-1] = pix
			    Memd[fits+lastpix-1] = fit
			    Memd[users+lastpix-1] = user
			    Memi[labels+lastpix-1] = label
			}
			next
		    }
		}

		nmatch2 = nmatch2 + 1
		peak = abs (id_peak (id, smw_c1trand (ID_PL(id), pix)))
		if (nfound < ID_MAXFEATURES(id)) {
		    nfound = nfound + 1
		    if (peak < minval) {
		    	nextpix = nfound
			minval = peak
		    }
		    Memd[pixes+nfound-1] = pix
		    Memd[fits+nfound-1] = fit
		    Memd[users+nfound-1] = user
		    Memi[labels+nfound-1] = label
		    lastpix = nfound
		} else if (peak > minval) {
		    Memd[pixes+nextpix-1] = pix
		    Memd[fits+nextpix-1] = fit
		    Memd[users+nextpix-1] = user
		    Memi[labels+nextpix-1] = label
		    lastpix = nextpix

		    minval = MAX_REAL
		    do i = 1, nfound {
			pix = Memd[pixes+i-1]
			peak = abs (id_peak (id, smw_c1trand (ID_PL(id), pix)))
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


# ID_COMPARE - Routine to compare line list coordinates for sorting.
# Zero indexing is used.

int procedure id_compare (ll, x1, x2)

pointer	ll		#I Pointer to array of line list coordinates
int	x1, x2		#I Indices to array of line list coordinates

begin
	if (Memd[ll+x1] < Memd[ll+x2])
	    return (-1)
	else if (Memd[ll+x1] > Memd[ll+x2])
	    return (1)
	else
	    return (0)
end
