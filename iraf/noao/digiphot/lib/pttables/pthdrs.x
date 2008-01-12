include "../ptkeysdef.h"

# PT_KNAME -- Procedure to add a name to the keyword structure

procedure pt_kname (key, line, nchars, nunique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	nunique		# number of #N lines

int	nkeys, onstore
long	optr
pointer	sp, id, keyword, temp
int	nscan(), strdic()

begin
	# Store the old number of keywords and the old values pointer.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)

	# Check the buffer sizes.
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + KY_NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    #lshift = KY_VALUES(key) - optr
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], KY_VALUES(key)- optr,
	        Memi[KY_PTRS(key)], onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
		onstore)
	}

	# Check the available space.
	if (nunique > KY_NLINES) {
	    call realloc (KY_NPLINE(key), nunique, TY_INT)
	    call realloc (KY_NCONTINUE(key), nunique, TY_INT)
	}

	# Allocate space for the keywords.
	call smark (sp)
	call salloc (id, KY_SZPAR, TY_CHAR)
	call salloc (keyword, KY_SZPAR, TY_CHAR)
	call salloc (temp, KY_SZPAR, TY_CHAR)

	# Scan the string and remove the id.
	call sscan (line)
	    call gargwrd (Memc[id], KY_SZPAR)
	if (nscan() != 1) {
	    call sfree (sp)
	    return
	}

	# Loop over the keywords.
	nkeys = 0
	call gargwrd (Memc[keyword], KY_SZPAR)
	while (Memc[keyword] != EOS && Memc[keyword] != '\\') {
	    if (strdic (Memc[keyword], Memc[temp], KY_SZPAR,
	        Memc[KY_WORDS(key)]) == 0) {
	        nkeys = nkeys + 1
	        call pt_kykeywrd (Memc[keyword], KY_SZPAR, LEN_KWORDS(key),
	            Memc[KY_WORDS(key)])
            }
	    call gargwrd (Memc[keyword], KY_SZPAR)
	}

	# Update.
	#if (nunique == 1)
	    #KY_NOKEYS(key) = KY_NKEYS(key)
	Memi[KY_NPLINE(key)+nunique-1] = nkeys
	Memi[KY_NCONTINUE(key)+nunique-1] = 0
	KY_NKEYS(key) = KY_NKEYS(key) + nkeys

	call sfree (sp)
end


# PT_KNUNITS -- Procedure to add a unit name to the keyword structure.

procedure pt_knunits (key, line, nchars, uunique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	uunique		# number of #U lines

int	nkeys, onstore
long	optr
pointer	sp, id, units, temp
int	nscan()

begin
	# If there are no unique names for this line quit.
	if (Memi[KY_NPLINE(key)+uunique-1] <= 0)
	    return

	# Store old number of keywords and old values pointer.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)

	# Check the buffer sizes.
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + KY_NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    #lshift = KY_VALUES(key) - optr
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], KY_VALUES(key) - optr,
	        Memi[KY_PTRS(key)], onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
		onstore)
	}

	# Allocate space for the units.
	call smark (sp)
	call salloc (id, KY_SZPAR, TY_CHAR)
	call salloc (units, KY_SZPAR, TY_CHAR)
	call salloc (temp, KY_SZPAR, TY_CHAR)

	# Scan the string and decode the elements.
	call sscan (line)
	    call gargwrd (Memc[id], KY_SZPAR)

	# Remove the id.
	if (nscan() != 1) {
	    call sfree (sp)
	    return
	}

	# Loop over the units string.
	if (uunique == 1)
	    nkeys = KY_NPKEYS(key)
	call gargwrd (Memc[units], KY_SZPAR)
	while (Memc[units] != EOS && Memc[units] != '\\') {
	    call pt_kyunits (Memc[units], KY_SZPAR, Memc[KY_UNITS(key)],
	        Memi[KY_UINDICES(key)], nkeys + 1)
	    nkeys = nkeys + 1
	    call gargwrd (Memc[units], KY_SZPAR)
	}

	call sfree (sp)
end


# PT_KNFORMATS -- Procedure to add a format to the keyword structure.

procedure pt_knformats (key, line, nchars, funique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	funique		# number of format lines

int	nkeys, onstore
long	optr
pointer	sp, id, format, temp
int	nscan()

begin
	# If there are no unique names for this line quit.
	if (Memi[KY_NPLINE(key)+funique-1] <= 0)
	    return

	# Store the old number of keywords and the old values pointer.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)

	# Check the buffer sizes.
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + KY_NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    #lshift = KY_VALUES(key) - optr
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], KY_VALUES(key) - optr,
	        Memi[KY_PTRS(key)], onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
		onstore)
	}

	# Allocate space for the keywords.
	call smark (sp)
	call salloc (id, KY_SZPAR, TY_CHAR)
	call salloc (format, KY_SZPAR, TY_CHAR)
	call salloc (temp, KY_SZPAR, TY_CHAR)

	# Scan the string and decode the elements.
	call sscan (line)
	    call gargwrd (Memc[id], KY_SZPAR)

	# Remove the id.
	if (nscan() != 1) {
	    call sfree (sp)
	    return
	}

	# Loop over the formats.
	if (funique == 1)
	    nkeys = KY_NPKEYS(key)
	call gargwrd (Memc[format], KY_SZPAR)
	while (Memc[format] != EOS && Memc[format] != '\\') {
	    call pt_kyformat (Memc[format], KY_SZPAR, Memc[KY_FORMATS(key)],
	        Memi[KY_FINDICES(key)], Memi[KY_TYPES(key)], Memi[KY_PTRS(key)],
		Memi[KY_KINDICES(key)], nkeys + 1)
	    nkeys = nkeys + 1
	    call gargwrd (Memc[format], KY_SZPAR)
	}

	call sfree (sp)
end
