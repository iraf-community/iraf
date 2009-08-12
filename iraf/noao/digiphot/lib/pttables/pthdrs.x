include "../ptkeysdef.h"

# PT_KNAME -- Procedure to add a name to the keyword structure

procedure pt_kname (key, line, nchars, nunique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	nunique		# number of #N lines

size_t	sz_val
int	nkeys, onstore
pointer	sp, id, keyword, temp, optr
int	nscan(), strdic()

include	<nullptr.inc>

begin
	# Store the old number of keywords and the old values pointer.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)

	# Check the buffer sizes.
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + KY_NPARS
	    sz_val = KY_NSTORE(key) * KY_SZPAR
	    call realloc (KY_WORDS(key), sz_val, TY_CHAR)
	    call realloc (KY_VALUES(key), sz_val, TY_CHAR)
	    call realloc (KY_UNITS(key), sz_val, TY_CHAR)
	    call realloc (KY_FORMATS(key), sz_val, TY_CHAR)
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_TYPES(key), sz_val, TY_INT)
	    call realloc (KY_KINDICES(key), sz_val, TY_INT)
	    call realloc (KY_UINDICES(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key) + 1
	    call realloc (KY_FINDICES(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_NELEMS(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key) - onstore
	    call aclri (Memi[KY_NELEMS(key)+onstore], sz_val)
	    #lshift = KY_VALUES(key) - optr
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_PTRS(key), sz_val, TY_POINTER)
	    sz_val = onstore
	    call aaddkp (Memp[KY_PTRS(key)], KY_VALUES(key) - optr,
			 Memp[KY_PTRS(key)], sz_val)
	    sz_val = KY_NSTORE(key) - onstore
	    call amovkp (NULLPTR, Memp[KY_PTRS(key)+onstore], sz_val)
	}

	# Check the available space.
	if (nunique > KY_NLINES) {
	    sz_val = nunique
	    call realloc (KY_NPLINE(key), sz_val, TY_INT)
	    call realloc (KY_NCONTINUE(key), sz_val, TY_INT)
	}

	# Allocate space for the keywords.
	call smark (sp)
	sz_val = KY_SZPAR
	call salloc (id, sz_val, TY_CHAR)
	call salloc (keyword, sz_val, TY_CHAR)
	call salloc (temp, sz_val, TY_CHAR)

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

size_t	sz_val
int	nkeys, onstore
pointer	sp, id, units, temp, optr
int	nscan()

include	<nullptr.inc>

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
	    sz_val = KY_NSTORE(key) * KY_SZPAR
	    call realloc (KY_WORDS(key), sz_val, TY_CHAR)
	    call realloc (KY_VALUES(key), sz_val, TY_CHAR)
	    call realloc (KY_UNITS(key), sz_val, TY_CHAR)
	    call realloc (KY_FORMATS(key), sz_val, TY_CHAR)
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_TYPES(key), sz_val, TY_INT)
	    call realloc (KY_KINDICES(key), sz_val, TY_INT)
	    call realloc (KY_UINDICES(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key) + 1
	    call realloc (KY_FINDICES(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_NELEMS(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key) - onstore
	    call aclri (Memi[KY_NELEMS(key)+onstore], sz_val)
	    #lshift = KY_VALUES(key) - optr
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_PTRS(key), sz_val, TY_POINTER)
	    sz_val = onstore
	    call aaddkp (Memp[KY_PTRS(key)], KY_VALUES(key) - optr,
			 Memp[KY_PTRS(key)], sz_val)
	    sz_val = KY_NSTORE(key) - onstore
	    call amovkp (NULLPTR, Memp[KY_PTRS(key)+onstore], sz_val)
	}

	# Allocate space for the units.
	call smark (sp)
	sz_val = KY_SZPAR
	call salloc (id, sz_val, TY_CHAR)
	call salloc (units, sz_val, TY_CHAR)
	call salloc (temp, sz_val, TY_CHAR)

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

size_t	sz_val
int	nkeys, onstore
pointer	sp, id, format, temp, optr
int	nscan()

include	<nullptr.inc>

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
	    sz_val = KY_NSTORE(key) * KY_SZPAR
	    call realloc (KY_WORDS(key), sz_val, TY_CHAR)
	    call realloc (KY_VALUES(key), sz_val, TY_CHAR)
	    call realloc (KY_UNITS(key), sz_val, TY_CHAR)
	    call realloc (KY_FORMATS(key), sz_val, TY_CHAR)
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_TYPES(key), sz_val, TY_INT)
	    call realloc (KY_KINDICES(key), sz_val, TY_INT)
	    call realloc (KY_UINDICES(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key) + 1
	    call realloc (KY_FINDICES(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_NELEMS(key), sz_val, TY_INT)
	    sz_val = KY_NSTORE(key) - onstore
	    call aclri (Memi[KY_NELEMS(key)+onstore], sz_val)
	    #lshift = KY_VALUES(key) - optr
	    sz_val = KY_NSTORE(key)
	    call realloc (KY_PTRS(key), sz_val, TY_POINTER)
	    sz_val = onstore
	    call aaddkp (Memp[KY_PTRS(key)], KY_VALUES(key) - optr,
			 Memp[KY_PTRS(key)], sz_val)
	    sz_val = KY_NSTORE(key) - onstore
	    call amovkp (NULLPTR, Memp[KY_PTRS(key)+onstore], sz_val)
	}

	# Allocate space for the keywords.
	call smark (sp)
	sz_val = KY_SZPAR
	call salloc (id, sz_val, TY_CHAR)
	call salloc (format, sz_val, TY_CHAR)
	call salloc (temp, sz_val, TY_CHAR)

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
	        Memi[KY_FINDICES(key)], Memi[KY_TYPES(key)],
		Memp[KY_PTRS(key)], Memi[KY_KINDICES(key)], nkeys + 1)
	    nkeys = nkeys + 1
	    call gargwrd (Memc[format], KY_SZPAR)
	}

	call sfree (sp)
end
