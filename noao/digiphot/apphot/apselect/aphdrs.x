include "../lib/apkeysdef.h"

# AP_KNAME -- Procedure to add a name to the keyword structure

procedure ap_kname (key, line, nchars, nunique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	nunique		# number of #N lines

int	nkeys, onstore
long	optr, lshift
pointer	sp, id, keyword, temp
int	nscan(), strdic()

begin
	# Check the buffer sizes.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    lshift = KY_VALUES(key) - optr
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], lshift, Memi[KY_PTRS(key)],
		onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
		onstore)
	}

	# Check the available space.
	if (nunique > NLINES)
	    call realloc (KY_NPLINE(key), nunique, TY_INT)

	# Allocate space for the keywords.
	call smark (sp)
	call salloc (id, SZ_PAR, TY_CHAR)
	call salloc (keyword, SZ_PAR, TY_CHAR)
	call salloc (temp, SZ_PAR, TY_CHAR)

	# Scan the string and remove the id.
	call sscan (line)
	    call gargwrd (Memc[id], SZ_PAR)
	if (nscan() != 1) {
	    call sfree (sp)
	    return
	}

	# Loop over the keywords.
	nkeys = 0
	call gargwrd (Memc[keyword], SZ_PAR)
	while (Memc[keyword] != EOS && Memc[keyword] != '\\') {
	    if (strdic (Memc[keyword], Memc[temp], SZ_PAR,
	        Memc[KY_WORDS(key)]) == 0) {
	        nkeys = nkeys + 1
	        call ap_kykeywrd (Memc[keyword], SZ_PAR, LEN_KWORDS(key),
	            Memc[KY_WORDS(key)])
            }
	    call gargwrd (Memc[keyword], SZ_PAR)
	}

	# Update.
	if (nunique == 1)
	    KY_NOKEYS(key) = KY_NKEYS(key)
	Memi[KY_NPLINE(key)+nunique-1] = nkeys
	KY_NKEYS(key) = KY_NKEYS(key) + nkeys

	call sfree (sp)
end


# AP_KNUNITS -- Procedure to add a unit name to the keyword structure.

procedure ap_knunits (key, line, nchars, uunique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	uunique		# number of #U lines

int	nkeys, onstore
long	optr, lshift
pointer	sp, id, units, temp
int	nscan()

begin
	# Check the buffer sizes.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    lshift = KY_VALUES(key) - optr
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], lshift, Memi[KY_PTRS(key)],
		onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
		onstore)
	}

	# Allocate space for the units.
	call smark (sp)
	call salloc (id, SZ_PAR, TY_CHAR)
	call salloc (units, SZ_PAR, TY_CHAR)
	call salloc (temp, SZ_PAR, TY_CHAR)

	# Scan the string and decode the elements.
	call sscan (line)
	    call gargwrd (Memc[id], SZ_PAR)

	# Remove the id.
	if (nscan() != 1) {
	    call sfree (sp)
	    return
	}

	# Loop over the units string.
	if (uunique == 1)
	    nkeys = KY_NPKEYS(key)
	call gargwrd (Memc[units], SZ_PAR)
	while (Memc[units] != EOS && Memc[units] != '\\') {
	    call ap_kyunits (Memc[units], SZ_PAR, Memc[KY_UNITS(key)],
	        Memi[KY_UINDICES(key)], nkeys + 1)
	    nkeys = nkeys + 1
	    call gargwrd (Memc[units], SZ_PAR)
	}

	call sfree (sp)
end


# AP_KNFORMATS -- Procedure to add a format to the keyword structure.

procedure ap_knformats (key, line, nchars, funique)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line
int	funique		# number of format lines

int	nkeys, onstore
long	optr, lshift
pointer	sp, id, format, temp
int	nscan()

begin
	# Check the buffer sizes.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * SZ_PAR, TY_CHAR)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    lshift = KY_VALUES(key) - optr
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], lshift, Memi[KY_PTRS(key)],
		onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
		onstore)
	}

	# Allocate space for the keywords.
	call smark (sp)
	call salloc (id, SZ_PAR, TY_CHAR)
	call salloc (format, SZ_PAR, TY_CHAR)
	call salloc (temp, SZ_PAR, TY_CHAR)

	# Scan the string and decode the elements.
	call sscan (line)
	    call gargwrd (Memc[id], SZ_PAR)

	# Remove the id.
	if (nscan() != 1) {
	    call sfree (sp)
	    return
	}

	# Loop over the formats.
	if (funique == 1)
	    nkeys = KY_NPKEYS(key)
	call gargwrd (Memc[format], SZ_PAR)
	while (Memc[format] != EOS && Memc[format] != '\\') {
	    call ap_kyformat (Memc[format], SZ_PAR, Memc[KY_FORMATS(key)],
	        Memi[KY_FINDICES(key)], Memi[KY_TYPES(key)], Memi[KY_PTRS(key)],
		Memi[KY_KINDICES(key)], nkeys + 1)
	    nkeys = nkeys + 1
	    call gargwrd (Memc[format], SZ_PAR)
	}

	call sfree (sp)
end
