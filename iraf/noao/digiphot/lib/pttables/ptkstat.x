include "../ptkeysdef.h"

# PT_KSTATI -- Get an integer parameter from the keyword structure.

int procedure pt_kstati (key, column, parameter)

pointer	key		# pointer to the database strucuture
char	column[ARB]	# column name
int	parameter	# parameter to be returned

char	left_bracket
int	index, element, value
pointer	sp, kname
int	strdic(), stridx(), ctoi()
data	left_bracket /'['/

begin
	call smark (sp)
	call salloc (kname, KY_SZPAR, TY_CHAR)

	# Get the proper name in upper case and strip off the subscript.
	call strcpy (column, Memc[kname], KY_SZPAR)
	call strupr (Memc[kname])
	index = stridx (left_bracket, Memc[kname])
	if (index > 0) {
	    Memc[kname+index-1] = EOS
	    index = index + 1
	    if (ctoi (column, index, element) < 0)
		element = 1
	} else
	    element = 1

	# Find the field.
	index = strdic (Memc[kname], Memc[kname], KY_SZPAR, Memc[KY_WORDS(key)])

	# Fetch the parameter.
	switch (parameter) {
	case KY_INDEX:
	    value = index
	case KY_DATATYPE:
	    if (index > 0)
	        value = Memi[KY_TYPES(key)+index-1]
	    else
		value = INDEFI
	case KY_LENGTH:
	    if (index > 0)
	        value = Memi[KY_KINDICES(key)+index-1]
	    else
		value = INDEFI
	case KY_ELEMENT:
	    if (index <= 0)
		value = INDEFI
	    else if (element >= 1 && element <= Memi[KY_NELEMS(key)+index-1])
		value = element
	    else
		value = INDEFI
	case KY_NUMELEMS:
	    value = Memi[KY_NELEMS(key)+index-1]
	default:
	    value = INDEFI
	}

	call sfree (sp)

	return (value)
end


# PT_KSTATS -- Get a string parameter from the keyword structure.

procedure pt_kstats (key, column, parameter, str, maxch)

pointer	key		# pointer to the database strucuture
char	column[ARB]	# column name
int	parameter	# parameter to be returned
char	str[ARB]	# output string
int	maxch		# maximum number of characters

char	left_bracket
int	index, element, ip, len
pointer	sp, kname
int	strdic(), stridx(), ctoi()
data	left_bracket /'['/

begin
	call smark (sp)
	call salloc (kname, KY_SZPAR, TY_CHAR)

	# Get the proper name in upper case and strip off the subscript.
	call strcpy (column, Memc[kname], KY_SZPAR)
	call strupr (Memc[kname])
	index = stridx (left_bracket, Memc[kname])
	if (index > 0) {
	    Memc[kname+index-1] = EOS
	    index = index + 1
	    if (ctoi (column, index, element) < 0)
		element = 1
	} else
	    element = 1

	# Find the field.
	index = strdic (Memc[kname], Memc[kname], KY_SZPAR, Memc[KY_WORDS(key)])

	# Fetch the parameter.
	switch (parameter) {
	case KY_UNITSTR:
	    if (index <= 0) {
		str[1] = EOS
	    } else if (index == 1) {
		ip = 1
		len = Memi[KY_UINDICES(key)]
	        call strcpy (Memc[KY_UNITS(key)+ip-1], str, len)
	    } else {
		ip =  Memi[KY_UINDICES(key)+index-2] + 1
		len = Memi[KY_UINDICES(key)+index-1] -
		    Memi[KY_UINDICES(key)+index-2]
	        call strcpy (Memc[KY_UNITS(key)+ip-1], str, len)
	    }
	case KY_FMTSTR:
	    if (index <= 0) {
		str[1] = EOS
	    } else if (index == 1) {
		ip = 1
		len = Memi[KY_FINDICES(key)]
	        call strcpy (Memc[KY_FORMATS(key)+ip-1], str, len)
	    } else {
		ip =  Memi[KY_FINDICES(key)+index-2] + 1
		len = Memi[KY_FINDICES(key)+index-1] -
		    Memi[KY_FINDICES(key)+index-2]
	        call strcpy (Memc[KY_FORMATS(key)+ip-1], str, len)
	    }
	default:
	    str[1] = EOS
	}

	call sfree (sp)
end
