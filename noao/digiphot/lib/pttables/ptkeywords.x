include "../ptkeysdef.h"

# PT_KYINIT -- Initilize the keyword structure.

procedure pt_kyinit (key)

pointer	key	# pointer to the keys structure

begin
	# Allocate space for structure and initialize constants.
	call malloc (key, LEN_KEYSTRUCT, TY_STRUCT)
	KY_NKEYS(key) = 0
	KY_NPKEYS(key) = 0
	KY_NSTORE(key) = KY_NPARS
	LEN_KWORDS(key) = 0

	# Allocate space for the string buffers.
	call malloc (KY_WORDS(key), KY_SZPAR * KY_NPARS, TY_CHAR)
	call malloc (KY_VALUES(key), KY_SZPAR * KY_NPARS, TY_CHAR)
	call malloc (KY_UNITS(key), KY_SZPAR * KY_NPARS, TY_CHAR)
	call malloc (KY_FORMATS(key), KY_SZPAR * KY_NPARS, TY_CHAR)

	# Allocate space for the indices buffers and initialize.
	call malloc (KY_PTRS(key), KY_NPARS, TY_INT)
	call malloc (KY_KINDICES(key), KY_NPARS, TY_INT)
	call malloc (KY_UINDICES(key), KY_NPARS, TY_INT)
	call malloc (KY_FINDICES(key), KY_NPARS + 1, TY_INT)
	call calloc (KY_NELEMS(key), KY_NPARS, TY_INT)
	call malloc (KY_TYPES(key), KY_NPARS, TY_INT)
	call calloc (KY_NPLINE(key), KY_NLINES, TY_INT)
	call calloc (KY_NCONTINUE(key), KY_NLINES, TY_INT)
	Memi[KY_PTRS(key)] = KY_VALUES(key)
	call amovki (NULL, Memi[KY_PTRS(key)+1], KY_NPARS - 1)

	# Initialize the select buffers.
	KY_SELECT(key) = NULL
	KY_ELEM_SELECT(key) = NULL
	KY_LEN_SELECT(key) = NULL
	KY_NAME_SELECT(key) = NULL
	KY_UNIT_SELECT(key) = NULL
	KY_FMT_SELECT(key) = NULL

	# Initilize the strings.
	Memc[KY_WORDS(key)] = EOS
	Memc[KY_VALUES(key)] = EOS
	Memc[KY_UNITS(key)] = EOS
	Memc[KY_FORMATS(key)] = EOS
end


# PT_KYFREE -- Procedure to free the keywords structure.

procedure pt_kyfree (key)

pointer	key		# pointer to keyword structure

int	i
pointer	ptr

begin
	# Free the string buffers.
	if (KY_WORDS(key) != NULL)
	    call mfree (KY_WORDS(key), TY_CHAR)
	if (KY_VALUES(key) != NULL)
	    call mfree (KY_VALUES(key), TY_CHAR)
	if (KY_UNITS(key) != NULL)
	    call mfree (KY_UNITS(key), TY_CHAR)
	if (KY_FORMATS(key) != NULL)
	    call mfree (KY_FORMATS(key), TY_CHAR)

	# Free the indices buffers.
	if (KY_TYPES(key) != NULL)
	    call mfree (KY_TYPES(key), TY_INT)
	if (KY_KINDICES(key) != NULL)
	    call mfree (KY_KINDICES(key), TY_INT)
	if (KY_UINDICES(key) != NULL)
	    call mfree (KY_UINDICES(key), TY_INT)
	if (KY_FINDICES(key) != NULL)
	    call mfree (KY_FINDICES(key), TY_INT)
	if (KY_PTRS(key) != NULL) {
	    do i = 1, KY_NSTORE(key) {
		ptr = Memi[KY_PTRS(key)+i-1]
		if (ptr != NULL && Memi[KY_NELEMS(key)+i-1] > 1)
		    call mfree (ptr, TY_CHAR)
	    }
	    call mfree (KY_PTRS(key), TY_INT)
	}
	if (KY_NELEMS(key) != NULL)
	    call mfree (KY_NELEMS(key), TY_INT)
	if (KY_NPLINE(key) != NULL)
	    call mfree (KY_NPLINE(key), TY_INT)
	if (KY_NCONTINUE(key) != NULL)
	    call mfree (KY_NCONTINUE(key), TY_INT)

	# Free the select buffers.
	if (KY_SELECT(key) != NULL)
	    call mfree (KY_SELECT(key), TY_INT)
	if (KY_ELEM_SELECT(key) != NULL)
	    call mfree (KY_ELEM_SELECT(key), TY_INT)
	if (KY_LEN_SELECT(key) != NULL)
	    call mfree (KY_LEN_SELECT(key), TY_INT)
	if (KY_NAME_SELECT(key) != NULL)
	    call mfree (KY_NAME_SELECT(key), TY_CHAR)
	if (KY_UNIT_SELECT(key) != NULL)
	    call mfree (KY_UNIT_SELECT(key), TY_CHAR)
	if (KY_FMT_SELECT(key) != NULL)
	    call mfree (KY_FMT_SELECT(key), TY_CHAR)

	call mfree (key, TY_STRUCT)
end


# PT_KYADD -- Procedure to add a parameter to the keyword structure.

procedure pt_kyadd (key, line, nchars)

pointer	key		# pointer to keyword structure
char	line[ARB]	# line to be decoded
int	nchars		# number of characters in the line

int	index, onstore
long	optr
pointer	sp, id, keyword, equals, value, units, format, temp 
int	nscan(), strdic()

begin
	# Check the buffer sizes.
	onstore = KY_NSTORE(key)
	optr = KY_VALUES(key)
	if ((KY_NKEYS(key) + 1) > KY_NSTORE(key)) {
	    KY_NSTORE(key) = KY_NSTORE(key) + KY_NPARS
	    call realloc (KY_WORDS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_VALUES(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_UNITS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_FORMATS(key), KY_NSTORE(key) * KY_SZPAR, TY_CHAR)
	    call realloc (KY_NELEMS(key), KY_NSTORE(key), TY_INT)
	    call aclri (Memi[KY_NELEMS(key)+onstore], KY_NSTORE(key) - onstore)
	    call realloc (KY_PTRS(key), KY_NSTORE(key), TY_INT)
	    call aaddki (Memi[KY_PTRS(key)], KY_VALUES(key) - optr,
	        Memi[KY_PTRS(key)], onstore)
	    call amovki (NULL, Memi[KY_PTRS(key)+onstore], KY_NSTORE(key) -
	        onstore)
	    call realloc (KY_TYPES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_KINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_UINDICES(key), KY_NSTORE(key), TY_INT)
	    call realloc (KY_FINDICES(key), KY_NSTORE(key) + 1, TY_INT)
	}

	# Allocate space for the keywords.
	call smark (sp)
	call salloc (id, KY_SZPAR, TY_CHAR)
	call salloc (keyword, KY_SZPAR, TY_CHAR)
	call salloc (equals, KY_SZPAR, TY_CHAR)
	call salloc (value, KY_SZPAR, TY_CHAR)
	call salloc (units, KY_SZPAR, TY_CHAR)
	call salloc (format, KY_SZPAR, TY_CHAR)
	call salloc (temp, KY_SZPAR, TY_CHAR)

	# Scan the string and decode the elements.
	call sscan (line)
	    call gargwrd (Memc[id], KY_SZPAR)
	    call gargwrd (Memc[keyword], KY_SZPAR)
	    call gargwrd (Memc[equals], KY_SZPAR)
	    call gargwrd (Memc[value], KY_SZPAR)
	    call gargwrd (Memc[units], KY_SZPAR)
	    call gargwrd (Memc[format], KY_SZPAR)

	# Return if insufficient number of elements.
	if (nscan() < 6) {
	    call sfree (sp)
	    return
	}

	# Add the parameters.
	index = strdic (Memc[keyword], Memc[temp], KY_SZPAR,
	    Memc[KY_WORDS(key)])
	if (index == 0) {
	    KY_NPKEYS(key) = KY_NPKEYS(key) + 1
	    KY_NKEYS(key) = KY_NKEYS(key) + 1
	    Memi[KY_NELEMS(key)+KY_NKEYS(key)-1] = 1 
	    call pt_kykeywrd (Memc[keyword], KY_SZPAR, LEN_KWORDS(key),
	        Memc[KY_WORDS(key)])
	    call pt_kyunits (Memc[units], KY_SZPAR, Memc[KY_UNITS(key)],
	        Memi[KY_UINDICES(key)], KY_NKEYS(key))
	    call pt_kyformat (Memc[format], KY_SZPAR, Memc[KY_FORMATS(key)],
	        Memi[KY_FINDICES(key)], Memi[KY_TYPES(key)], Memi[KY_PTRS(key)],
		Memi[KY_KINDICES(key)], KY_NKEYS(key))
	    call pt_kyvalue (Memc[value], KY_SZPAR, Memc[KY_VALUES(key)],
	        Memi[KY_PTRS(key)], Memi[KY_KINDICES(key)], KY_NKEYS(key))
        } else
	    call pt_kyaddval (Memc[value], KY_SZPAR, Memc[KY_VALUES(key)],
		Memi[KY_PTRS(key)], Memi[KY_KINDICES(key)], index)

	call sfree (sp)
end


# PT_KYKEYWRD -- Procedure to encode a single keyword into the keyword
# dictionary.

procedure pt_kykeywrd (keyword, maxch, klength, kdic)

char	keyword[ARB]	# the keyword to be added to the list
int	maxch		# maximum length of keyword
int	klength		# the current length of the dictionary
char	kdic[ARB]	# the keyword dictionary

int	kp, ip

begin
	# Insert leading , for record delimiter.
	kp = klength + 1
	if (kp == 1) {
	    kdic[kp] = ','
	    kp = kp + 1
	}

	# Copy the keyword.
	for (ip = 1; ip <= maxch && keyword[ip] != EOS; ip = ip + 1) {
	    kdic[kp] = keyword[ip]
	    kp = kp + 1
	}
	kdic[kp] = ','
	kdic[kp+1] = EOS

	klength = kp
end


# PT_KYUNITS -- Procedure to add a units name to the units string.

procedure pt_kyunits (ustr, maxch, units, uindices, nkey)

char	ustr[ARB]	# units string
int	maxch		# max length of units string
char	units[ARB]	# units string
int	uindices[ARB]	# units indices
int	nkey		# current unit

int	ip, up

begin
	# Get initial position.
	if (nkey == 1)
	    up = 1
	else
	    up = uindices[nkey-1] + 1

	# Copy the units string.
	for (ip = 1; ip <= maxch && ustr[ip] != EOS; ip = ip + 1) {
	    units[up] = ustr[ip]
	    up = up + 1
	}
	units[up] = EOS

	uindices[nkey] = up - 1
end


# PT_KYFORMAT -- Procedure to encode the formats and field widths.

procedure pt_kyformat (fstr, maxch, formats, findices, types, ptrs, fields,
	key)

char	fstr[ARB]		# format string
int	maxch			# maximum number of characters
char	formats[ARB]		# format string
int	findices[ARB]		# string pointers
int	types[ARB]		# data type of key
pointer	ptrs[ARB]		# array of pointers
int	fields[ARB]		# integer field widths
int	key			# key number

char	cjunk
int	ip, fp, fwidth, junk, ftype
int	pt_kyfstr(), strlen()

begin
	# Set the pointer.
	if (key > 1)
	    ptrs[key] = ptrs[key-1] + fields[key-1]

	# Find the initial position.
	if (key == 1)
	    fp = 1
	else
	    fp = findices[key-1] + 1

	# Crack the format string.
	if (pt_kyfstr (fstr, fwidth, junk, ftype, cjunk) == ERR) {
	    fields[key] = KY_SZPAR
	    types[key] = TY_CHAR
	    #if (key > 1)
		#ptrs[key] = ptrs[key-1] + fwidth
	    call sprintf (formats[fp], maxch, "%s%ds")
		call pargstr ("%")
		call pargi (-KY_SZPAR)
	    fp = fp + strlen (formats[fp])
	} else {
	    fields[key] = fwidth
	    types[key] = ftype
	    #if (key > 1)
		#ptrs[key] = ptrs[key-1] + fields[key-1]
	    for (ip = 1; ip <= maxch && fstr[ip] != EOS; ip = ip + 1) {
	        formats[fp] = fstr[ip]
	        fp = fp + 1
	    }
	    formats[fp] = EOS
	}

	findices[key] = fp - 1
end


# PT_KYVALUE -- Procedure to store the parameter value.

procedure pt_kyvalue (vstr, maxch, values, ptrs, kindices, key)

char	vstr[ARB]	# value string
int	maxch		# maximum number of characters
char	values[ARB]	# values string
pointer	ptrs[ARB]	# array of pointers
int	kindices[ARB]	# storage points in the string
int	key		# parameter

int	ip, vp

begin
	# Find the initial position.
	if (key == 1)
	    vp = 1
	else
	    vp = ptrs[key] - ptrs[1] + 1

	# Copy the value string.
	for (ip = 1; ip <= maxch && vstr[ip] != EOS; ip = ip + 1) {
	    values[vp] = vstr[ip]
	    vp = vp + 1
	}
	for (; ip <= kindices[key]; ip = ip + 1) {
	    values[vp] = ' '
	    vp = vp + 1
	}
	values[vp] = EOS
end


# PT_KYADDVAL -- Change the value of an existing keyword.

procedure pt_kyaddval (vstr, maxch, values, ptrs, kindices, key)

char	vstr[ARB]	# value string
int	maxch		# maximum number of characters
char	values[ARB]	# values string
pointer	ptrs[ARB]	# array of pointers
int	kindices[ARB]	# storage points in the string
int	key		# parameter

int	ip, vp

begin
	# Find the initial position.
	if (key == 1)
	    vp = 1
	else
	    vp = ptrs[key] - ptrs[1] + 1

	# Insert the new value.
	for (ip = 1; ip <= maxch && vstr[ip] != EOS; ip = ip + 1) {
	    values[vp] = vstr[ip]
	    vp = vp + 1
	}
	for (; ip <= kindices[key]; ip = ip + 1) {
	    values[vp] = ' '
	    vp = vp + 1
	}
end


# PT_KYFSTR -- Procedure to decode the format string.

int procedure pt_kyfstr (fstr, width, precision, type, ctype)

char	fstr[ARB]	# format string
int	width		# field width
int	precision	# precision
int	type		# data type
char	ctype		# char format type

int	ip
int	ctoi

begin
	if (fstr[1] != '%')
	    return (ERR)

	# Get the field width.
	ip = 2
	if (ctoi (fstr, ip, width) == ERR)
	    return (ERR)
	width = abs (width)

	# Get the precision.
	if (fstr[ip] == '.')
	    ip = ip + 1
	if (ctoi (fstr, ip, precision) == ERR)
	    precision = 0
	
	# Get the datatype.
	if (fstr[ip] == 'f' || fstr[ip] == 'g') {
	    type = TY_REAL
	    ctype = fstr[ip]
	} else if (fstr[ip] == 'd') {
	    type = TY_INT
	    ctype = 'd'
	} else if (fstr[ip] == 's') {
	    type = TY_CHAR
	    ctype = 's'
	} else if (fstr[ip] == 'b') {
	    type = TY_BOOL
	    ctype = 'b'
	} else
	    return (ERR)
	return (OK)
end


# PT_KYTYPE -- Procedure to get the data type of a field.

int procedure pt_kytype (key, field, root, ranges, maxels)

pointer	key		# pointer to field strucuture
char	field[ARB]	# field for which type is to be extracted
char	root[ARB]	# root variable name
char	ranges[ARB]	# range of selected elements
int	maxels		# maximum number of elements

char	left_bkt, right_bkt
int	findex, lindex, index
pointer	sp, temp
int	stridx(), strdic()
data	left_bkt /'['/, right_bkt /']'/

begin
	# Allocate working space.
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	root[1] = EOS
	ranges[1] = EOS

	# Get the root name.
	findex = stridx (left_bkt, field)
	if (findex == 0)
	    call strcpy (field, root, SZ_FNAME)
	else
	    call strcpy (field, root, findex - 1)

	# Find the appropriate keyword.
        index = strdic (root, Memc[temp], SZ_LINE, Memc[KY_WORDS(key)])
	call sfree (sp)

	# Return the root, ranges and type.
	if (index != 0) {
	    maxels = Memi[KY_NELEMS(key)+index-1]
	    lindex = stridx (right_bkt, field)
	    if ((lindex - findex - 1) > 0)
		call strcpy (field[findex+1], ranges, lindex - findex - 1)
	    if (ranges[1] == EOS && maxels > 1)
		call sprintf (ranges, SZ_FNAME, "1-%d")
		    call pargi (maxels)
	    return (Memi[KY_TYPES(key)+index-1])
	} else
	    call error (0, "Unknown keyword in expression")
end


# PT_KYBOOL -- Procedure to get a boolean parameter.

bool procedure pt_kybool (key, field, element)

pointer	key		# pointer to keys structure
char	field[ARB]	# parameter name
int	element		# element of int array

int	index
pointer	sp, temp, ptr
int	strmatch()
pointer	strdic()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)
	index = strdic (field, Memc[temp], SZ_LINE, Memc[KY_WORDS(key)])
	call sfree (sp)

	if (index != 0) {
	    if (Memi[KY_NELEMS(key)+index-1] == 1)
		ptr = Memi[KY_PTRS(key)+index-1]
	    else
		ptr = Memi[KY_PTRS(key)+index-1] + (element - 1) *
		    Memi[KY_KINDICES(key)+index-1]
	    if (strmatch (Memc[ptr], "yes") == 0)
		return (false)
	    else
	        return (true)
	} else
	    call error (0, "Unknown boolean keyword in expression")
end


# PT_KYINTEGER -- Procedure to get an integer parameter.

int procedure pt_kyinteger (key, field, element)

pointer	key		# pointer to keys structure
char	field[ARB]	# parameter name
int	element		# element of int array

int	index, ip, ival
pointer	sp, temp, ptr
int	ctoi()
pointer	strdic()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)
	index = strdic (field, Memc[temp], SZ_LINE, Memc[KY_WORDS(key)])
	call sfree (sp)

	if (index != 0) {
	    if (Memi[KY_NELEMS(key)+index-1] == 1)
		ptr = Memi[KY_PTRS(key)+index-1]
	    else
		ptr = Memi[KY_PTRS(key)+index-1] + (element - 1) *
		    Memi[KY_KINDICES(key)+index-1]
	    ip = 1
	    if (ctoi (Memc[ptr], ip, ival) == 0)
		call error (0, "Cannot decode integer parameter")
	    else
		return (ival)
	} else
	    call error (0, "Unknown integer keyword in expression")
end


# PT_KYREAL -- Procedure to get a real parameter.

real procedure pt_kyreal (key, field, element)

pointer	key		# pointer to keys structure
char	field[ARB]	# parameter name
int	element		# which element to extract

int	index, ip
pointer	sp, temp, ptr
real	rval
int	ctor()
pointer	strdic()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)
	index = strdic (field, Memc[temp], SZ_LINE, Memc[KY_WORDS(key)])
	call sfree (sp)

	if (index != 0) {
	    if (Memi[KY_NELEMS(key)+index-1] == 1)
		ptr = Memi[KY_PTRS(key)+index-1]
	    else
		ptr = Memi[KY_PTRS(key)+index-1] + (element - 1) *
		    Memi[KY_KINDICES(key)+index-1]
	    ip = 1
	    if (ctor (Memc[ptr], ip, rval) <= 0)
		call error (0, "Cannot decode real parameter")
	    else
		return (rval)
	} else
	    call error (0, "Unknown real keyword in expression")
end


# PT_KYSTR -- Procedure to get a string parameter.

procedure pt_kystr (key, field, element, str, maxch)

pointer	key		# pointer to keys structure
char	field[ARB]	# parameter name
int	element		# element of the array
char	str[ARB]	# output string
int	maxch		# maximum number of character

int	index, nk
pointer	sp, temp, ptr
pointer	strdic()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)
	index = strdic (field, Memc[temp], SZ_LINE, Memc[KY_WORDS(key)])
	call sfree (sp)

	if (index != 0) {
	    if (Memi[KY_NELEMS(key)+index-1] == 1)
		ptr = Memi[KY_PTRS(key)+index-1]
	    else
		ptr = Memi[KY_PTRS(key)+index-1] + (element - 1) *
		    Memi[KY_KINDICES(key)+index-1]
	    for (; Memc[ptr] == ' ';)
		ptr = ptr + 1
	    for (nk = 0; Memc[ptr+nk] != ' '; nk = nk + 1)
		;
	    call strcpy (Memc[ptr], str, min (nk, maxch))
	} else
	    call error (0, "Unknown string keyword in expression")
end
