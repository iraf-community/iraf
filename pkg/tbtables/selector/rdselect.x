define	MAXSECT		3

# RDSELECT -- Break a filename into root and selectors

procedure rdselect (file, root, rowselect, colselect, maxch)
		       
char	file[ARB]	# i: filename
char	root[ARB]	# o: filename minus any selectors
char	rowselect[ARB]	# o: row selector
char	colselect[ARB]	# o: column selector
int	maxch		# i: max length of output strings
#--
char	colon
int	ic, nc, isect, nsect, idtype
pointer	sp, ident, extend, errmsg, bracket[MAXSECT]

data	colon	 / ':' /
string	idlist	 "|row|column|"
string	badtype  "Unrecognized selector (%s)"

bool	nextbrak()
int	stridx(), strdic()

errchk	nextbrak

begin
	call smark (sp)
	call salloc (ident, SZ_FNAME, TY_CHAR)
	call salloc (extend, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Search for the first unescaped bracket
	# Copy all chars prior to bracket into root

	for (ic = 1; file[ic] != EOS; ic = ic + 1) {
	    if (file[ic] == '\\' && file[ic+1] != EOS) {
		ic = ic + 1
	    } else if (file[ic] == '['){
		break
	    }
	}

	nc = min (ic-1, maxch)
	call strcpy (file, root, nc)

	# Get bracketed sections from file name

	for (isect = 1; isect <= MAXSECT; isect = isect + 1) {

	    call salloc (bracket[isect], SZ_FNAME, TY_CHAR)
	    if (! nextbrak (file, ic, Memc[bracket[isect]], maxch))
		break
	}

	nsect = isect - 1

	rowselect[1] = EOS
	colselect[1] = EOS

	# Use leading identifier to determine type of selector

	do isect = 1, nsect {
	    ic = stridx (colon, Memc[bracket[isect]])
	    if (ic == 0) {
		# Append bracketed sections with no identifier to the root

		call sprintf (Memc[extend], SZ_FNAME, "[%s]")
		call pargstr (Memc[bracket[isect]])

		call strcat (Memc[extend], root, maxch)

	    } else if (ic > 0) {
		call strcpy (Memc[bracket[isect]], Memc[ident], ic-1)
		idtype = strdic (Memc[ident], Memc[ident], SZ_FNAME, idlist)

		if (idtype == 0) {
		    call sprintf (Memc[extend], SZ_FNAME, "[%s]")
		    call pargstr (Memc[bracket[isect]])

		    call strcat (Memc[extend], root, maxch)

		} else if (idtype == 1 && rowselect[1] == EOS) {
		    call strcpy (Memc[bracket[isect]+ic], rowselect, maxch)

		} else if (idtype == 2 && colselect[1] == EOS) {
		    call strcpy (Memc[bracket[isect]+ic], colselect, maxch)

		} else {
		    call sprintf (Memc[errmsg], SZ_LINE, badtype)
		    call pargstr (file)

		    call error (1, Memc[errmsg])
		}
	    }
	}

	call sfree (sp)
end

# NEXTBRAK -- Get next bracketed section from file name

bool procedure nextbrak (file, ic, section, maxch)

char	file[ARB]	# i: file name
int	ic		# u: index to char within name
char	section[ARB]	# o: section extracted from name
int	maxch		# i: maximum length of section
#--
int	jc, level
pointer	sp, errmsg

string	badsect  "No closing bracket (%s)"

begin
	if (file[ic] != '[') {
	    section[1] = EOS
	    return (false)
	} else {
	    level = 1
	    ic = ic + 1
	}

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	jc = 1
	while (level > 0 && file[ic] != EOS) {
	    if (file[ic] == '[' && file[ic-1] != '\\') {
		level = level + 1
	    } else if (file[ic] == ']' && file[ic-1] != '\\') {
		level = level - 1
	    }

	    if (level > 0 && jc <= maxch) {
		section[jc] = file[ic]
		jc = jc + 1
	    }

	    ic = ic + 1
	}

	section[jc] = EOS

	if (level > 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, badsect)
	    call pargstr (file)
	    call error (1, Memc[errmsg])
	}

	call sfree (sp)
	return (true)
end
