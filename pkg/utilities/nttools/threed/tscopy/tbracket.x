#* HISTORY *
#* B.Simon	07-Nov-94	original

# TBRACKET -- Break a table name into bracket delimeted substrings

procedure tbracket (table, root, rowselect, colselect, maxch)

char	table[ARB]		# i: Table name
char	root[ARB]		# o: Name minus bracketed sections
char	rowselect[ARB]		# o: Row selector section
char	colselect[ARB]		# o: Column selector section
int	maxch			# i: Maximum length of output strings
#--
bool	found
char	eq
int	ic, nc

data	eq  / '=' /

errchk	tsplitter
bool	tsplitter()
int	stridx()

begin
	# Search for the first unescaped bracket

	for (ic = 1; table[ic] != EOS; ic = ic + 1) {
	    if (table[ic] == '\\' && table[ic+1] != EOS) {
		ic = ic + 1
	    } else if (table[ic] == '['){
		break
	    }
	}

	nc = min (ic-1, maxch)
	call strcpy (table, root, nc)

	# Get bracketed sections from table name. If there is only 
	# a single section, disambiguate by looking for an equals 
	# sign, which indicates a row selector.

	found = tsplitter (table, ic, rowselect, maxch)

	if (! tsplitter (table, ic, colselect, maxch)) {
	    if (stridx (eq, rowselect) == 0) {
		call strcpy (rowselect, colselect, maxch)
		rowselect[1] = EOS
	    }
	}

end

# TSPLITTER -- Splits table filename into sections

bool procedure tsplitter (table, ic, section, maxch)

char	table[ARB]	# i: table name
int	ic		# u: index to char within name
char	section[ARB]	# o: section extracted from name
int	maxch		# i: maximum length of section
#--
int	jc, level
pointer	sp, errmsg

string	badsect  "No closing bracket (%s)"

begin
	if (table[ic] != '[') {
	    section[1] = EOS
	    return (false)
	} else {
	    level = 1
	    ic = ic + 1
	}

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	jc = 1
	while (level > 0 && table[ic] != EOS) {
	    if (table[ic] == '[' && table[ic-1] != '\\') {
		level = level + 1
	    } else if (table[ic] == ']' && table[ic-1] != '\\') {
		level = level - 1
	    }

	    if (level > 0 && jc <= maxch) {
		section[jc] = table[ic]
		jc = jc + 1
	    }

	    ic = ic + 1
	}

	section[jc] = EOS

	if (level > 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, badsect)
	    call pargstr (table)
	    call error (1, Memc[errmsg])
	}

	call sfree (sp)
	return (true)
end
