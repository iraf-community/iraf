# Table name template package, copied from the image template package imt.x.
#
#          tnt = tbnopen (template)
#          tnt = tbnopenp (clparam)
#                tbnclose (tnt)
#   nchars|EOF = tbnget (tnt, table, maxch)
#          len = tbnlen (tnt)
#                tbnrew (tnt)
#
# Phil Hodge, 8-Sep-1995  Copied from imt.x, names changed.

define	SZ_FNT		512
define	CH_DELIM	20B		# used to flag brackets

# TBNOPENP -- Open a table name template obtained as the string value of a CL
# parameter.

pointer	procedure tbnopenp (param)

char	param[ARB]		# i: CL parameter with string value template
#--
pointer	sp, template, tnt
pointer	tbnopen()
errchk	clgstr

begin
	call smark (sp)
	call salloc (template, SZ_LINE, TY_CHAR)

	call clgstr (param, Memc[template], SZ_LINE)
	tnt = tbnopen (Memc[template])

	call sfree (sp)
	return (tnt)
end

# TBNOPEN -- Open a table name template.

pointer	procedure tbnopen (template)

char	template[ARB]		# i: table name template
#--
int	sort, level, ip, ch
pointer	sp, listp, fnt, op
define	output {Memc[op]=$1;op=op+1}
int	fntopnb(), strlen()

begin
	call smark (sp)
	call salloc (fnt, strlen(template)*12/10 + SZ_FNT, TY_CHAR)

	# Sorting is disabled as input and output templates, derived from the
	# same database but with string editing used to modify the output list,
	# may be sorted differently as sorting is performed upon the edited
	# output list.

	sort = NO

	op = fnt
	for (ip=1;  template[ip] != EOS;  ip=ip+1) {
	    ch = template[ip]

	    if (ch == '[') {
		if (ip > 1 && template[ip-1] == '!') {
		    # ![ -- Pass a [ to FNT (character class notation).
		    Memc[op-1] = '['

		} else if (ip > 1 && template[ip-1] == '\\') {
		    # \[ -- The [ is part of the filename.  Pass it on as an
		    # escape sequence to get by the FNT.

		    output ('[')

		} else {
		    # [ -- Unescaped [.  This marks the beginning of an
		    # extension name.  Output `%%[...]%' and escape all
		    # pattern matching metacharacters until a comma template
		    # delimiter is encountered.  Note that a comma within []
		    # is not a template delimiter.

		    output ('%')
		    output ('%')
		    output (CH_DELIM)

		    level = 0
		    for (;  template[ip] != EOS;  ip=ip+1) {
			ch = template[ip]
			if (ch == ',') {		# ,
			    if (level <= 0)
				break			# exit loop
			    else {
				output ('\\')
				output (ch)
			    }
			} else if (ch == '[') {		# [
			    output ('\\')
			    output (ch)
			    level = level + 1
			} else if (ch == ']') {		# ]
			    output (ch)
			    level = level - 1
			} else if (ch == '*') {		# *
			    output ('\\')
			    output (ch)
			} else {			# normal chars
			    output (ch)
			}
		    }
		    output ('%')
		    ip = ip - 1
		}

	    } else if (ch == '@') {
		# List file reference.  Output the CH_DELIM code before the @
		# to prevent further translations on the table names
		# returned from the list file, e.g., "CH_DELIM // @listfile".

		output (CH_DELIM)
		output ('/')
		output ('/')
		output (ch)

	    } else {
		output (ch)
	    }
	}

	Memc[op] = EOS

	listp = fntopnb (Memc[fnt], sort)

	call sfree (sp)
	return (listp)
end


# TBNGET -- Get the next table name from the expanded list.
# EOF is returned if there are no more names.

int procedure tbnget (tnt, outstr, maxch)

pointer	tnt			# i: table template descriptor
char	outstr[ARB]		# o: output string
int	maxch			# i: max chars out
#--
int	nchars
pointer	sp, buf
int	fntgfnb(), tbn_mapname()
errchk	fntgfnb

begin
	call smark (sp)
	call salloc (buf, SZ_PATHNAME, TY_CHAR)

	if (fntgfnb (tnt, Memc[buf], SZ_PATHNAME) == EOF) {
	    outstr[1] = EOS
	    call sfree (sp)
	    return (EOF)
	}

	nchars = tbn_mapname (Memc[buf], outstr, maxch)
	call sfree (sp)
	return (nchars)
end


# TBNLEN -- Return the number of table names in the expanded list.

int procedure tbnlen (tnt)

pointer	tnt			# i: table name template descriptor
#--
int	fntlenb()

begin
	return (fntlenb (tnt))
end

    
# TBNREW -- Rewind the expanded table name list.

procedure tbnrew (tnt)

pointer	tnt			# i: table name template descriptor

begin
	call fntrewb (tnt)
end

    
# TBNCLOSE -- Close a table name template.

procedure tbnclose (tnt)

pointer	tnt			# io: table name template descriptor

begin
	call fntclsb (tnt)
end


# TBN_MAPNAME -- Translate the string returned by FNT into a table
# specification suitable for input to the table I/O routines.

int procedure tbn_mapname (fnt, outstr, maxch)

char	fnt[ARB]		# i: FNT string
char	outstr[ARB]		# o: output string
int	maxch

int	ip, op

begin
	op = 1
	for (ip=1;  fnt[ip] != EOS;  ip=ip+1) {
	    if (fnt[ip] == '[') {
		outstr[op] = '\\'
		op = op + 1
		outstr[op] = '['
		op = op + 1

	    } else if (fnt[ip] == CH_DELIM) {
		for (ip=ip+1;  fnt[ip] != EOS;  ip=ip+1) {
		    outstr[op] = fnt[ip]
		    op = op + 1
		    if (op > maxch)
			break
		}
		break

	    } else {
		outstr[op] = fnt[ip]
		op = op + 1
		if (op > maxch)
		    break
	    }
	}
	
	outstr[op] = EOS
	return (op - 1)
end
