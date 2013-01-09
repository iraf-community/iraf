include	<ctype.h>
include <ctotok.h>
include <tbset.h>

# tbnparse -- extract different portions of table name
# For a table in a QPOE file, the user may give a table name such as
# stuff.qp[abc], where "stuff.qp" is the file name, and "abc" is the name
# of the QPOE parameter containing the table.
#
# For a FITS file, the user may explicitly specify the extension number,
# or the extension may be given by extension name (the value of the EXTNAME
# keyword) and/or version number (EXTVER).  If the extension number was
# given explicitly, neither extension name nor version may be given, and
# they will be set to "" and -1 respectively on output.  The numbering
# convention is that the first extension after the primary HDU is number
# one.  (This differs from the convention in the FITSIO interface.)
# If the extension was specified by name and/or version rather than number,
# the HDU number will be set to -1.
# The overwrite flag is independent of the other information returned;
# +1 means yes, 0 means no, and -1 means overwrite was not specified.
#
# Phil Hodge,  2-Feb-1996  Subroutine created.
# Phil Hodge, 30-Sep-1997  Add rowselect, colselect, maxchsel to calling seq.
# Phil Hodge, 15-Jun-1998  For STDIN, STDOUT, or text file, set type to text.
# Phil Hodge, 12-Apr-1999  Remove table type from calling sequence.

int procedure tbnparse (inputname, fname, extname, brackets, maxch,
		extver, hdu, overwrite,
		rowselect, colselect, maxchsel)

char	inputname[ARB]	# i: name as specified by user
char	fname[ARB]	# o: name of file containing table
char	extname[ARB]	# o: extension name, or null if none
char	brackets[ARB]	# o: expression in brackets, or null if none
int	maxch		# i: size of fname and extname strings
int	extver		# o: extension version number, if specified
int	hdu		# o: HDU number for FITS file, if specified
int	overwrite	# o: YES, NO, or YES-1 --> not specified
char	rowselect[ARB]	# o: row selector string
char	colselect[ARB]	# o: column selector string
int	maxchsel	# i: max size of rowselect and colselect
#--
int	level, ip, op, ch	# for extracting file name
pointer sp
pointer tablename	# input name without selectors
pointer expr		# scratch
int	nchar		# number of non-blank characters in inputname
int	last_char	# last character in file name before [ or \[
int	len_name	# length of table name, ignoring trailing whitespace
int	nbrackets	# number of bracket pairs at end of file name
int	localmax	# size of local string
bool	done
int	nowhite()
int	strlen()
errchk	rdselect, tbnexpr

begin
	localmax = max (SZ_LINE, maxch, 2*maxchsel)

	# Check for blank input name.
	call smark (sp)
	call salloc (tablename, localmax, TY_CHAR)
	call salloc (expr, localmax, TY_CHAR)
	nchar = nowhite (inputname, Memc[expr], localmax)
	if (nchar < 1) {
	    fname[1] = EOS
	    extname[1] = EOS
	    brackets[1] = EOS
	    rowselect[1] = EOS
	    colselect[1] = EOS
	    extver = -1
	    hdu = -1
	    overwrite = YES - 1
	    call sfree (sp)
	    return (0)
	}

	# Extract row and column selector strings, if any.
	call rdselect (inputname, Memc[tablename],
			rowselect, colselect, maxchsel)

	# Work backwards to find the first [ following the file name.
	ip = strlen (Memc[tablename])
	while (IS_WHITE(Memc[tablename+ip-1]))
	    ip = ip - 1				# ignore trailing whitespace
	len_name = ip
	last_char = 0
	nbrackets = 0
	done = false
	while (!done) {

	    if (Memc[tablename+ip-1] == ']') {

		nbrackets = nbrackets + 1
		while (Memc[tablename+ip-1] != '[') {
		    ip = ip - 1
		    if (ip < 1)
			call error (1, "tbnparse:  unmatched ] in file name")
		    if (Memc[tablename+ip-1] == ']')
			call error (1, "tbnparse:  nested brackets not allowed")
		}
		ip = ip - 1			# back up over the [

		if (ip < 1)
		    done = true
		else if (Memc[tablename+ip-1] == '\\')
		    ip = ip - 1

		if (ip < 1)
		    done = true

	    } else {

		last_char = ip
		done = true
	    }
	}

	if (last_char > maxch)
	    call error (1, "tbnparse:  file name is too long")
	else if (last_char < 1)
	    call error (1, "tbnparse:  no file name specified")

	# Extract root name.
	level = 0
	op = 1
	do ip = 1, last_char {
	    if (Memc[tablename+ip-1] == '[')
		level = level + 1
	    else if (Memc[tablename+ip-1] == ']')
		level = level - 1
	    fname[op] = Memc[tablename+ip-1]
	    op = op + 1
	}
	fname[op] = EOS
	if (level != 0)
	    call error (1, "tbnparse:  unmatched bracket in file name")

	# Copy bracketed expression, if any, to output.
	brackets[1] = EOS
	call strcpy (inputname[last_char+1], brackets, maxch)

	# Extract the expression in brackets, if any.  Exclude the brackets.

	level = 0
	ip = last_char + 1
	if (Memc[tablename+ip-1] == '[')
	    ip = ip + 1				# ignore initial open bracket
	op = 1
	for (ch=Memc[tablename+ip-1];  ch != EOS;  ch=Memc[tablename+ip-1]) {
	    if (op > maxch)
		call error (1, "tbnparse:  name in brackets is too long")

	    if (ch == '"') {
		if (level == 0)
		    level = 1			# beginning of a string
		else
		    level = 0			# ending of a string
	    }

	    # Delete or modify characters under certain conditions.
	    if (ch == '\\' && level == 0) {
		;
	    } else if (ch == ';' && level == 0) {
		Memc[expr+op-1] = ','		# ; --> ,
		op = op + 1
	    } else if (ch == ']' && ip >= len_name) {
		;				# ignore final close bracket
	    } else if (ch == ']' && level == 0) {
		# Replace multiple brackets with a comma.
		if (Memc[tablename+ip] == '[') {
		    Memc[expr+op-1] = ','
		    op = op + 1
		    ip = ip + 1
		} else if (Memc[tablename+ip] == '\\') {
		    if (Memc[tablename+ip+1] == '[') {
			Memc[expr+op-1] = ','
			op = op + 1
			ip = ip + 2
		    } else {		# but ]\ is probably a syntax error
			Memc[expr+op-1] = ch
			op = op + 1
		    }
		} else {
		    Memc[expr+op-1] = ch
		    op = op + 1
		}
	    } else {
		Memc[expr+op-1] = ch
		op = op + 1
	    }
	    ip = ip + 1
	}
	if (level > 0)
	    call error (1, "tbnparse:  unmatched quote in table name")

	Memc[expr+op-1] = EOS

	# Now replace commas with spaces.  We do this so we can use
	# ctowrd instead of ctotok to get extname values.
	for (ip = 1;  Memc[expr+ip-1] != EOS;  ip = ip + 1) {
	     if (Memc[expr+ip-1] == ',')
		 Memc[expr+ip-1] = ' '
	}

	# Parse the expression we just extracted into Memc[expr].
	call tbnexpr (Memc[expr], extname, maxch, extver, hdu, overwrite)

	call sfree (sp)
	return (nchar)
end

define	TBN_EXTENSION	1
define	TBN_EXTNAME	2
define	TBN_EXTVER	3
define	TBN_OVERWRITE	4

# tbnexpr -- extract information from an expression appended to a table name

procedure tbnexpr (expr, extname, maxch, extver, hdu, overwrite)

char	expr[ARB]	# i: expression extracted from bracket(s)
char	extname[ARB]	# o: extension name
int	maxch		# i: max size of extname string
int	extver		# o: extension version number
int	hdu		# o: HDU number for FITS file
int	overwrite	# o: YES, NO, or YES-1 --> not specified
#--
pointer sp
pointer token		# scratch for the value of the token
pointer word		# scratch
int	t_class		# token type
int	option		# index returned by strdic
int	nchar, ip, ip_last, ip2
int	itemp
bool	done
int	ctotok(), ctoi(), ctowrd(), strdic()
bool	streq()

begin
	extname[1] = EOS			# initial values
	extver = -1
	hdu = -1
	overwrite = YES - 1

	if (expr[1] == EOS)
	    return

	call smark (sp)
	call salloc (token, SZ_LINE, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)

	ip = 1
	done = false
	while (!done) {

	    ip_last = ip				# save previous ip
	    t_class = ctotok (expr, ip, Memc[token], SZ_LINE)

	    if (t_class == TOK_EOS || t_class == TOK_NEWLINE) {
		done = true

	    } else if (t_class == TOK_CHARCON) {
		call error (1, "unrecognized character in table name")

	    } else if (t_class == TOK_NUMBER) {

		# Is this actually an EXTNAME that begins with a number?
		if (IS_ALPHA(expr[ip])) {
		    # Use ctowrd because ctotok would extract only the
		    # integer portion of e.g. 123xyz.
		    ip = ip_last			# back up
		    nchar = ctowrd (expr, ip, extname, maxch)
		} else {
		    ip2 = 1
		    if (ctoi (Memc[token], ip2, itemp) < 1)
			call error (1, "tbnparse:  can't read HDU number")

		    # If we already have an EXTNAME, assume this number is
		    # an EXTVER; otherwise, assume it's the extension number.
		    if (extname[1] != EOS) {
			extver = itemp
		    } else if (extver > 0) {
			call error (1,
				"tbnparse:  ambiguous number in table name")
		    } else {
			hdu = itemp
			if (hdu < 0)
			    call error (1,
				"tbnparse:  extension number can't be negative")
		    }
		}

	    } else if (t_class == TOK_IDENTIFIER) {

		call strcpy (Memc[token], Memc[word], SZ_LINE)
		call strlwr (Memc[word])
		option = strdic (Memc[word], Memc[word], SZ_LINE,
			"|extension|extname|extver|overwrite")

		if (option == TBN_EXTENSION) {

		    if (hdu > 0)
			call error (1, "can't specify extension number twice")

		    t_class = ctotok (expr, ip, Memc[token], SZ_LINE)
		    if (Memc[token] != '=')
			call error (1, "table name syntax:  [extension=<n>]")
		    if (ctoi (expr, ip, hdu) < 1)
			call error (1, "tbnparse:  can't read extension number")
		    if (hdu <= 0)
			call error (1, "extension number must be positive")

		} else if (option == TBN_EXTNAME) {

		    t_class = ctotok (expr, ip, Memc[token], SZ_LINE)
		    if (Memc[token] != '=')
			call error (1, "table name syntax:  [extname=<n>]")
		    if (ctowrd (expr, ip, extname, maxch) < 1)
			call error (1, "tbnparse:  missing EXTNAME string")

		} else if (option == TBN_EXTVER) {

		    t_class = ctotok (expr, ip, Memc[token], SZ_LINE)
		    if (Memc[token] != '=')
			call error (1, "table name syntax:  [extver=<n>]")
		    if (ctoi (expr, ip, extver) < 1)
			call error (1, "tbnparse:  invalid EXTVER number")

		} else if (option == TBN_OVERWRITE) {

		    # Get the '=' sign, if there is one, else get + or -.
		    ip2 = ip			# save, so we can back up
		    t_class = ctotok (expr, ip, Memc[token], SZ_LINE)

		    if (t_class == TOK_EOS || t_class == TOK_NEWLINE) {
			overwrite = YES
			done = true
		    } else if (t_class == TOK_PUNCTUATION) {
			# "overwrite", i.e. without a value
			overwrite = YES
		    } else if (streq (Memc[token], "]")) {
			overwrite = YES
		    } else {
			if (Memc[token] == '=')
			    t_class = ctotok (expr, ip, Memc[token], SZ_LINE)
			call strcpy (Memc[token], Memc[word], SZ_LINE)
			call strlwr (Memc[word])
			option = strdic (Memc[word], Memc[word], SZ_LINE,
				"|+|yes|true|-|no|false")
			if (option >= 1 && option <= 3) {
			    overwrite = YES
			} else if (option >= 4 && option <= 6) {
			    overwrite = NO
			} else {
			    overwrite = YES
			    ip = ip2			# back up
			}
		    }

		} else if (option == 0) {

		    # Could be either ambiguous or EXTNAME.
		    call strcpy (Memc[token], Memc[word], SZ_LINE)
		    call strlwr (Memc[word])
		    if (streq (Memc[word], "e") ||
			streq (Memc[word], "ex") ||
			streq (Memc[word], "ext")) {
			call strcpy ("`", Memc[word], SZ_LINE)
			call strcat (expr, Memc[word], SZ_LINE)
			call strcat ("' is ambiguous", Memc[word], SZ_LINE)
			call error (1, Memc[word])
		    } else {
			# Take original value, not lower case copy.
			call strcpy (Memc[token], extname, maxch)
		    }
		}

	    } else if (t_class == TOK_STRING) {

		call strcpy (Memc[token], extname, maxch)

	    } else if (t_class == TOK_PUNCTUATION) {
		;
	    } else {
		call strcpy ("syntax error:  `[", Memc[word], SZ_LINE)
		call strcat (expr, Memc[word], SZ_LINE)
		call strcat ("]'", Memc[word], SZ_LINE)
		call error (1, Memc[word])
	    }
	}

	if (hdu > 0 && overwrite != YES &&
		(extname[1] != EOS || extver > 0))
	    call error (1,
		"can't give extension number and EXTNAME or EXTVER")
end
