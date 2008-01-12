# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"help.h"

.help hb_getnextblk
.nf ___________________________________________________________________________
HB_GETNEXTBLK -- Scan a file for the next help block, i.e., block of lines
delimited by ".help" and ".endhelp".  The syntax of a help block header is
as follows:

	.help[typestr] key1,key2,...,keyN section title

Whitespace is NOT permitted in the keyword list unless it is quoted.  Quotes
are optional for each of the three strings.  If the line ends with the
backslash character or a comma we assume that the header is continued on the
next line.  The typestr, denoting the type of help block, is optional.
If absent it defaults to "hlp".  System help blocks are of type "sys".

We keep track of the file line number when searching for a help block.  The
line number is updated in the HB structure.  For this number to be accurate,
the caller must see that it is initialized before we are called.
.endhelp ______________________________________________________________________

define	SZ_SBUF		(MAX_KEYS * SZ_KEY)
define	IS_KEYWCHAR	(IS_ALNUM($1) || $1 == '_' || $1 == '$')
define	exit_		90


int procedure hb_getnextblk (hb, ctrl)

pointer	hb
pointer	ctrl

bool	at_eof
char	key[SZ_KEY]
int	fd, n, ip, op, junk
pointer	sp, lbuf, sbuf, p
int	hb_getstr(), strmatch(), getline(), strlen()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (sbuf, SZ_SBUF, TY_CHAR)

	# Search forward for the next help block.
	fd = H_IN(ctrl)
	at_eof = true

	while (getline (fd, Memc[lbuf]) != EOF) {
	    HB_LINENO(hb) = HB_LINENO(hb) + 1
	    if (strmatch (Memc[lbuf], "^.{help}") > 0) {
		at_eof = false
		break
	    }
	}

	if (at_eof) {
	    call sfree (sp)
	    return (EOF)
	}

	# Initialize everything in case of an early exit.
	HB_TYPE(hb)	= TY_UNKNOWN
	HB_NKEYS(hb)	= 0
	HB_TYPESTR(hb)	= EOS
	HB_SECTION(hb)	= EOS
	HB_TITLE(hb)	= EOS

	# Decode help block into the HB structure.  First step is to
	# get the type suffix string, if any.

	ip = strlen (".help") + 1
	p = lbuf + ip - 1
	if (IS_ALPHA (Memc[p])) {
	    for (n=0;  IS_ALNUM (Memc[p+n]);  n=n+1)
		;
	    call strcpy (Memc[p], HB_TYPESTR(hb), min(n,SZ_TYPESTR))
	    if (     strmatch (HB_TYPESTR(hb), "{sys}") > 0)
		HB_TYPE(hb) = TY_SYS
	    else if (strmatch (HB_TYPESTR(hb), "{hlp}") > 0)
		HB_TYPE(hb) = TY_HLP
	    else
		HB_TYPE(hb) = TY_UNKNOWN
	} else {
	    HB_TYPE(hb) = TY_HLP
	    n = 0
	}
	ip = ip + n

	# Now get the keyword string, and break the keywords out into
	# the keyword list.

	if (hb_getstr (fd, Memc[lbuf], ip, Memc[sbuf], SZ_SBUF,
	HB_LINENO(hb)) == 0)
	    goto exit_

	p = sbuf
	while (IS_WHITE (Memc[p]) || Memc[p] == ',')
	    p = p + 1
	for (n=1;  n <= MAX_KEYS && Memc[p] != EOS;  n=n+1) {
	    for (op=1;  IS_KEYWCHAR (Memc[p]);  op=op+1) {
		key[op] = Memc[p]
		p = p + 1
	    }
	    key[op] = EOS
	    call strcpy (key, HB_KEY(hb,n), SZ_KEY)
	    while (IS_WHITE (Memc[p]) || Memc[p] == ',')
		p = p + 1
	}
	HB_NKEYS(hb) = n - 1
		
	# Fetch section label string and title string.

	if (hb_getstr (fd, Memc[lbuf], ip, HB_SECTION(hb), SZ_SECTION,
	HB_LINENO(hb)) == 0)
	    goto exit_
	junk = hb_getstr (fd, Memc[lbuf], ip, HB_TITLE(hb), SZ_TITLE,
	    HB_LINENO(hb))

exit_	call sfree (sp)
	return (HB_LINENO(hb))
end


# HB_GETSTR -- Fetch a string (optionally quoted) from the line buffer.
# Handle everything having to do with continuation.

int procedure hb_getstr (fd, lbuf, ip, outstr, maxch, lineno)

int	fd
char	lbuf[ARB]
int	ip
char	outstr[ARB]
int	maxch
int	lineno

char	ch
int	op, dstart
int	stridx(), getline()
string	delim " \t'\""

begin
	while (IS_WHITE (lbuf[ip]))
	    ip = ip + 1
	op = 1

	# If quoted string, only a quote can end the string.
	dstart = 1
	if (lbuf[ip] == '\'' || lbuf[ip] == '"') {
	    ip = ip + 1
	    dstart = 3
	}

	# Fetch the string.
	for (ch=lbuf[ip];  stridx (ch, delim[dstart]) == 0;  ch=lbuf[ip]) {
	    if (op >= maxch)
		break

	    switch (ch) {
	    case '\\', ',':
		if (lbuf[ip+1] == '\n' || lbuf[ip+1] == EOS) {
		    # Continue on next line.
		    if (getline (fd, lbuf) == EOF)
			lbuf[1] = EOS
		    else
			lineno = lineno + 1
		    ip = 1
		    if (ch == '\\')
			next
		} else
		    ip = ip + 1
		outstr[op] = ch
		op = op + 1

	    case '\n', EOS:
		if (delim[dstart] == '\'') {
		    call eprintf ("Missing right quote in helpfile, line %d\n")
			call pargi (lineno)
		}
		break

	    default:
		outstr[op] = ch
		op = op + 1
		ip = ip + 1
	    }
	}

	if (ch != EOS)
	    ip = ip + 1
	outstr[op] = EOS

	return (op - 1)
end
