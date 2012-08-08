# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<error.h>
include	<ctype.h>

task	sym	= t_sym

define	LOOKUP		1
define	ENTER		2
define	MARK		3
define	FREE		4
define	LISTALL		5
define	SQUEEZE		6
define	SAVE		7
define	RESTORE		8
define	INFO		9
define	SCANFILE	10
define	HELP		11


# SYM -- Test symbol entry and retrieval using the SYMTAB package.

procedure t_sym()

bool	timeit
pointer	stp, sym
long	svtime[2]
char	lbuf[SZ_LINE], key[SZ_FNAME]
int	ip, lp, stmt, marker, fd, indexlen, stablen, sbuflen, junk

bool	clgetb()
int	ctowrd(), strlen(), getline(), strmatch()
int	stpstr(), sthead(), stnext(), open(), clgeti()
pointer	stopen(), stfind(), stenter(), strestore()

begin
	indexlen = clgeti ("indexlen")
	stablen  = clgeti ("stablen")
	sbuflen  = clgeti ("sbuflen")
	timeit   = clgetb ("timeit")

	stp = stopen ("symtab.zzdebug test symbol table",
	    indexlen, stablen, sbuflen)

	repeat {
	    call printf ("* ")
	    call flush (STDOUT)
	    if (getline (STDIN, lbuf) == EOF) {
		call printf ("\n")
		break
	    } else if (strmatch (lbuf, "^bye") > 0)
		break

	    for (ip=1;  IS_WHITE(lbuf[ip]);  ip=ip+1)
		;

	    # Determine type of statement.
	    switch (lbuf[ip]) {
	    case '\n':
		next
	    case '=':
		ip = ip + 1
		stmt = LOOKUP
	    default:
		if (strmatch (lbuf[ip], "^.mark") > 0) {
		    stmt = MARK
		} else if (strmatch (lbuf[ip], "^.free") > 0) {
		    stmt = FREE
		} else if (strmatch (lbuf[ip], "^.list") > 0) {
		    stmt = LISTALL
		} else if (strmatch (lbuf[ip], "^.squeeze") > 0) {
		    stmt = SQUEEZE
		} else if (strmatch (lbuf[ip], "^.save") > 0) {
		    stmt = SAVE
		    ip = ip + 5
		} else if (strmatch (lbuf[ip], "^.restore") > 0) {
		    stmt = RESTORE
		    ip = ip + 8
		} else if (strmatch (lbuf[ip], "^.info") > 0) {
		    stmt = INFO
		    ip = ip + 5
		} else if (strmatch (lbuf[ip], "^.scanfile") > 0) {
		    stmt = SCANFILE
		    ip = ip + 9
		} else if (strmatch (lbuf[ip], "^.help") > 0) {
		    stmt = HELP
		    ip = ip + 5
		} else
		    stmt = ENTER
	    }

	    # Extract key name (or filename).
	    junk = ctowrd (lbuf, ip, key, SZ_FNAME)

	    if (timeit)
		call sys_mtime (svtime)

	    switch (stmt) {
	    case LOOKUP:
		# Lookup symbol in table.
		sym = stfind (stp, key)

		if (sym == NULL) {
		    call eprintf ("`%s' not found\n")
			call pargstr (key)
		    next
		}

		# Print keyword = value.
		call psym (stp, sym)

	    case ENTER:
		# Enter symbol in table.
		sym = stenter (stp, key, 1)

		# Get offset of value string.
		ip = strmatch (lbuf, "=")
		if (ip == 0)
		    ip = strlen(lbuf) + 1
		else {
		    while (IS_WHITE (lbuf[ip]))
			ip = ip + 1
		}

		# Step on the newline.
		for (lp=ip;  lbuf[lp] != EOS;  lp=lp+1)
		    if (lbuf[lp] == '\n') {
			lbuf[lp] = EOS
			break
		    }

		# Deposit value string in symbol table string buffer and save
		# offset in symstruct.

		Memi[sym] = -stpstr (stp, lbuf[ip], 0)

	    case MARK:
		call stmark (stp, marker)

	    case FREE:
		call stfree (stp, marker)

	    case LISTALL:
		for (sym=sthead(stp);  sym != NULL;  sym=stnext(stp,sym))
		    call psym (stp, sym)

	    case SQUEEZE:
		call stsqueeze (stp)

	    case SAVE:
		# In this case 'key' contains the savefile filename.
		iferr (call delete (key))
		    ;
		iferr (fd = open (key, NEW_FILE, BINARY_FILE)) {
		    call erract (EA_WARN)
		    next
		}

		call stsave (stp, fd)
		call close (fd)
		
	    case RESTORE:
		# In this case 'key' contains the savefile filename.

		iferr (fd = open (key, READ_ONLY, BINARY_FILE)) {
		    call erract (EA_WARN)
		    next
		}

		call stclose (stp)
		stp = strestore (fd)
		call close (fd)

	    case INFO:
		if (key[1] == 'v')
		    call stinfo (stp, STDOUT, YES)
		else
		    call stinfo (stp, STDOUT, NO)

	    case SCANFILE:
		call scanfile (key, stp)

	    case HELP:
		call zz_help (STDOUT)
	    default:
		call eprintf ("syntax error\n")
	    }

	    if (timeit)
		call sys_ptime (STDOUT, key, svtime)
	}

	call stclose (stp)
end


# SCANFILE -- Scan a text file, breaking the input up into a series of tokens.
# Place each new integer token in the symbol table.  If the token is already
# present in the table, increment its count field.

procedure scanfile (fname, stp)

char	fname[ARB]		# file to be scanned
pointer	stp			# symtab descriptor

char	lbuf[SZ_LINE], tokbuf[SZ_FNAME]
int	fd, ip, token
pointer	sym
int	open(), getline(), ctotok()
pointer	stenter(), stfind()
errchk	open, stenter

begin
	fd = open (fname, READ_ONLY, TEXT_FILE)

	while (getline (fd, lbuf) != EOF) {
	    ip = 1
	    repeat {
		token = ctotok (lbuf, ip, tokbuf, SZ_FNAME)
		if (token == TOK_IDENTIFIER) {
		    sym = stfind (stp, tokbuf)
		    if (sym == NULL) {
			sym = stenter (stp, tokbuf, 1)
			Memi[sym] = 1
		    } else
			Memi[sym] = Memi[sym] + 1
		}
	    } until (token == TOK_NEWLINE || token == TOK_EOS)
	}

	call close (fd)
end


# PSYM -- Print the name and value of a symbol in the form "key = value".
# There are two types of values, string and count.  A string operand is
# flagged as negative.

procedure psym (stp, sym)

pointer	stp			# symtab descriptor
pointer	sym			# pointer to symbol

int	val
pointer	vp
pointer	strefsbuf(), stname()

begin
	val = Memi[sym]
	if (val < 0) {
	    vp = strefsbuf (stp, -val)
	    call printf ("%s = %s\n")
		call pargstr (Memc[stname(stp,sym)])
		call pargstr (Memc[vp])
	} else {
	    call printf ("%s = %d\n")
		call pargstr (Memc[stname(stp,sym)])
		call pargi (val)
	}
end


# ZZ_HELP -- Print command dictionary for interpreter.

procedure zz_help (fd)

int 	fd

begin
	call fprintf (fd, ".mark                mark top of symbol table\n")
	call fprintf (fd, ".free                free back to last mark\n")
	call fprintf (fd, ".list                list all symbols in table\n")
	call fprintf (fd, ".squeeze             minimize storage\n")
	call fprintf (fd, ".save <fname>        save table in a file\n")
	call fprintf (fd, ".restore <fname>     restore table from a file\n")
	call fprintf (fd, ".info                print info on table\n")
	call fprintf (fd, ".scanfile <fname>    enter symbols from file\n")
	call fprintf (fd, "keyword = value      enter a symbol in table\n")
	call fprintf (fd, "= keyword            print value of named symbol\n")
	call fprintf (fd, "bye                  exit\n")
	call flush (fd)
end
