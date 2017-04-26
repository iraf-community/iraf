include <ctotok.h>
include <evvexpr.h>
include <fset.h>
include <error.h>
include "import.h"

define	DEBUG	false
define	VDEBUG	false


.help fmtdb Augl93 "Format Database Interface"
.ih
DESCRIPTION
Format Database Procedures -- Routines for opening the format database given
in the task parameter, reading sequential and randome records within it, as
well as getting entried from within a selected record.

PROCEDURES
.nf
    PUBLIC PROCEDURES:

          fd = fdb_opendb ()
              fdb_closedb (fd)
        fmt = fdb_get_rec (fd, format)
       fmt = fdb_next_rec (fd)
   fmt = fdb_scan_records (fd, keyword, getop, opdata, fcn, fcndata)
                  fdbgstr (fmt, param, str, maxchar)
                fdb_close (fmt)

    PRIVATE PROCEDURES:

	       fdb_gfield (fd, fmt, key, val)
                fdb_gexpr (fd, fmt, expr, maxchars)
          fdb_strip_colon (in, out, maxch)
          fdb_strip_quote (in, out, maxch)

.fi

The FDB_OPENDB procedure returns a file descriptor to the database file
(named in the task parameters), and FDB_CLOSEDB will close the file.  When
searching for a specific format, the FDB_GET_REC procedure will return a
pointer to a symtab containing the database record.  The FDB_NEXT_REC
will return a symtab pointer to the next record in the database when reading
it sequentially.  The FDB_SCAN_RECS procedure can be used to scan the 
database, returning the symtab pointer to a record whose 'keyword' field eval-
uates as true.  The FDB_CLOSE procedure will free the symtab pointer returned
by the previous two routines.

Once a pointer is found for a database record the FDBGSTR procedure
can be used to return a value for an entry within that database record.
.ih
SEE ALSO
Source code
.endhelp


# Symbol table definitions.
define	LEN_INDEX	10		# Length of symtab index
define	LEN_STAB	(20*SZ_EXPR)	# Length of symtab
define	SZ_SBUF		512		# Size of symtab string buffer
define	SYMLEN		SZ_EXPR		# Length of symbol structure
define	SZ_FMTVAL	SZ_EXPR		# Size of format value string

# Symbol table structure
define	FMTVAL		Memc[P2C($1)]	# Format value string


# FDB_OPENDB -- Return a file descriptor to the format database.  The
# specified database may be a list of files in which case they will be
# concatenated to a single temporary file that is removed when the database 
# is closed.

int procedure fdb_opendb ()

int	fd, in, out
int	stat, nfiles
pointer	sp, fname, buf
pointer	dbfiles

int	open()
int	clpopni(), clplen(), clgfil()

errchk	open, clpopni

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	dbfiles = clpopni ("database")
	nfiles = clplen (dbfiles)
	if (nfiles == 0) {
	    call error (0, "No format database specified.")

	} else if (nfiles == 1) {
	    call clgstr ("database", Memc[fname], SZ_FNAME)
	    stat = clgfil (dbfiles, Memc[fname], SZ_FNAME)

	} else {
	    # The database parameter specified a list, concatenate the files
	    # to a temp file and open that instead.
	    call mktemp ("tmp$db", Memc[fname], SZ_FNAME)
	    out = open (Memc[fname], APPEND, TEXT_FILE)
	    while (clgfil (dbfiles, Memc[buf], SZ_FNAME) != EOF) {
		in = open (Memc[buf], READ_ONLY, TEXT_FILE)
		call fcopyo (in, out)
		call close (in)
	    }
	    call close (out)
	}

	# Open format database.
	fd = open (Memc[fname], READ_ONLY, TEXT_FILE)

	call sfree (sp)
	return (fd)
end


# FDB_CLOSEDB -- Close the format database.

procedure fdb_closedb (fd)

int	fd					#i file descriptor

pointer	sp, buf
int	strncmp()

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	# Get the database filename, if it's a temp file then the input
	# was probably and list and we need to clean up.
	call fstats (fd, F_FILENAME, Memc[buf], SZ_FNAME)
	call close (fd)
	if (strncmp (Memc[buf], "tmp$db", 6) == 0)
	    call delete (Memc[buf])

	call sfree (sp)
end


# FDB_GET_REC -- Get the requested format information in symbol table.

pointer procedure fdb_get_rec (fd, format)

int	fd					#i database file descriptor
char	format[ARB]				#i format name

pointer	fmt					#o format symbol table pointer
bool    found
char    colon
pointer sp, key, expr, sym

int     fscan(), stridx()
pointer stopen(), stenter()
bool    streq()

errchk  stopen, stenter, fscan

begin
	# Allocate local storage.
        call smark (sp)
        call salloc (key, SZ_FNAME, TY_CHAR)
        call salloc (expr, SZ_EXPR, TY_CHAR)

	# Find format entry.
	found = false
	colon = ':'
	while (fscan (fd) != EOF) {
	    call fdb_gfield (fd, NULL, Memc[key], Memc[expr])
            if (stridx (colon, Memc[key]) > 0) {
                call fdb_strip_colon (Memc[key], Memc[key], SZ_FNAME)
            } else if (Memc[key]=='#')  # skip comment lines
                next
	    if (streq (Memc[key], format)) {
	        found = true
	        break
	    }
	}
	if (!found) { 				# check if entry was found
	    call sfree (sp)
	    return (NULL)
	}

	# Create the symbol table.
        fmt = stopen (format, LEN_INDEX, LEN_STAB, SZ_SBUF)

        # Read the file and enter the parameters in the symbol table.
        sym = stenter (fmt, "format", SYMLEN)
        call strcpy (format, FMTVAL(sym), SZ_FMTVAL)
        while (fscan(fd) != EOF) {
            call fdb_gfield (fd, fmt, Memc[key], Memc[expr])
            if (stridx (colon, Memc[key]) > 0) {
                call fdb_strip_colon (Memc[key], Memc[expr], SZ_FNAME)
                call strcpy ("alias", Memc[key], SZ_FNAME)
            } else if (Memc[key] == '#' || Memc[key] == '') {
                next
            } else if (Memc[key] == EOS) {
                call sfree (sp)
                return (fmt)
            }
            sym = stenter (fmt, Memc[key], SYMLEN)
            call strcpy (Memc[expr], FMTVAL(sym), SZ_FMTVAL)
        }

        call close (fd)
        call sfree (sp)
        return (fmt)
end


# FDB_NEXT_REC -- Open format database and store the requested format
# information in symbol table.

pointer procedure fdb_next_rec (fd)

int	fd					#i input binary file descriptor

pointer	fmt					# Format symbol table pointer
char    colon
pointer sp, key, expr, sym, tmp

int     fscan(), stridx()
pointer stopen(), stenter()

errchk  stopen, stenter, fscan

begin
        # Allocate local storage.
        call smark (sp)
        call salloc (key, SZ_FNAME, TY_CHAR)
        call salloc (tmp, SZ_FNAME, TY_CHAR)
        call salloc (expr, SZ_FMTVAL, TY_CHAR)

	# Skip ahead top the beginning of the next record.
        colon = ':'
        while (fscan (fd) != EOF) {
            Memc[key] = EOS
            Memc[expr] = EOS
            call fdb_gfield (fd, NULL, Memc[key], Memc[expr])
            if (stridx (colon, Memc[key]) > 0) {
                call fdb_strip_colon (Memc[key], Memc[key], SZ_FNAME)
                break
            } else if (Memc[key] != '#' && Memc[key] != EOS)	# skip comment
                next

        }

        # The file will either be position at the BOF or at the end of the
        # previous record.  We will just read until the end of record and
        # return the pointer.

        # Create symbol table, but strip the ':' first.
        call fdb_strip_colon (Memc[key], Memc[tmp], SZ_FNAME)
        fmt = stopen (Memc[tmp], LEN_INDEX, LEN_STAB, SZ_SBUF)

        if (DEBUG) {call eprintf("next_rec: fmt='%s' ");call pargstr(Memc[tmp])}

        # Read the file and enter the parameters in the symbol table.
        sym = stenter (fmt, "format", SYMLEN)
        call strcpy (Memc[tmp], FMTVAL(sym), SZ_FMTVAL)
        while (fscan(fd) != EOF) {
            call fdb_gfield (fd, fmt, Memc[key], Memc[expr])
            if (stridx (colon, Memc[key]) > 0) {
                call fdb_strip_colon (Memc[key], Memc[expr], SZ_FNAME)
                call strcpy ("alias", Memc[key], SZ_FNAME)
            } else if (Memc[key] == '#' || Memc[key] == '') {
                next
            } else if (Memc[key] == EOS) {
                call sfree (sp)
                return (fmt)
            }
            sym = stenter (fmt, Memc[key], SYMLEN)
            call strcpy (Memc[expr], FMTVAL(sym), SZ_FMTVAL)
        }

        call sfree (sp)                         # shouldn't get here
        return (NULL)
end


# FDB_SCAN_RECORDS -- Scan the database for a record whose image_id evaluates
# as true.

pointer procedure fdb_scan_records (fd, keyword, getop, opdata, fcn, fcndata)

int	fd					#i input binary file descriptor
char	keyword[ARB]				#i keyword to be evaluated
int	getop					#i func to get an operand
int	opdata					#i data pointer for getop
int	fcn					#i user functions in evvexpr
int	fcndata					#i data pointer for fcn

pointer	sp, expr, fm
pointer	fmt, o

pointer	fdb_next_rec(), evvexpr()

errchk	evvexpr

begin
	call smark (sp)
	call salloc (expr, SZ_EXPR, TY_CHAR)
	call salloc (fm, SZ_FNAME, TY_CHAR)

        # Rewind the file descriptor.
	call seek (fd, BOF)

        if (DEBUG) { call eprintf("scan_rec: keyw='%s' ");call pargstr(keyword)}

	# Loop over all of the database records.
        repeat {
            fmt = fdb_next_rec (fd)
            if (fmt == NULL)
                break
            call fdbgstr (fmt, keyword, Memc[expr], SZ_EXPR)

            if (DEBUG) { 
		call eprintf(" expr='%s'\n"); call pargstr(Memc[expr]) 
		call flush (STDERR)
	    }

            # Evaluate keyword expression.
	    iferr {
	        o = evvexpr (Memc[expr], getop, opdata, fcn, fcndata, EV_RNGCHK)
	        if (O_TYPE(o) != TY_BOOL)
                    call error (0, "Expression must be a boolean")

	    } then {
		call erract (EA_WARN)
		break
	    }

            if (O_VALI(o) == YES) {		# see if we've found it
        	if (DEBUG) { 
            	    call fdbgstr (fmt, "format", Memc[fm], SZ_FNAME)
		    call eprintf(" format='%s'\n");call pargstr(Memc[fm])
		}
		call evvfree (o)
		call sfree (sp)
                return (fmt)
	    }

	    call evvfree (o)
            call fdb_close (fmt) 		# free fmt pointer
        }

	call sfree (sp)
        return (NULL)
end


# FDBCLOSE -- Close the format symbol table pointer.

procedure fdb_close (fmt)

pointer	fmt					#i Format symbol table pointer

begin
	if (fmt != NULL)
	    call stclose (fmt)
end


# FDBGSTR -- Get string valued format parameter.  We simply return the 
# expression, evaluation is up to the caller.

procedure fdbgstr (fmt, param, str, maxchar)

pointer	fmt					#i format symbol table pointer
char	param[ARB]				#i format parameter
char	str[ARB]				#o format parameter value
int	maxchar					#i maximum characters for string

pointer	sym, stfind()

begin
	call aclrc (str, maxchar)
	sym = stfind (fmt, param)
	if (sym == NULL)
	    call strcpy ("", str, maxchar)
	else
	    call strcpy (FMTVAL(sym), str, maxchar)
end


## END OF PUBLIC PROCEDURES ##


# FDB_GFIELD - Get field in the database record.  

procedure fdb_gfield (fd, fmt, keyword, expr)

int	fd					#i file descriptor
pointer fmt                                     #i format symtab pointer
char	keyword[ARB]				#o field keyword
char	expr[ARB]				#o field expression

pointer	sp, tmp

begin
	call smark (sp)
	call salloc (tmp, SZ_FNAME, TY_CHAR)

	call gargwrd (keyword, SZ_FNAME)
	call gargwrd (Memc[tmp], SZ_FNAME)

	if (keyword[1] == EOS) {
	    call sfree (sp)
	    return
	#} else if (Memc[tmp] == '#') {
	} else if (keyword[1] == '#') {
	    expr[1] = EOS
	} else if (Memc[tmp] != EOS)
	    call fdb_gexpr (fd, fmt, expr, SZ_EXPR)
	else
	    expr[1] = EOS

        if (VDEBUG && keyword[1] != '#' && keyword[1] != '') {
	    call eprintf("'%s'='%s'\n")
		call pargstr (keyword) ; call pargstr (expr)
	}

	call sfree (sp)
end


# FDB_GEXPR - Get an expression from the input stream.

procedure fdb_gexpr (fd, fmt, expr, maxchars)

int	fd					#i file descriptor
pointer fmt                                     #i format symtab pointer
char	expr[ARB]				#o returned expression
int	maxchars				#i maxchars

pointer sp, ntok, tok, tokval, next_tok, last_tok
pointer	sym
int	level, qlevel

int	fscan()
pointer	stfind()

define	dopar_	99

begin
        call smark (sp)
        call salloc (tok, SZ_FNAME, TY_CHAR)
        call salloc (ntok, SZ_FNAME, TY_CHAR)
 
	# Gather the expression.  For now we'll just eat everything up until
	# the closing parenthesis.
	call aclrc (expr, maxchars)

	# An expression is made up of a numeric or symbolic constant, a
	# quoted literal string, or some boolean or arithmetic operation.
	# The strategy is to get the first token and take action depending
	# on it's value and whether a following token completes the expr-
	# ession.   Expressions may break across newlines, literal strings
	# must be enclosed in double quotes.

	level = 0
	qlevel = 0
	last_tok = TOK_UNKNOWN
	repeat {
	    call gargtok (tokval, Memc[tok], SZ_EXPR)

	    switch (tokval) {
	    case TOK_NUMBER:
		call strcat (Memc[tok], expr, SZ_EXPR)
	    case TOK_STRING:
		# There are no operations on strings, but they might be passed
		# to a function as an argument, so check the level.  Oh yeah,
		# keep the double quotes in the string.
		call strcat ("\"", expr, SZ_EXPR)
		call strcat (Memc[tok], expr, SZ_EXPR)
		call strcat ("\"", expr, SZ_EXPR)
	    case TOK_PUNCTUATION:
		if (Memc[tok] == '(')
		    level = level + 1
		else if (Memc[tok] == ')')
		    level = level - 1
		call strcat (Memc[tok], expr, SZ_EXPR)
	    case TOK_OPERATOR:
		if (Memc[tok] == '"') {			# pass quoted strings
		    if (qlevel == 1)  
			qlevel = 0
		    else if (qlevel == 0)  
			qlevel = 1
		}
		if (Memc[tok] == '#' && qlevel == 0) {	# skip comments
		    if (fscan (fd) == EOF)
		        call eprintf ("WARNING: Unexpected EOF\n")
		    if (level == 0 && last_tok != TOK_OPERATOR)
		        break
		} else
		    call strcat (Memc[tok], expr, SZ_EXPR)
	    case TOK_NEWLINE:
		if (level != 0 || last_tok == TOK_OPERATOR) {
		    if (fscan (fd) == EOF)
		        call eprintf ("WARNING: Unexpected EOF\n")
		}
	    case TOK_IDENTIFIER:
		if (Memc[tok] == '$') {
                    call strcat (Memc[tok], expr, SZ_EXPR)
                } else if (fmt != NULL) {
                    sym = stfind (fmt, Memc[tok])
                    if (sym == NULL) {
			if (Memc[tok] == 'F') {
                    	    call strcat (Memc[tok], expr, SZ_EXPR)
			} else {
                	    call gargtok (next_tok, Memc[ntok], SZ_EXPR)
                	    if (Memc[ntok] == '(') {
			        # Copy to output buffer, it's a function name.
                                call strcat (Memc[tok], expr, SZ_EXPR)
                                call strcat (Memc[ntok], expr, SZ_EXPR)
                	        tokval = next_tok
                    	        level = level + 1
			        next
			    } else {
			        # It's an undefined database field.
			        call eprintf("Undefined database field '%s'.\n")
				    call pargstr (Memc[tok])
			    }
			}
                    } else
                        call strcat (FMTVAL(sym), expr, SZ_EXPR)
                } else {
                    call strcat (Memc[tok], expr, SZ_EXPR)
                }
                call gargtok (next_tok, Memc[tok], SZ_EXPR)
dopar_          if (Memc[tok] == '(')
                    level = level + 1
                else if (Memc[tok] == ')') {
                    level = level - 1
                    if (level == 0) {
                        call strcat (Memc[tok], expr, SZ_EXPR)
                        break
                    }
                }
                if (next_tok != TOK_NEWLINE)
                    call strcat (Memc[tok], expr, SZ_EXPR)
                tokval = next_tok
	    default:
		break
	    }

	    last_tok = tokval
	}

	# Check for an obvious error.
	if (level > 0)
	    call eprintf ("Missing right paren in expression: '%s'\n")
	else if (level < 0)
	    call eprintf ("Missing left paren in expression: '%s'\n")
	call pargstr (expr)

	call sfree (sp)
end


# FDB_STRIP_COLON -- Return the input string up to a ':' character.

procedure fdb_strip_colon (in, out, maxch)

char	in[ARB]					#i input string
char	out[ARB]				#o output string
int	maxch					#i max chars out

int	ip, op

begin
	op = 1
	do ip = 1, ARB {
	    if (in[ip] == ':' || op > maxch || in[ip] == EOS)
		break
	    out[op] = in[ip]
	    op = op + 1
	}
	out[op] = EOS
end


# FDB_STRIP_QUOTE -- Strip double quote chars from the string.

procedure fdb_strip_quote (in, out, maxch)

char	in[ARB]					#i input string
char	out[ARB]				#o output string
int	maxch					#i max chars out

int	ip, op

begin
	op = 1
	do ip = 1, ARB {
	    if (op > maxch || in[ip] == EOS)
		break
	    if (in[ip] != '"') {
	        out[op] = in[ip]
	        op = op + 1
	    }
	}
	out[op] = EOS
end
