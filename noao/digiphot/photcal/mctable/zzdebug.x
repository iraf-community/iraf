include	<ctotok.h>
include	<error.h>
include	<ctype.h>
include	"../lib/mctable.h"

task	mctable	= t_mctable

# Types
define	TYPES		"|char|short|int|long|real|double|complex|pointer|"
define	TYPE_CHAR	1
define	TYPE_SHORT	2
define	TYPE_INT	3
define	TYPE_LONG	4
define	TYPE_REAL	5
define	TYPE_DOUBLE	6
define	TYPE_COMPLEX	7
define	TYPE_POINTER	8

# File modes
define	MODES		"|write_only|read_write|new_file|temp_file|"
define	MODE_WRITE_ONLY	1
define	MODE_READ_WRITE	2
define	MODE_NEW_FILE	3
define	MODE_TEMP_FILE	4

# Commands
define	COMMANDS	"|allocate|free|copy|save|restore|\
			 |reset|shrink|clear|nrows|ncols|maxrows|maxcol|type|\
			 |getbuf|getrow|getrandom|putrandom|\
			 |rewind|getsequential|putsequential|\
			 |data|header|help|tables|time|quit|"
define	ALLOCATE	1
define	FREE		2
define	COPY		3
define	SAVE		4
define	RESTORE		5
# newline		6
define	RESET		7
define	SHRINK		8
define	CLEAR		9
define	NROWS		10
define	NCOLS		11
define	MAXROW		12
define	MAXCOL		13
define	TYPE		14
# newline		15
define	GETBUF		16
define	GETROW		17
define	GETRAN		18
define	PUTRAN		19
# newline		20
define	REWIND		21
define	GETSEQ		22
define	PUTSEQ		23
# newline		24
define	DATA		25
define	HEADER		26
define	HELP		27
define	TABLES		28
define	TIME		29
define	QUIT		30

# Max number of tables
define	MAX_TABLES	10


# MCTABLE -- Test MCTABLE package.

procedure t_mctable()

bool	timeit				# time commands ?
char	line[SZ_LINE]			# input line
char	key[SZ_FNAME]
char	cmd[SZ_LINE]			# command string
int	ncmd				# command number
int	i, ip
long	svtime[2]
pointer	table[MAX_TABLES]		# table pointers

int	getline()
int	strdic(), strlen()
int	strext()

begin
	# Clear table pointers
	call amovki (NULL, table, MAX_TABLES)

	# Do not time commands
	timeit = false

	# Print initial message
	call printf ("Multicolumn table test program\n")
	call printf ("Type `help` to get a list of commands\n\n")

	# Loop reading commands
	repeat {

	    # Get next command
	    call printf ("mctable> ")
	    call flush (STDOUT)
	    if (getline (STDIN, line) == EOF) {
		call printf ("\n")
		break
	    }
	    line[strlen (line)] = EOS

	    # Extract command from line
	    ip = 1
	    if (strext (line, ip, " ", YES, cmd, SZ_LINE) == 0)
		next
	    ncmd = strdic (cmd, cmd, SZ_LINE, COMMANDS)
	    if (ncmd == 0) {
		call eprintf ("Unknown or ambiguous command (%s)\n")
		    call pargstr (cmd)
		next
	    }

	    # Time command
	    if (timeit)
		call sys_mtime (svtime)

	    switch (ncmd) {
	    case ALLOCATE:
		call zzallocate (table, line, ip)

	    case FREE:
		call zzfree (table, line, ip)

	    case COPY:
		call zzcopy (table, line, ip)

	    case SAVE:
		call zzsave (table, line, ip)

	    case RESTORE:
		call zzrestore (table, line, ip)

	    case RESET:
		call zzreset (table, line, ip)

	    case SHRINK:
		call zzshrink (table, line, ip)

	    case CLEAR:
		call zzclear (table, line, ip)

	    case NROWS:
		call zznrows (table, line, ip)

	    case NCOLS:
	        call zzncols (table, line, ip)

	    case MAXROW:
	        call zzmaxrow (table, line, ip)

	    case MAXCOL:
	        call zzmaxcol (table, line, ip)

	    case TYPE:
		call zztype (table, line, ip)

	    case GETBUF:
		call zzgetbuf (table, line, ip)

	    case GETROW:
		call zzgetrow (table, line, ip)

	    case GETRAN:
		call zzgetran (table, line, ip)

	    case PUTRAN:
		call zzputran (table, line, ip)

	    case REWIND:
		call zzrewind (table, line, ip)
		
	    case GETSEQ:
		call zzgetseq (table, line, ip)

	    case PUTSEQ:
		call zzputseq (table, line, ip)

	    case DATA:
		call zzdata (table, line, ip)

	    case HEADER:
		call zzheader (table, line, ip)

	    case HELP:
		call zzhelp ()

	    case TABLES:
		call printf ("table..\n")
		do i = 1, MAX_TABLES {
		    call printf ("%d\t%d\n")
			call pargi (i)
			call pargi (table[i])
		}
		call flush (STDOUT)

	    case TIME:
		timeit = !timeit
		if (timeit)
		    call printf ("time..\n")
		else
		    call printf ("do not time..\n")
		call flush (STDOUT)

	    case QUIT:
		call printf ("quit..\n")
		call flush (STDOUT)
		return

	    default:
		call eprintf ("Syntax error\n")
	    }

	    if (timeit)
		call sys_ptime (STDOUT, key, svtime)
	}
end


# ZZALLOCATE -- Allocate table.

procedure zzallocate (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# character pointer

int	tnum, nrows, ncols, type

begin
	call printf ("allocate..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgtype  (line, ip, type)
	    call zzgeti   (line, ip, nrows)
	    call zzgeti   (line, ip, ncols)
	} then {
	    call erract (EA_WARN)
	    return
	}

	iferr (call mct_alloc (table[tnum], nrows, ncols, type)) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d) allocated, nrows = (%d), ncols = (%d), type = (%d)\n")
	    call pargi (tnum)
	    call pargi (nrows)
	    call pargi (ncols)
	    call pargi (type)

	call flush (STDOUT)
end


# ZZFREE -- Free table.

procedure zzfree (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum

begin
	call printf ("free..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (call mct_free (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d) freed\n")
	    call pargi (tnum)

	call flush (STDOUT)
end


# ZZCOPY -- Copy one table into another.

procedure zzcopy (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum1, tnum2

begin
	call printf ("copy..\n")
	iferr {
	    call zzgtable (line, ip, tnum1)
	    call zzgtable (line, ip, tnum2)
	} then {
	    call erract (EA_WARN)
	    return
	}
	iferr (call mct_copy (table[tnum1], table[tnum2])) {
	    call erract (EA_WARN)
	    return
	}
	call printf ("Table (%d) copied into table (%d)\n")
	    call pargi (tnum1)
	    call pargi (tnum2)

	call flush (STDOUT)
end


# ZZSAVE -- Save table into a file.

procedure zzsave (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

char	fname[SZ_FNAME]
int	fmode, tnum

begin
	call printf ("save..\n")
	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgstr   (line, ip, fname, SZ_FNAME)
	    call zzgmode  (line, ip, fmode)
	} then {
	    call erract (EA_WARN)
	    return
	}
	iferr (call mct_save (fname, fmode, table[tnum])) {
	    call erract (EA_WARN)
	    return
	}
	call printf ("Table (%d) saved into file (%s)\n")
	    call pargi (tnum)
	    call pargstr (fname)

	call flush (STDOUT)
end


# ZZRESTORE -- Restore table from a file.

procedure zzrestore (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

char	fname[SZ_FNAME]
int	tnum

begin
	call printf ("restore..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgstr   (line, ip, fname, SZ_FNAME)
	} then {
	    call erract (EA_WARN)
	    return
	}

	iferr (call mct_restore (fname, table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d) restored from file (%s)\n")
	    call pargi (tnum)
	    call pargstr (fname)

	call flush (STDOUT)
end


# ZZRESET -- Reset table counters.

procedure zzreset (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum

begin
	call printf ("reset..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (call mct_reset (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d) reseted\n")
	    call pargi (tnum)

	call flush (STDOUT)
end


# ZZSHRINK -- Shibk table.

procedure zzshrink (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum

begin
	call printf ("shrink..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (call mct_shrink (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d) shrunk\n")
	    call pargi (tnum)

	call flush (STDOUT)
end


# ZZCLEAR -- Clear table.

procedure zzclear (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	type, tnum

char	cval

short	sval

int	ival

long	lval

real	rval

double	dval

complex	xval

pointer	pval


begin
	call printf ("clear..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgtype  (line, ip, type)
	    switch (type) {

	    case TY_CHAR:
		call zzgetc (line, ip, cval)

	    case TY_SHORT:
		call zzgets (line, ip, sval)

	    case TY_INT:
		call zzgeti (line, ip, ival)

	    case TY_LONG:
		call zzgetl (line, ip, lval)

	    case TY_REAL:
		call zzgetr (line, ip, rval)

	    case TY_DOUBLE:
		call zzgetd (line, ip, dval)

	    case TY_COMPLEX:
		call zzgetx (line, ip, xval)

	    case TY_POINTER:
		call zzgetp (line, ip, pval)

	    }
	} then {
	    call erract (EA_WARN)
	    return
	}

	switch (type) {

	case TY_CHAR:
	    iferr (call mct_clearc (table[tnum], cval)) {
		call erract (EA_WARN)
		return
	    }

	case TY_SHORT:
	    iferr (call mct_clears (table[tnum], sval)) {
		call erract (EA_WARN)
		return
	    }

	case TY_INT:
	    iferr (call mct_cleari (table[tnum], ival)) {
		call erract (EA_WARN)
		return
	    }

	case TY_LONG:
	    iferr (call mct_clearl (table[tnum], lval)) {
		call erract (EA_WARN)
		return
	    }

	case TY_REAL:
	    iferr (call mct_clearr (table[tnum], rval)) {
		call erract (EA_WARN)
		return
	    }

	case TY_DOUBLE:
	    iferr (call mct_cleard (table[tnum], dval)) {
		call erract (EA_WARN)
		return
	    }

	case TY_COMPLEX:
	    iferr (call mct_clearx (table[tnum], xval)) {
		call erract (EA_WARN)
		return
	    }

	case TY_POINTER:
	    iferr (call mct_clearp (table[tnum], pval)) {
		call erract (EA_WARN)
		return
	    }

	}

	call printf ("Table (%d) cleared\n")
	    call pargi (tnum)

	call flush (STDOUT)
end


# ZZNROWS -- Get number of rows in table.

procedure zznrows (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, nrows

int	mct_nrows()

begin
	call printf ("nrows..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (nrows = mct_nrows (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), nrows = (%d)\n")
	    call pargi (tnum)
	    call pargi (nrows)

	call flush (STDOUT)
end


# ZZNCOLS -- Get number of columns in table.

procedure zzncols (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, ncols

int	mct_ncols()

begin
	call printf ("ncols..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (ncols = mct_ncols (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), ncols = (%d)\n")
	    call pargi (tnum)
	    call pargi (ncols)

	call flush (STDOUT)
end


# ZZMAXROW -- Get maximum number of rows in table.

procedure zzmaxrow (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, nrows

int	mct_maxrow()

begin
	call printf ("maxrow..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (nrows = mct_maxrow (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), maxrow = (%d)\n")
	    call pargi (tnum)
	    call pargi (nrows)

	call flush (STDOUT)
end


# ZZMAXCOL -- Get maximum number of columns in table.

procedure zzmaxcol (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, ncols

int	mct_maxcol()

begin
	call printf ("maxcol..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (ncols = mct_maxcol (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), maxcol = (%d)\n")
	    call pargi (tnum)
	    call pargi (ncols)

	call flush (STDOUT)
end


# ZZTYPE -- Get table type.

procedure zztype (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, type

int	mct_type()

begin
	call printf ("type..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (type = mct_type (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), type = (%d)\n")
	    call pargi (tnum)
	    call pargi (type)

	call flush (STDOUT)
end


# ZZGETBUF -- Get data buffer pointer.

procedure zzgetbuf (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum
pointer	pval

pointer	mct_getbuf()

begin
	call printf ("getbuf..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (pval = mct_getbuf (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), buffer = (%d)\n")
	    call pargi (tnum)
	    call pargi (pval)

	call flush (STDOUT)
end


# ZZGETROW -- Get row pointer.

procedure zzgetrow (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, row
pointer	pval

pointer	mct_getrow()

begin
	call printf ("getrow..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgeti   (line, ip, row)
	} then {
	    call erract (EA_WARN)
	    return
	}

	iferr (pval = mct_getrow (table[tnum], row)) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d), row buffer (%d) = (%d)\n")
	    call pargi (tnum)
	    call pargi (row)
	    call pargi (pval)

	call flush (STDOUT)
end


# ZZGETRAN -- Get value randomly from table.

procedure zzgetran (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	type, tnum, row, col

char	cval

short	sval

int	ival

long	lval

real	rval

double	dval

complex	xval

pointer	pval



char	mct_getc()

short	mct_gets()

int	mct_geti()

long	mct_getl()

real	mct_getr()

double	mct_getd()

complex	mct_getx()

pointer	mct_getp()


begin
	call printf ("getrandom..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgtype  (line, ip, type)
	    call zzgeti   (line, ip, row)
	    call zzgeti   (line, ip, col)
	} then {
	    call erract (EA_WARN)
	    return
	}

	switch (type) {

	case TY_CHAR:
	    iferr (cval = mct_getc (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargc (cval)

	case TY_SHORT:
	    iferr (sval = mct_gets (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargs (sval)

	case TY_INT:
	    iferr (ival = mct_geti (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargi (ival)

	case TY_LONG:
	    iferr (lval = mct_getl (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargl (lval)

	case TY_REAL:
	    iferr (rval = mct_getr (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargr (rval)

	case TY_DOUBLE:
	    iferr (dval = mct_getd (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargd (dval)

	case TY_COMPLEX:
	    iferr (xval = mct_getx (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargx (xval)

	case TY_POINTER:
	    iferr (pval = mct_getp (table[tnum], row, col)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
		"Table get (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargi (pval)

	}

	call flush (STDOUT)
end


# ZZPUTRAN -- Put value randomly in table.

procedure zzputran (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	type, tnum, row, col

char	cval

short	sval

int	ival

long	lval

real	rval

double	dval

complex	xval

pointer	pval


begin
	call printf ("putrandom..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgtype  (line, ip, type)
	    call zzgeti   (line, ip, row)
	    call zzgeti   (line, ip, col)
	    switch (type) {

	    case TY_CHAR:
		call zzgetc (line, ip, cval)

	    case TY_SHORT:
		call zzgets (line, ip, sval)

	    case TY_INT:
		call zzgeti (line, ip, ival)

	    case TY_LONG:
		call zzgetl (line, ip, lval)

	    case TY_REAL:
		call zzgetr (line, ip, rval)

	    case TY_DOUBLE:
		call zzgetd (line, ip, dval)

	    case TY_COMPLEX:
		call zzgetx (line, ip, xval)

	    case TY_POINTER:
		call zzgetp (line, ip, pval)

	    }
	} then {
	    call erract (EA_WARN)
	    return
	}

	switch (type) {

	case TY_CHAR:
	    iferr (call mct_putc (table[tnum], row, col, cval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargc (cval)

	case TY_SHORT:
	    iferr (call mct_puts (table[tnum], row, col, sval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargs (sval)

	case TY_INT:
	    iferr (call mct_puti (table[tnum], row, col, ival)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargi (ival)

	case TY_LONG:
	    iferr (call mct_putl (table[tnum], row, col, lval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargl (lval)

	case TY_REAL:
	    iferr (call mct_putr (table[tnum], row, col, rval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargr (rval)

	case TY_DOUBLE:
	    iferr (call mct_putd (table[tnum], row, col, dval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargd (dval)

	case TY_COMPLEX:
	    iferr (call mct_putx (table[tnum], row, col, xval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargx (xval)

	case TY_POINTER:
	    iferr (call mct_putp (table[tnum], row, col, pval)) {
		call erract (EA_WARN)
		return
	    }

	    call printf (
	    "Table put (%d), type = (%d), row = (%d), col = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (row)
		call pargi (col)
		call pargi (pval)

	}

	call flush (STDOUT)
end


# ZZREWIND -- Rewind table.

procedure zzrewind (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum

begin
	call printf ("rewind..\n")

	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}

	iferr (call mct_rew (table[tnum])) {
	    call erract (EA_WARN)
	    return
	}

	call printf ("Table (%d) rewound\n")
	    call pargi (tnum)

	call flush (STDOUT)
end


# ZZGETSEQ -- Get value sequentialy from table.

procedure zzgetseq (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	type, tnum, stat

char	cval

short	sval

int	ival

long	lval

real	rval

double	dval

complex	xval

pointer	pval



int	mct_sgetc()

int	mct_sgets()

int	mct_sgeti()

int	mct_sgetl()

int	mct_sgetr()

int	mct_sgetd()

int	mct_sgetx()

int	mct_sgetp()


begin
	call printf ("getsequential..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgtype  (line, ip, type)
	} then {
	    call erract (EA_WARN)
	    return
	}

	switch (type) {

	case TY_CHAR:
	    iferr (stat = mct_sgetc (table[tnum], cval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargc (cval)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_SHORT:
	    iferr (stat = mct_sgets (table[tnum], sval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargs (sval)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_INT:
	    iferr (stat = mct_sgeti (table[tnum], ival)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (ival)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_LONG:
	    iferr (stat = mct_sgetl (table[tnum], lval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargl (lval)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_REAL:
	    iferr (stat = mct_sgetr (table[tnum], rval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargr (rval)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_DOUBLE:
	    iferr (stat = mct_sgetd (table[tnum], dval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargd (dval)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_COMPLEX:
	    iferr (stat = mct_sgetx (table[tnum], xval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargx (xval)
		if (stat == EOF)
		    call pargstr ("EOF")
		else if (stat == OK)
		    call pargstr ("OK")
		else
		    call pargstr ("???")

	case TY_POINTER:
	    iferr (stat = mct_sgetp (table[tnum], pval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
		"Table getsequential (%d), type = (%s), value = (%g) (stat=%s)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (pval)

	}

	call flush (STDOUT)
end


# ZZPUTSEQ -- Put value sequentaly.

procedure zzputseq (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	type, tnum

char	cval

short	sval

int	ival

long	lval

real	rval

double	dval

complex	xval

pointer	pval


begin
	call printf ("putsequential..\n")

	iferr {
	    call zzgtable (line, ip, tnum)
	    call zzgtype  (line, ip, type)
	    switch (type) {

	    case TY_CHAR:
		call zzgetc (line, ip, cval)

	    case TY_SHORT:
		call zzgets (line, ip, sval)

	    case TY_INT:
		call zzgeti (line, ip, ival)

	    case TY_LONG:
		call zzgetl (line, ip, lval)

	    case TY_REAL:
		call zzgetr (line, ip, rval)

	    case TY_DOUBLE:
		call zzgetd (line, ip, dval)

	    case TY_COMPLEX:
		call zzgetx (line, ip, xval)

	    case TY_POINTER:
		call zzgetp (line, ip, pval)

	    }
	} then {
	    call erract (EA_WARN)
	    return
	}

	switch (type) {

	case TY_CHAR:
	    iferr (call mct_sputc (table[tnum], cval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargc (cval)

	case TY_SHORT:
	    iferr (call mct_sputs (table[tnum], sval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargs (sval)

	case TY_INT:
	    iferr (call mct_sputi (table[tnum], ival)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (ival)

	case TY_LONG:
	    iferr (call mct_sputl (table[tnum], lval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargl (lval)

	case TY_REAL:
	    iferr (call mct_sputr (table[tnum], rval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargr (rval)

	case TY_DOUBLE:
	    iferr (call mct_sputd (table[tnum], dval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargd (dval)

	case TY_COMPLEX:
	    iferr (call mct_sputx (table[tnum], xval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargx (xval)

	case TY_POINTER:
	    iferr (call mct_sputp (table[tnum], pval)) {
		call erract (EA_WARN)
		return
	    }
	    call printf (
	    "Table putsequential (%d), type = (%d), value = (%g)\n")
		call pargi (tnum)
		call pargi (type)
		call pargi (pval)

	}

	call flush (STDOUT)
end


# ZZDATA -- Display table data.

procedure zzdata (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum, type, offset
int	row, row1, row2, col

char	cval

short	sval

int	ival

long	lval

real	rval

double	dval

complex	xval

pointer	pval


begin
	call printf ("data..\n")
	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}
	iferr (call zzgeti  (line, ip, row1))
	    row1 = 1
	else
	    row1 = min (max (row1, 1), MCT_MAXROW (table[tnum]))
	iferr (call zzgeti  (line, ip, row2))
	    row2 = MCT_MAXROW (table[tnum])
	else
	    row2 = max (min (row2, MCT_MAXROW (table[tnum])), row1)

call eprintf ("table[%d]=%d\n")
call pargi (tnum)
call pargi (table[tnum])

	if (table[tnum] == NULL) {
	    call eprintf ("ERROR: Null table pointer\n")
	    return
	}
	if (MCT_DATA (table[tnum]) == NULL) {
	    call eprintf ("ERROR: Null data pointer\n")
	    return
	}

	type = MCT_TYPE (table[tnum])

	call printf ("(%d x %d) -> (%d:%d)\n")
	    call pargi (MCT_MAXROW (table[tnum]))
	    call pargi (MCT_MAXCOL (table[tnum]))
	    call pargi (row1)
	    call pargi (row2)

	do row = row1, row2 {

	    call printf ("%d\t")
		call pargi (row)

	    do col = 1, MCT_MAXCOL (table[tnum]) {

	        offset = MCT_MAXCOL (table[tnum]) * (row - 1) + col - 1

		switch (type) {

		case TY_CHAR:
		    cval = Memc[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargc (cval)

		case TY_SHORT:
		    sval = Mems[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargs (sval)

		case TY_INT:
		    ival = Memi[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargi (ival)

		case TY_LONG:
		    lval = Meml[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargl (lval)

		case TY_REAL:
		    rval = Memr[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargr (rval)

		case TY_DOUBLE:
		    dval = Memd[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargd (dval)

		case TY_COMPLEX:
		    xval = Memx[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargx (xval)

		case TY_POINTER:
		    pval = Memi[MCT_DATA (table[tnum]) + offset]
		    call printf (" %g")
			call pargi (pval)

		}
	    }

	    call printf ("\n")
	    call flush (STDOUT)
	}

	call flush (STDOUT)
end


# ZZHEADER -- Print table header.

procedure zzheader (table, line, ip)

pointer	table[MAX_TABLES]	# table array
char	line[ARB]		# command line
int	ip			# input character pointer

int	tnum

begin
	call printf ("header..\n")
	iferr (call zzgtable (line, ip, tnum)) {
	    call erract (EA_WARN)
	    return
	}
	if (table[tnum] == NULL) {
	    call eprintf ("ERROR: Null table pointer\n")
	    return
	}
	call printf ("magic     %d\n")
	    call pargi (MCT_MAGIC (table[tnum]))
	call printf ("type      %d\n")
	    call pargi (MCT_TYPE (table[tnum]))
	call printf ("incrow    %d\n")
	    call pargi (MCT_INCROWS (table[tnum]))
	call printf ("maxrow    %d\n")
	    call pargi (MCT_MAXROW (table[tnum]))
	call printf ("maxcol    %d\n")
	    call pargi (MCT_MAXCOL (table[tnum]))
	call printf ("nprows    %d\n")
	    call pargi (MCT_NPROWS (table[tnum]))
	call printf ("npcols    %d\n")
	    call pargi (MCT_NPCOLS (table[tnum]))
	call printf ("ngrows    %d\n")
	    call pargi (MCT_NGROWS (table[tnum]))
	call printf ("ngcols    %d\n")
	    call pargi (MCT_NGCOLS (table[tnum]))
	call printf ("data      %d\n")
	    call pargi (MCT_DATA (table[tnum]))

	call flush (STDOUT)
end


# ZZHELP -- Print command dictionary for interpreter.

procedure zzhelp ()

begin
	call printf ("help..\n")
	call printf ("allocate      <table> <type> <nrows> <ncols>\n")
	call printf ("free          <table>\n")
	call printf ("copy          <table> <table>\n\n")
	call printf ("save          <table> <fname> <fmode>\n")
	call printf ("restore       <table> <fname>\n\n")
	call printf ("reset         <table>\n")
	call printf ("rewind        <table>\n")
	call printf ("clear         <table> <value>\n\n")
	call printf ("maxrow        <table>\n")
	call printf ("maxcol        <table>\n")
	call printf ("nrows         <table>\n")
	call printf ("ncols         <table>\n")
	call printf ("type          <table>\n\n")
	call printf ("getbuf        <table>\n")
	call printf ("getrow        <table> <row>\n\n")
	call printf ("getrandom     <table> <type> <row> <col>\n")
	call printf ("putrandom     <table> <type> <row> <col> <value>\n\n")
	call printf ("putsequential <table> <type> <value>\n")
	call printf ("getsequential <table> <type>\n\n")
	call printf ("tables\n")
	call printf ("header        <table>\n")
	call printf ("data          <table> <row1> <row2>\n")
	call printf ("help\n")
	call printf ("quit\n")
	call printf ("\nwhere:\n")
	call printf ("	<table> = 1..10\n")
	call printf ("	<type>  = %s\n")
	    call pargstr (TYPES)
	call printf ("	<fmode> = %s\n")
	    call pargstr (MODES)

	call flush (STDOUT)
end


# ZZGTABLE -- Get table number and check its range.

procedure zzgtable (line, ip, num)

char	line[ARB]		# command line
int	ip			# input character pointer
int	num			# table number

errchk	zzgeti()

begin
	call zzgeti (line, ip, num)
	if (num < 1 || num > MAX_TABLES)
	    call error (0, "Table number out of range")
end


# ZZGTYPE -- Convert from string to integer type

procedure zzgtype (line, ip, type)

char	line[ARB]		# input line
int	ip			# input character pointer
int	type			# table type (output)

char	strval[SZ_LINE]
int	ntype

int	strdic()

begin
	call zzgstr (line, ip, strval, SZ_LINE)

	ntype = strdic (strval, strval, SZ_LINE, TYPES)

	switch (ntype) {
	case TYPE_CHAR:
	    type = TY_CHAR
	case TYPE_SHORT:
	    type = TY_SHORT
	case TYPE_INT:
	    type = TY_INT
	case TYPE_LONG:
	    type = TY_LONG
	case TYPE_REAL:
	    type = TY_REAL
	case TYPE_DOUBLE:
	    type = TY_DOUBLE
	case TYPE_COMPLEX:
	    type = TY_COMPLEX
	case TYPE_POINTER:
	    type = TY_POINTER
	default:
	    call error (0, "Unknown table type")
	}
end


# ZZGMODE -- Get mode string and convert it into a file mode.

procedure zzgmode (line, ip, mode)

char	line[ARB]		# input line
int	ip			# input character pointer
int	mode			# file mode (output)

char	strval[SZ_LINE]
int	nmode

int	strdic()

begin
	call zzgstr (line, ip, strval, SZ_LINE)

	nmode = strdic (strval, strval, SZ_LINE, MODES)

	switch (nmode) {
	case MODE_WRITE_ONLY:
	    mode = WRITE_ONLY
	case MODE_READ_WRITE:
	    mode = READ_WRITE
	case MODE_NEW_FILE:
	    mode = NEW_FILE
	case MODE_TEMP_FILE:
	    mode = TEMP_FILE
	default:
	    call error (0, "zzgmode: Unknown file mode")
	}
end



# ZZGET -- Get number from command line

procedure zzgetc (line, ip, cval)

char	line[ARB]		# command line
int	ip			# input character pointer
char	cval			# number

char	number[SZ_LINE]
int	op

int	cctoc()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	    if (cctoc (number, op, cval) == 0)
	        call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgets (line, ip, sval)

char	line[ARB]		# command line
int	ip			# input character pointer
short	sval			# number

char	number[SZ_LINE]
int	op

int	ctoi()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	    if (ctoi (number, op, int (sval)) == 0)
	        call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgeti (line, ip, ival)

char	line[ARB]		# command line
int	ip			# input character pointer
int	ival			# number

char	number[SZ_LINE]
int	op

int	ctoi()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	if (ctoi (number, op, ival) == 0)
	    call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgetl (line, ip, lval)

char	line[ARB]		# command line
int	ip			# input character pointer
long	lval			# number

char	number[SZ_LINE]
int	op

int	ctol()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	if (ctol (number, op, lval) == 0)
	    call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgetr (line, ip, rval)

char	line[ARB]		# command line
int	ip			# input character pointer
real	rval			# number

char	number[SZ_LINE]
int	op

int	ctor()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	if (ctor (number, op, rval) == 0)
	    call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgetd (line, ip, dval)

char	line[ARB]		# command line
int	ip			# input character pointer
double	dval			# number

char	number[SZ_LINE]
int	op

int	ctod()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	if (ctod (number, op, dval) == 0)
	    call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgetx (line, ip, xval)

char	line[ARB]		# command line
int	ip			# input character pointer
complex	xval			# number

char	number[SZ_LINE]
int	op

int	ctox()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	if (ctox (number, op, xval) == 0)
	    call error (0, "zzget: Impossible number conversion\n")
end

# ZZGET -- Get number from command line

procedure zzgetp (line, ip, pval)

char	line[ARB]		# command line
int	ip			# input character pointer
pointer	pval			# number

char	number[SZ_LINE]
int	op

int	ctoi()
int	strext()

begin
	if (strext (line, ip, " ", YES, number, SZ_LINE) == 0)
	    call error (0, "Missing numeric parameter\n")

	op = 1
	    if (ctoi (number, op, pval) == 0)
	        call error (0, "zzget: Impossible number conversion\n")
end



# ZZGSTR -- Get string from command line

procedure zzgstr (line, ip, strval, maxch)

char	line[ARB]		# command line
int	ip			# input character pointer
char	strval[maxch]		# output string
int	maxch			# max number of characters

int	strext()

begin
	if (strext (line, ip, " ", YES, strval, maxch) == 0)
	    call error (0, "Missing string parameter")
end
