include	<error.h>
include	<fset.h>
include	<evvexpr.h>
include	<ctype.h>
include	<ctotok.h>
include	<lexnum.h>
include	<time.h>
include	"astfunc.h"

define	SZ_KEY		32


# T_ASTCALC -- Calculator including astronomical routines.

procedure t_astcalc()

pointer	cmd			# command file
pointer	imlist			# image list
pointer	table			# data table
pointer	col			# column names
pointer	prompt			# prompt for STDIN
bool	verbose			# verbose output?

bool	eval
int	i, ip, sz_cmd, fdcmd, ncmds, nim, tm[LEN_TMSTRUCT]
long	pos
pointer	sp, image, key, expr, keys, exprs, ast
pointer	stopen(), immap()
int	open(), fscan(), fstati(), nowhite(), ctotok()
int	imtopenp(), imtlen(), imtgetim()
int	strlen(), stridxs()
long	note(), clktime(), lsttogmt()
bool	clgetb(), streq()
errchk	open, stopen, immap

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (prompt, SZ_FNAME, TY_CHAR)
	call salloc (col, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_KEY, TY_CHAR)

	sz_cmd = SZ_LINE
	call malloc (cmd, sz_cmd, TY_CHAR)
	call malloc (expr, sz_cmd, TY_CHAR)

	# Create ast_func data structure.
	call calloc (ast, LEN_AST, TY_STRUCT)

	# Open symbol table for storing results.
	AST_STP(ast) = stopen ("astcalc", 20, 1024, 20*SZ_KEY)

	# Open the command file and initialize
	ncmds = 0
	call clgstr ("commands", Memc[cmd], sz_cmd)
	if (nowhite (Memc[cmd], Memc[cmd], sz_cmd) > 0) {
	    fdcmd = open (Memc[cmd], READ_ONLY, TEXT_FILE)
	    Memc[prompt] = EOS
	} else {
	    fdcmd = STDIN
	    call clgstr ("prompt", Memc[prompt], SZ_FNAME)
	}
	eval = TRUE
	verbose = clgetb ("verbose")
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Open the image list and first image.
	imlist = imtopenp ("images")
	nim = imtlen (imlist)
	if (nim > 0) {
	    i = imtgetim (imlist, Memc[image], SZ_FNAME)
	    iferr (AST_IM(ast) = immap (Memc[image], READ_WRITE, 0))
		AST_IM(ast) = immap (Memc[image], READ_ONLY, 0)
	    call sprintf (Memc[expr], sz_cmd, "\"%s\"")
		call pargstr (Memc[image])
	    call ac_evaluate (ast, "$I", Memc[expr], eval, verbose)
	}

	# Open the table file.
	call clgstr ("table", Memc[table], SZ_FNAME)
	if (nowhite (Memc[table], Memc[table], SZ_FNAME) > 0) {
	    AST_TFD(ast) = open (Memc[table], READ_ONLY, TEXT_FILE)
	    pos = note (AST_TFD(ast))
	}

	# Set special operands.
	pos = clktime(0)
	call brktime (pos, tm)
	call sprintf (Memc[expr], sz_cmd, "\"%04d-%02d-%02d\"")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_MONTH(tm))
	call ac_evaluate (ast, "$D", Memc[expr], eval, verbose)
	call sprintf (Memc[expr], sz_cmd, "\"%02d:%02d:%02d\"")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	call ac_evaluate (ast, "$T", Memc[expr], eval, verbose)
	call brktime (lsttogmt(pos), tm)
	call sprintf (Memc[expr], sz_cmd, "\"%04d-%02d-%02d\"")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	call ac_evaluate (ast, "$GMD", Memc[expr], eval, verbose)
	call sprintf (Memc[expr], sz_cmd, "\"%02d:%02d:%02d\"")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	call ac_evaluate (ast, "$GMT", Memc[expr], eval, verbose)
	call sprintf (Memc[expr], sz_cmd, "\"%04d-%02d-%02dT%02d:%02d:%02d\"")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	call ac_evaluate (ast, "$GMDT", Memc[expr], eval, verbose)

	# Read and evaluate commands.
	repeat {
	    if (Memc[prompt] != EOS) {
		call printf (Memc[prompt])
		call flush (STDOUT)
	    }

	    # Get next command.  Allow for continuation lines.
	    if (fscan (fdcmd) == EOF)
		break
	    ip = 1
	    repeat {
		call gargstr (Memc[cmd+ip-1], sz_cmd)
		ip = strlen (Memc[cmd])
		if (Memc[cmd+ip-1] != '\\')
		    break
		if (fscan (fdcmd) == EOF)
		    break
		if (ip + SZ_LINE >= sz_cmd) {
		    sz_cmd = sz_cmd + SZ_LINE
		    call realloc (cmd, sz_cmd, TY_CHAR)
		    call realloc (expr, sz_cmd, TY_CHAR)
		}
	    }

	    # Eliminate comments, leading/trailing whitespace, and blank lines.
	    ip = stridxs ("#", Memc[cmd])
	    if (ip > 0)
		Memc[cmd+ip-1] = EOS
	    ip = strlen (Memc[cmd])
	    while (IS_WHITE(Memc[cmd+ip-1]))
		ip = ip - 1
	    Memc[cmd+ip] = EOS
	    ip = 1
	    while (IS_WHITE(Memc[cmd+ip-1]))
		ip = ip + 1
	    call strcpy (Memc[cmd+ip-1], Memc[cmd], sz_cmd)
	    if (Memc[cmd] == EOS)
		next

	    # Parse variable.
	    ip = 1
	    if (Memc[cmd+ip-1] == '$') {
	       ip = ip + 1
	       Memc[key] = '$'
	       i = ctotok (Memc[cmd], ip, Memc[key+1], SZ_KEY)
	    } else if (Memc[cmd+ip-1] == '@') {
		ip = ip + 2
		i = 0
                while (Memc[cmd+ip-1]!=Memc[cmd+1] && Memc[cmd+ip-1]!=EOS) {
		    Memc[key+i] = Memc[cmd+ip-1]
		    i = i + 1
                    ip = ip + 1
		}
		Memc[key+i] = EOS
		if (Memc[cmd+ip-1] != Memc[cmd+1]) {
		    call sprintf (Memc[expr], sz_cmd,
			"Syntax error `%s'")
			call pargstr (Memc[cmd])
		    call error (1, Memc[cmd])
		}
		ip = ip + 1
		i = TOK_IDENTIFIER
	    } else
	       i = ctotok (Memc[cmd], ip, Memc[key], SZ_KEY)

	    switch (i) {
	    case TOK_IDENTIFIER:
		while (IS_WHITE(Memc[cmd+ip-1]))
		    ip = ip + 1
		if (Memc[cmd+ip-1] == '=')
		    ip = ip + 1
		else {
		    ip = 1
		    Memc[key] = EOS
		}
	    default:
		ip = 1
		Memc[key] = EOS
	    }

	    # Parse expression.
	    while (IS_WHITE(Memc[cmd+ip-1]) || Memc[cmd+ip-1] == '=')
		ip = ip + 1
	    call strcpy (Memc[cmd+ip-1], Memc[expr], sz_cmd)

	    if (streq (Memc[expr], "quit"))
		break

	    # Save command.
	    if (ncmds == 0) {
		call malloc (keys, 100, TY_POINTER)
		call malloc (exprs, 100, TY_POINTER)
	    } else if (mod (ncmds, 100) == 0) {
		call realloc (keys, ncmds+100, TY_POINTER)
		call realloc (exprs, ncmds+100, TY_POINTER)
	    }

	    call salloc (Memi[keys+ncmds], SZ_KEY, TY_CHAR)
	    call strcpy (Memc[key], Memc[Memi[keys+ncmds]], SZ_KEY)
	    ip = strlen (Memc[expr])
	    call salloc (Memi[exprs+ncmds], ip, TY_CHAR)
	    call strcpy (Memc[expr], Memc[Memi[exprs+ncmds]], ip)

	    # Evaluate expression.
	    call ac_evaluate (ast, Memc[key], Memc[expr], eval, verbose)

	    ncmds = ncmds + 1
	}
	if (AST_IM(ast) != NULL)
	    call imunmap (AST_IM(ast))
	call close (fdcmd)

	# Repeat commands for other lines in the table and other images.
	# Note must be called in order to check for EOF in table.

	if (ncmds > 0 && (nim > 1 || AST_TFD(ast) != NULL)) {
	    repeat {
		eval = TRUE
		if (AST_TFD(ast) != NULL) {
		    if (pos == note (AST_TFD(ast)))
			break
		    if (fstati (AST_TFD(ast), F_EOF) == YES)
			break
		}
		if (nim > 0) {
		    if (imtgetim (imlist, Memc[image], SZ_FNAME) == EOF)
			break
		    iferr (AST_IM(ast) = immap (Memc[image], READ_WRITE, 0))
			AST_IM(ast) = immap (Memc[image], READ_ONLY, 0)
		    call sprintf (Memc[expr], sz_cmd, "\"%s\"")
			call pargstr (Memc[image])
		    call ac_evaluate (ast, "$I", Memc[expr], eval, verbose)
		}

		do i = 1, ncmds
		    call ac_evaluate (ast, Memc[Memi[keys+i-1]],
			Memc[Memi[exprs+i-1]], eval, verbose)

		if (AST_IM(ast) != NULL)
		    call imunmap (AST_IM(ast))
	    }
	}

	if (AST_TFD(ast) != NULL)
	    call close (AST_TFD(ast))
	if (AST_STP(ast) != NULL)
	    call stclose (AST_STP(ast))
	call mfree (cmd, TY_CHAR)
	call mfree (expr, TY_CHAR)
	call sfree (sp)
end


# AC_EVALUATE -- Evaluate the value of the key and add it to symbol table.

procedure ac_evaluate (ast, key, expr, eval, verbose)

pointer	ast			#I Data structure
char	key[ARB]		#I name of key to be edited
char	expr[ARB]		#I value expression
bool	eval			#U Conditional flag
bool	verbose			#I verbose output?

bool	streq()
pointer	o, sym, evvexpr(), stfind(), stenter()
int	locpr(), strncmp()
extern	ac_getop(), ast_func()
errchk	evvexpr

begin
	# Check conditional evaluation.
	if (streq (expr, "endif")) {
	    eval = TRUE
	    return
	} else if (streq (expr, "else")) {
	    eval = (!eval)
	    return
	}
	if (!eval)
	    return

	# Evaluate the expression.
	o = NULL
	if (expr[1] != EOS)
	    o = evvexpr (expr, locpr (ac_getop), ast, locpr (ast_func), ast, 0)
	if (o == NULL)
	    return

	# Set conditional evalution.
	if (key[1] == EOS) {
	    if (strncmp (expr, "if ", 3) == 0 || strncmp (expr, "if(", 3) == 0)
		eval = (O_VALI(o) != 0)

	} else {
	    # Print the verbose output.
	    if (verbose) {
		switch (O_TYPE(o)) {
		case TY_BOOL:
		    call printf ("  %s = %b\n")
			call pargstr (key)
			call pargi (O_VALI(o))
		case TY_CHAR:
		    call printf ("  %s = %s\n")
			call pargstr (key)
			call pargstr (O_VALC(o))
		case TY_INT:
		    call printf ("  %s = %d\n")
			call pargstr (key)
			call pargi (O_VALI(o))
		case TY_REAL:
		    call printf ("  %s = %g\n")
			call pargstr (key)
			call pargr (O_VALR(o))
		case TY_DOUBLE:
		    call printf ("  %s = %g\n")
			call pargstr (key)
			call pargd (O_VALD(o))
		}
	    }

	    sym = stfind (AST_STP(ast), key)
	    if (sym == NULL)
		sym = stenter (AST_STP(ast), key, SZ_LINE)
	    Memi[sym] = O_TYPE(o)
	    switch (O_TYPE(o)) {
	    case TY_BOOL:
		Memi[sym+2] = O_VALI(o)
	    case TY_CHAR:
		call strcpy (O_VALC(o), Memc[P2C(sym+2)], SZ_LINE)
	    case TY_INT:
		Memi[sym+2] = O_VALI(o)
	    case TY_REAL:
		Memd[P2D(sym+2)] = O_VALR(o)
	    case TY_DOUBLE:
		Memd[P2D(sym+2)] = O_VALD(o)
	    }
	}

	call mfree (o, TY_STRUCT)
end


# AC_GETOP -- Satisfy an operand request from EVEXPR.  In this context,
# operand names refer to entries in the symbol table.

procedure ac_getop (ast, operand, o)

pointer	ast			#I Data structure
char	operand[ARB]		#I name of operand to be returned
pointer	o			#O pointer to output operand

pointer	sym, stfind()

begin
	# Get operand value from symbol table.
	sym = stfind (AST_STP(ast), operand)
	if (sym == NULL)
	    call xvv_error1 ("variable `%s' not found", operand[1])

	switch (Memi[sym]) {
	case TY_BOOL, TY_SHORT, TY_INT, TY_LONG:
	    call xvv_initop (o, 0, TY_INT)
	    O_VALI(o) = Memi[sym+2]

	case TY_REAL, TY_DOUBLE, TY_COMPLEX:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = Memd[P2D(sym+2)]

	default:
	    call xvv_initop (o, SZ_LINE, TY_CHAR)
	    call strcpy (Memc[P2C(sym+2)], O_VALC(o), SZ_LINE)
	}
end
