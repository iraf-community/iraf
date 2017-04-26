include	<error.h>
include	<fset.h>
include	<evvexpr.h>
include	<ctype.h>
include	<ctotok.h>
include	<lexnum.h>
include	<imset.h>
include	<time.h>
include	"astfunc.h"

define	SZ_KEY		8
define	SPECIAL	"|if|else|endif|print|printf|quit|"


# T_ASTHEDIT -- Edit/calculator keywords in an image header including
# astronomical routines.

procedure t_asthedit()

int	imlist			# list of images
pointer	cmd			# command file
pointer	table			# data table
pointer	col			# column names
pointer	prompt			# prompt for STDIN
bool	update			# update image header?
bool	verbose			# verbose output?
bool	oldstyle		# use old style without equals sign?

bool	eval
int	i, ip, sz_cmd, fdcmd, ncmds, acmode, nim, tm[LEN_TMSTRUCT]
long	pos
pointer	sp, image, key, expr, keys, exprs, ast
pointer	stopen()
int	imtopenp(), imtlen(), imtgetim(), immap()
int	open(), fscan(), fstati(), nowhite(), ctowrd(), ctotok()
int	strlen(), stridxs(), strdic()
bool	clgetb(), streq()
long	note(), clktime(), lsttogmt()
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
	call malloc (expr, sz_cmd,  TY_CHAR)

	# Create ast_func data structure.
	call calloc (ast, LEN_AST, TY_STRUCT)

	# Open symbol table for storing results.
	AST_STP(ast) = stopen ("astcalc", 20, 1024, 20*SZ_KEY)

	# Open the image list and first image.
	imlist = imtopenp ("images")
	nim = imtlen (imlist)
	if (imtgetim (imlist, Memc[image], SZ_FNAME) == EOF) {
	    call mktemp ("tmp$iraf", Memc[image], SZ_FNAME)
	    AST_IM(ast) = immap (Memc[image], NEW_IMAGE, 0)
	} else {
	    update = clgetb ("update")
	    if (update)
		acmode = READ_WRITE
	    else
		acmode = READ_ONLY
	    repeat {
		ifnoerr (AST_IM(ast) = immap (Memc[image], acmode, 0))
		    break
		call erract (EA_WARN)
		if (imtgetim (imlist, Memc[image], SZ_FNAME) == EOF) {
		    call sfree (sp)
		    return
		}
	    }
	}

	# Open the command file.
	ncmds = 0
	call clgstr ("commands", Memc[cmd], SZ_LINE)
	if (nowhite (Memc[cmd], Memc[cmd], SZ_FNAME) > 0) {
	    fdcmd = open (Memc[cmd], READ_ONLY, TEXT_FILE)
	    Memc[prompt] = EOS
	} else {
	    fdcmd = STDIN
	    call clgstr ("prompt", Memc[prompt], SZ_FNAME)
	}
	oldstyle = clgetb ("oldstyle")

	# Set conditional flag and verbose and print output.
	eval = TRUE
	call fseti (STDOUT, F_FLUSHNL, YES)
	verbose = clgetb ("verbose")
	if (verbose) {
	    call printf ("%s:\n")
		call pargstr (Memc[image])
	}

	# Set special operands.
	call sprintf (Memc[expr], sz_cmd, "\"%s\"")
	    call pargstr (Memc[image])
	call ah_evaluate (ast, "$I", Memc[expr], eval, verbose)
	pos = clktime(0)
	call brktime (pos, tm)
	call sprintf (Memc[expr], sz_cmd, "\"%02d/%02d/%02d\"")
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (mod (TM_YEAR(tm), 100))
	call ah_evaluate (ast, "$D", Memc[expr], eval, verbose)
	call sprintf (Memc[expr], sz_cmd, "\"%02d:%02d:%02d\"")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	call ah_evaluate (ast, "$T", Memc[expr], eval, verbose)
	call brktime (lsttogmt(pos), tm)
	call sprintf (Memc[expr], sz_cmd, "\"%04d-%02d-%02d\"")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	call ah_evaluate (ast, "$GMD", Memc[expr], eval, verbose)
	call sprintf (Memc[expr], sz_cmd, "\"%02d:%02d:%02d\"")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	call ah_evaluate (ast, "$GMT", Memc[expr], eval, verbose)
	call sprintf (Memc[expr], sz_cmd, "\"%04d-%02d-%02dT%02d:%02d:%02d\"")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	call ah_evaluate (ast, "$GMDT", Memc[expr], eval, verbose)

	# Open the table file, get the column names, and insert
	# fscan in commands.

	Memc[col] = EOS
	call clgstr ("table", Memc[table], SZ_FNAME)
	if (nowhite (Memc[table], Memc[table], SZ_FNAME) > 0) {
	    AST_TFD(ast) = open (Memc[table], READ_ONLY, TEXT_FILE)
	    pos = note (AST_TFD(ast))
	    call clgstr ("colnames", Memc[col], SZ_LINE)
	    i = 0
	    ip = 1
	    while (ctowrd (Memc[col], ip, Memc[key], SZ_KEY) > 0) {
		if (i == 0)
		    call strcpy ("fscan (\"$", Memc[expr], sz_cmd)
		else
		    call strcat ("\", \"$", Memc[expr], sz_cmd)
		call strcat (Memc[key], Memc[expr], sz_cmd)
		i = i + 1
	    }
	    if (i > 0) {
		call strcat ("\")", Memc[expr], sz_cmd)
		Memc[key] = EOS
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
		ncmds = ncmds + 1

		call ah_evaluate (ast, Memc[key], Memc[expr], eval, verbose)
	    }
	}

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
		if (Memc[cmd+ip-1] == EOS)
		    ;
                else if (Memc[cmd+ip-1] == '=')
                    ip = ip + 1
                else {
		    if (oldstyle) {
			i = strdic (Memc[key], Memc[expr], sz_cmd, SPECIAL)
			if (i > 0 && streq (Memc[key], Memc[expr])) {
			    ip = 1
			    Memc[key] = EOS
			}
		    } else {
			ip = 1
			Memc[key] = EOS
		    }
                }
            default:
                ip = 1
                Memc[key] = EOS
            }

            # Parse expression.
            while (IS_WHITE(Memc[cmd+ip-1]) || Memc[cmd+ip-1] == '=')
                ip = ip + 1
            call strcpy (Memc[cmd+ip-1], Memc[expr], sz_cmd)

            if (streq (Memc[key], "quit"))
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
	    call ah_evaluate (ast, Memc[key], Memc[expr], eval, verbose)

	    ncmds = ncmds + 1
	}
	call imunmap (AST_IM(ast))
	call close (fdcmd)

	# Repeat commands for other images.
	if (ncmds > 0 && nim > 1) {
	    while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
		if (AST_TFD(ast) != NULL) {
		    if (pos == note (AST_TFD(ast)))
			break
		    if (fstati (AST_TFD(ast), F_EOF) == YES)
			call error (1, "Premature end-of-file in table")
		}

		iferr (AST_IM(ast) = immap (Memc[image], acmode, 0)) {
		    call erract (EA_WARN)
		    next
		}

		if (verbose) {
		    call printf ("%s:\n")
			call pargstr (Memc[image])
		}
	    
		eval = TRUE
		call sprintf (Memc[expr], sz_cmd, "\"%s\"")
		    call pargstr (Memc[image])
		call ah_evaluate (ast, "$I", Memc[expr], eval, verbose)

		do i = 1, ncmds
		    call ah_evaluate (ast, Memc[Memi[keys+i-1]],
			Memc[Memi[exprs+i-1]], eval, verbose)

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


# AH_EVALUATE -- Evaluate the value of the named key and add it to symbol table.

procedure ah_evaluate (ast, key, expr, eval, verbose)

pointer	ast			#I Data structure
char	key[ARB]		#I name of key to be edited
char	expr[ARB]		#I value expression
bool	eval			#U Conditional flag
bool	verbose			#I verbose output?

bool	streq()
pointer	sp, newval, oldval, o, im, sym, evvexpr(), stfind(), stenter()
int	locpr(), strncmp()
extern	ah_getop(), ast_func()
errchk	evvexpr

begin
	call smark (sp)

        # Check conditional evaluation.
        if (streq (key, "endif")) {
            eval = TRUE
            return
        } else if (streq (key, "else")) {
            eval = (!eval)
            return
        }
        if (!eval)
            return

	# Evaluate the expression.
	o = NULL
	if (expr[1] != EOS)
	    o = evvexpr (expr, locpr (ah_getop), ast, locpr (ast_func), ast, 0)

        # Set conditional evalution.
        if (key[1] == EOS) {
            if (strncmp (expr, "if ", 3) == 0 || strncmp (expr, "if(", 3) == 0)
                eval = (O_VALI(o) != 0)

	} else if (o != NULL) {
	    # Print the verbose output.
	    if (verbose) {
		call salloc (oldval, SZ_LINE, TY_CHAR)
		call salloc (newval, SZ_LINE, TY_CHAR)

		switch (O_TYPE(o)) {
		case TY_BOOL:
		    call sprintf (Memc[newval], SZ_LINE, "%b")
			call pargi (O_VALI(o))
		case TY_CHAR:
		    call sprintf (Memc[newval], SZ_LINE, "%s")
			call pargstr (O_VALC(o))
		case TY_INT:
		    call sprintf (Memc[newval], SZ_LINE, "%d")
			call pargi (O_VALI(o))
		case TY_REAL:
		    call sprintf (Memc[newval], SZ_LINE, "%g")
			call pargr (O_VALR(o))
		case TY_DOUBLE:
		    call sprintf (Memc[newval], SZ_LINE, "%g")
			call pargd (O_VALD(o))
		}

		if (key[1] == '$') {
		    call printf ("  %s = %s\n")
			call pargstr (key)
			call pargstr (Memc[newval])
		} else {
		    iferr (call imgstr (AST_IM(ast), key, Memc[oldval],
			SZ_LINE)) {
			call printf ("  %s = %s\n")
			    call pargstr (key)
			    call pargstr (Memc[newval])
		    } else {
			call printf ("  %s = %s -> %s\n")
			    call pargstr (key)
			    call pargstr (Memc[oldval])
			    call pargstr (Memc[newval])
		    }
		}
	    }

	    if (key[1] == '$') {
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
	    } else if (key[1] != EOS) {
		im = AST_IM(ast)
		iferr (call imdelf (im, key))
		    ;
		switch (O_TYPE(o)) {
		case TY_BOOL:
		    call imaddb (im, key, (O_VALI(o) == YES))
		case TY_CHAR:
		    call imastr (im, key, O_VALC(o))
		case TY_INT:
		    call imaddi (im, key, O_VALI(o))
		case TY_REAL:
		    call imaddr (im, key, O_VALR(o))
		case TY_DOUBLE:
		    call imaddd (im, key, O_VALD(o))
		}
	    }
	} else if (key[1] != '$') {
	    im = AST_IM(ast)

	    # Print the verbose output.
	    if (verbose) {
		call salloc (oldval, SZ_LINE, TY_CHAR)
		ifnoerr (call imgstr (im, key, Memc[oldval], SZ_LINE)) {
		    call printf ("  %s = %s -> DELETED\n")
			call pargstr (key)
			call pargstr (Memc[oldval])
		}
	    }

	    iferr (call imdelf (im, key))
		;
	}

	call mfree (o, TY_STRUCT)
	call sfree (sp)
end


# AH_GETOP -- Satisfy an operand request from EVEXPR.  In this context,
# operand names refer to image keywords or entries in the symbol table.

procedure ah_getop (ast, operand, o)

pointer	ast			#I Data structure
char	operand[ARB]		#I name of operand to be returned
pointer	o			#O pointer to output operand

int	ip, type, nchars
pointer	sym, im, cp
int	lexnum(), ctoi(), ctod(), imaccf(), imgeti(), imgftype()
double	imgetd()
pointer	stfind()

begin
	# Symbol table values.
	if (operand[1] == '$') {
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

	# Expression values.
	} else {
	    im = AST_IM(ast)
	    if (imaccf (im, operand) == NO)
		call xvv_error1 ("image keyword `%s' not found", operand[1])

	    switch (imgftype (im, operand)) {
	    case TY_BOOL, TY_SHORT, TY_INT, TY_LONG:
		call xvv_initop (o, 0, TY_INT)
		O_VALI(o) = imgeti (im, operand)

	    case TY_REAL, TY_DOUBLE, TY_COMPLEX:
		call xvv_initop (o, 0, TY_DOUBLE)
		O_VALD(o) = imgetd (im, operand)

	    default:
		call malloc (cp, SZ_LINE, TY_CHAR)
		call imgstr (im, operand, Memc[cp], SZ_LINE)

		ip = 1
		type = lexnum (Memc[cp], ip, nchars)
		if (Memc[cp+nchars+ip-1] != EOS)
		    type = LEX_NONNUM

		switch (type) {
		case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
		    call xvv_initop (o, 0, TY_INT)
		    ip = 1
		    nchars = ctoi (Memc[cp], ip, O_VALI(o))
		case LEX_REAL:
		    call xvv_initop (o, 0, TY_DOUBLE)
		    ip = 1
		    nchars = ctod (Memc[cp], ip, O_VALD(o))
		case LEX_NONNUM:
		    call xvv_initop (o, SZ_LINE, TY_CHAR)
		    call strcpy (Memc[cp], O_VALC(o), SZ_LINE)
		}

		call mfree (cp, TY_CHAR)
	    }
	}
end
