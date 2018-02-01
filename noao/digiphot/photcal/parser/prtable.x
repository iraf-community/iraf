.help prtable
Parser Symbol Table Handling.

.nf
Entry points:

	pr_obscol (variable, col)		Enter observational var. column
	pr_catcol (variable, col)		Enter catalog var. column
	pr_errcol (variable, col)		Enter error column
	pr_wtscol (variable, col)		Enter weight column

	pr_fitpar (name, value)			Enter fitting parameter value
	pr_const  (name, value)			Enter constant parameter value
	pr_delta  (name, value)			Enter delta for parameter value

	pr_seteq (name, eq, rpn, lenrpn)	Enter set equation

	pr_treq   (name, refeq, trneq,		Enter transformation equation
		   rpnref, lenref,
		   rpntrn, lentrn)
	pr_trder  (name, param, equation,	Enter trans. deriv.
		   rpneq, leneq)
	pr_trplot (name, xploteq, yploteq,	Enter trans. plot equations
		   rpnxplot, lenxplot,
		   rpnyplot, lenyplot)

	pr_erreq  (name, erreq, mineq, maxeq,	Enter error equation
		   rpnerr, lenerr, rpnmin,
		   lenmin, rpnmax, lenmax)
	pr_wtseq  (name, wghteq, mineq, maxeq,	Enter weight equation
		   rpnwght, lenwght, rpnmin,
		   lenmin, rpnmax, lenmax)

	pr_section (section)			Enter equation section

	pr_chkid   (name)			Check identifier type

Low level entry points:

	pr_incol (type, variable, col, spare)	    Enter input column
	pr_param (type, name, value)		    Enter parameter value
	pr_trvar (sym, nrcat, nrobs, nfcat, nfobs)  Update variables in eq.
	pr_trpar (sym, npar)			    Update parameters in eq.
	pr_trpnum (syme, symp)			    Get parameter number
.endhelp

include <mach.h>
include	"../lib/parser.h"
include	"../lib/prdefs.h"


# PR_OBSCOL -- Enter an observational variable name and its column in the
# input file into the symbol table.

procedure pr_obscol (variable, col)

char	variable[ARB]		# variable name
char	col[ARB]		# column

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_obscol (%s) (%s)\n")
	        #call pargstr (variable)
	        #call pargstr (col)
	#}

	# Enter observational variable
	call pr_incol (PTY_OBSVAR, variable, col, NO)
end


# PR_CATCOL -- Enter the name of a catalog variable for a catalog
# star, and its column in the input file into the symbol table.

procedure pr_catcol (variable, col)

char	variable[ARB]		# variable name
char	col[ARB]		# column

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_catcol (%s) (%s)\n")
	        #call pargstr (variable)
	        #call pargstr (col)
	#}

	# Enter catalog variable
	call pr_incol (PTY_CATVAR, variable, col, NO)
end


# PR_INCOL -- Enter an observational or catalog variable name, and its
# column in the input file into the symbol table.

procedure pr_incol (type, variable, col, spare)

int	type			# column type (observation or catalog)
char	variable[ARB]		# variable name
char	col[ARB]		# column
int	spare			# spare column (YES/NO) ?

char	aux[SZ_LINE]
int	sym, ip, colnum
pointer	ptr
#bool	clgetb()
int	ctoi(), pr_geti(), pr_getsym(), pr_putsym()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_incol (%d) (%s) (%s)\n")
		#call pargi (type)
	        #call pargstr (variable)
	        #call pargstr (col)
	#}

	# Enter variable into the symbol table if it's not already there.
	if (IS_INDEFI (pr_getsym (variable))) {

	    # Get column value, and check if it's in range.
	    ip = 1
	    if (ctoi (col, ip, colnum) <= 0)
		colnum = 0
	    if (colnum < pr_geti (MINCOL)) {
		call sprintf (aux, SZ_LINE,
		    "Column out of range or reserved for matching name [%s]")
		    call pargstr (variable)
		call pr_error (aux, PERR_SEMANTIC)
	    }

	    # Enter and initialize variable name in the table.
	    sym = pr_putsym (variable)

	    # Enter type.
	    call pr_psymi (sym, PSYMTYPE, type)

	    # Allocate space for the symbol substructure,
	    # and store it into the symbol structure.
	    call pr_inalloc (ptr)
	    call pr_psymp (sym, PSYMSUB, ptr)

	    # Enter column number and spare flag.
	    call pr_psymi (sym, PINPCOL, colnum)
	    call pr_psymi (sym, PINPSPARE, spare)

	    # Count variables, and enter variable number.
	    if (type == PTY_OBSVAR) {
	        call pr_inci (NOBSVARS, 1)
		call pr_psymi (sym, PSYMNUM, pr_geti (NOBSVARS))
	    } else {
	        call pr_inci (NCATVARS, 1)
		call pr_psymi (sym, PSYMNUM, pr_geti (NCATVARS))
	    }

	} else {
	    call sprintf (aux, SZ_LINE,
		"Input variable [%s] declared more than once")
		call pargstr (variable)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_ERRCOL -- Enter an observational or catalog variable error column in
# the input file into the symbol table.

procedure pr_errcol (variable, col)

char	variable[ARB]		# variable name
char	col[ARB]		# column

char	aux[SZ_LINE]
int	sym, ip, colnum
#bool	clgetb()
int	ctoi(), pr_geti(), pr_getsym(), pr_gsymi()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_errcol (%s) (%s)\n")
	        #call pargstr (variable)
	        #call pargstr (col)
	#}

	# Enter error into table if the variable is already there.
	sym = pr_getsym (variable)
	if (!IS_INDEFI (sym)) {

	    # Get column value, and check if it's in range.
	    ip = 1
	    if (ctoi (col, ip, colnum) <= 0)
		colnum = 0
	    if (colnum < pr_geti (MINCOL)) {
		call sprintf (aux, SZ_LINE,
		 "Error column out of range or reserved for matching name [%s]")
		    call pargstr (variable)
		call pr_error (aux, PERR_SEMANTIC)
	    }

	    # Enter column value
	    call pr_psymi (sym, PINPERRCOL, colnum)

	    # Enter spare value.
	    call sprintf (aux, SZ_LINE, "@E_%s")
		call pargstr (variable)
	    call pr_incol (pr_gsymi (sym, PSYMTYPE), aux, col, YES)

	} else {
	    call sprintf (aux, SZ_LINE,
		"Attempt to define error column for undefined variable [%s]")
		call pargstr (variable)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_WTSCOL -- Enter an observational or catalog variable weight column in
# the input file into the symbol table.

procedure pr_wtscol (variable, col)

char	variable[ARB]		# variable name
char	col[ARB]		# column

char	aux[SZ_LINE]
int	sym, ip, colnum
#bool	clgetb()
int	ctoi(), pr_geti(), pr_getsym(), pr_gsymi()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_wtscol (%s) (%s)\n")
	        #call pargstr (variable)
	        #call pargstr (col)
	#}

	# Enter error into table if the variable is already there.
	sym = pr_getsym (variable)
	if (!IS_INDEFI (sym)) {

	    # Get column value, and check if it's in range.
	    ip = 1
	    if (ctoi (col, ip, colnum) <= 0)
		colnum = 0
	    if (colnum < pr_geti (MINCOL)) {
		call sprintf (aux, SZ_LINE,
		"Weight column out of range or reserved for matching name [%s]")
		    call pargstr (variable)
		call pr_error (aux, PERR_SEMANTIC)
	    }

	    # Enter column value.
	    call pr_psymi (sym, PINPWTSCOL, colnum)

	    # Enter spare value.
	    call sprintf (aux, SZ_LINE, "@W_%s")
		call pargstr (variable)
	    call pr_incol (pr_gsymi (sym, PSYMTYPE), aux, col, YES)

	} else {
	    call sprintf (aux, SZ_LINE,
		"Attempt to define weight column for undefined variable [%s]")
		call pargstr (variable)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_FITPAR -- Enter a variable name and its value as a fitting parameter
# into the symbol table, if it's not already there.

procedure pr_fitpar (name, value)

char	name[ARB]		# parameter name
char	value[ARB]		# parameter value

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_fitpar (%s) (%s)\n")
	        #call pargstr (name)
	        #call pargstr (value)
	#}

	# Enter fitting parameter.
	call pr_param (PTY_FITPAR, name, value)
end


# PR_CONST -- Enter a variable name and its value as a constant parameter
# into the symbol table, if it's not already there.

procedure pr_const (name, value)

char	name[ARB]		# constant name
char	value[ARB]		# parameter value

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_const (%s) (%s)\n")
	        #call pargstr (name)
	        #call pargstr (value)
	#}

	# Enter constant parameter.
	call pr_param (PTY_CONST, name, value)
end


# PR_PARAM -- Enter a variable name and its value as either a constant
# or fitting parameter into the symbol table, if it's not already there.

procedure pr_param (type, name, value)

int	type			# parameter type
char	name[ARB]		# parameter name
char	value[ARB]		# parameter value

char	aux[SZ_LINE]
int	sym, ip, n, symtype
pointer	ptr
real	rval

#bool	clgetb()
int	ctor(), pr_geti(), pr_gsymi(), pr_getsym(), pr_putsym()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_param (%d) (%s) (%s)\n")
		#call pargi (type)
	        #call pargstr (name)
	        #call pargstr (value)
	#}

	# Get parameter value, and check it.
	ip = 1
	n = ctor (value, ip, rval)
	if (n == 0 || IS_INDEFR (rval)) {
	    call sprintf (aux, SZ_LINE,
		"Constant or fitting parameter value undefined for [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}

	# Get symbol and symbol type.
	sym = pr_getsym (name)
	if (!IS_INDEFI (sym))
	    symtype = pr_gsymi (sym, PSYMTYPE)
	else
	    symtype = INDEFI

	# Enter name into the symbol table if it's not
	# already there. Otherwise redefine it if possible.
	# Do not enter or redefine with undefined values.
	if (IS_INDEFI (sym)) {

	    # Enter name into symbol table.
	    sym = pr_putsym (name)

	    # Enter type
	    call pr_psymi (sym, PSYMTYPE, type)

	    # Allocate space for the symbol substructure,
	    # and store it into the symbol structure.
	    call pr_ftalloc (ptr)
	    call pr_psymp (sym, PSYMSUB, ptr)

	    # Count total number of parameters, and number
	    # of fitting parameters.
	    call pr_inci (NTOTPARS, 1)
	    if (type == PTY_FITPAR)
		call pr_inci (NFITPARS, 1)

	    # Enter number, and value.
	    call pr_psymi (sym, PSYMNUM, pr_geti (NTOTPARS))
	    call pr_psymr (sym, PFITVALUE, rval)

	} else if (symtype == PTY_FITPAR || symtype == PTY_CONST) {

	    # Update fitting parameter counter.
	    if (symtype == PTY_FITPAR && type == PTY_CONST)
		call pr_deci (NFITPARS, 1)
	    else if (symtype == PTY_CONST && type == PTY_FITPAR)
		call pr_inci (NFITPARS, 1)

	    # Redefine type and value, but not number.
	    call pr_psymi (sym, PSYMTYPE, type)
	    call pr_psymr (sym, PFITVALUE, rval)

	    # Issue warning message.
	    call sprintf (aux, SZ_LINE,
	        "Constant or fitting parameter [%s] redefined")
		call pargstr (name)
	    call pr_error (aux, PERR_WARNING)

	} else {
	    call sprintf (aux, SZ_LINE,
		"Constant or fitting parameter [%s] declared more than once")
	        call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_DELTA -- Enter a variable name and its value as the delta of a fitting
# or constant parameter. Check for negative or zero delta values.

procedure pr_delta (name, value)

char	name[ARB]		# parameter name
char	value[ARB]		# delta value

char	aux[SZ_LINE]
int	sym, ip, n, symtype
real	rval
#bool	clgetb()
int	ctor(), pr_getsym(), pr_gsymi()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_delta (%s) (%s)\n")
	        #call pargstr (name)
	        #call pargstr (value)
	#}

	# Get symbol and symbol type
	sym = pr_getsym (name)
	if (!IS_INDEFI (sym)) {

	    # Get delta value, and check it.
	    ip = 1
	    n = ctor (value, ip, rval)
	    if (n == 0 || IS_INDEFR (rval)) {
		call sprintf (aux, SZ_LINE,
		    "Delta value undefined for parameter [%s]")
		    call pargstr (name)
		call pr_error (aux, PERR_SEMANTIC)
	    } else if (rval <= 0) {
		call sprintf (aux, SZ_LINE,
		    "Delta value for parameter [%s] must be positive")
		    call pargstr (name)
		call pr_error (aux, PERR_SEMANTIC)
	    }

	    # Enter delta if the type is consistent.
	    symtype = pr_gsymi (sym, PSYMTYPE)
	    if (symtype == PTY_FITPAR || symtype == PTY_CONST) {
	    	call pr_psymr (sym, PFITDELTA, rval)
	    } else {
	        call sprintf (aux, SZ_LINE,
		    "Attempt to define a delta for a non-parameter [%s]")
		    call pargstr (name)
	        call pr_error (aux, PERR_SEMANTIC)
	    }

	} else {
	    call sprintf (aux, SZ_LINE,
		"Attempt to define delta for undefined parameter [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_SETEQ -- Enter the set equation the symbol table, if it's not already
# there.

procedure pr_seteq (name, eq, rpn, lenrpn)

char	name[ARB]		# equation name
char	eq[ARB]			# equation
pointer	rpn			# equation code
int	lenrpn			# code length

char	aux[SZ_LINE]
int	sym
pointer	ptr

#bool	clgetb()
int	pr_geti(), pr_getsym(), pr_putsym()
pointer	pr_cput()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_seteq (%s) (eq=%s) (rpn=%d,%d)\n")
	        #call pargstr (name)
	        #call pargstr (eq)
		#call pargi (rpn)
		#call pargi (lenrpn)
	#}

	# Enter ser equation into the symbol table if it's not
	# already there.
	if (IS_INDEFI (pr_getsym (name))) {

	    # Enter equation into symbol table.
	    sym = pr_putsym (name)

	    # Count equations.
	    call pr_inci (NSETEQS, 1)

	    # Enter equation type, and number.
	    call pr_psymi (sym, PSYMTYPE, PTY_SETEQ)
	    call pr_psymi (sym, PSYMNUM, pr_geti (NSETEQS))

	    # Allocate space for an equation substructure,
	    # and store it into the symbol structure.
	    call pr_stalloc (ptr)
	    call pr_psymp (sym, PSYMSUB, ptr)

	    # Enter equation string offset, and code.
	    call pr_psymc (sym, PSEQEQ, eq)
	    call pr_psymp (sym, PSEQRPNEQ, pr_cput (rpn, lenrpn))

	    # Enter null strings for error, and weight equations
	    # because they might not be defined afterwards, and because
	    # they can't be initialized at allocation time.
	    call pr_psymc (sym, PSEQERROR,  "")
	    call pr_psymc (sym, PSEQERRMIN, "")
	    call pr_psymc (sym, PSEQERRMAX, "")
	    call pr_psymc (sym, PSEQWEIGHT, "")
	    call pr_psymc (sym, PSEQWTSMIN, "")
	    call pr_psymc (sym, PSEQWTSMAX, "")

	} else {
	    call sprintf (aux, SZ_LINE,
		"Set equation declared more than once [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_TREQ -- Enter the transformation equation along with its reference
# equation, and its plotting equations into the symbol table.

procedure pr_treq (name, refeq, trneq, rpnref, lenref, rpntrn, lentrn)

char	name[ARB]		# equation name
char	refeq[ARB]		# reference equation
char	trneq[ARB]		# transformation equation
pointer	rpnref			# reference equation code
int	lenref			# code length
pointer	rpntrn			# transformation equation code
int	lentrn			# code length

char	aux[SZ_LINE]
int	i, nrcat, nrobs, nfcat, nfobs, npar, sym
pointer	ptr
#bool	clgetb()
int	mct_nrows(), pr_geti(), pr_getsym(), pr_putsym()
pointer	pr_getp(), pr_cput()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_treq (%s) (ref=%s) (eq=%s)\n")
	        #call pargstr (name)
	        #call pargstr (refeq)
	        #call pargstr (trneq)
	    #call eprintf (
		#"pr_treq (ref=%d,%d) (eq=%d,%d)\n")
		#call pargi (rpnref)
		#call pargi (lenref)
		#call pargi (rpntrn)
		#call pargi (lentrn)
	#}

	# Enter transformation equation into the symbol table
	# if it's not already there.
	if (IS_INDEFI (pr_getsym (name))) {

	    # Enter equation into symbol table. The symbol
	    # attributes are initialized to default values.
	    sym = pr_putsym (name)

	    # Count equations.
	    call pr_inci (NTRNEQS, 1)

	    # Enter equation type, and number.
	    call pr_psymi (sym, PSYMTYPE, PTY_TRNEQ)
	    call pr_psymi (sym, PSYMNUM, pr_geti (NTRNEQS))

	    # Get number of catalog and observational variables,
	    # and number of parameters for the current equation.
	    # All of them were stored in the temporary table during
	    # the parse of the expression.
	    nrcat = mct_nrows (pr_getp (TRCATTABLE))
	    nrobs = mct_nrows (pr_getp (TROBSTABLE))
	    nfcat = mct_nrows (pr_getp (TFCATTABLE))
	    nfobs = mct_nrows (pr_getp (TFOBSTABLE))
	    npar  = mct_nrows (pr_getp (TPARTABLE))

	    # Allocate space for an equation substructure,
	    # and store it into the symbol structure.
	    call pr_tralloc (ptr, nrcat, nrobs, nfcat, nfobs, npar)
	    call pr_psymp (sym, PSYMSUB, ptr)

	    # Update variable counters in the equation substructure.
	    call pr_trvar (sym, nrcat, nrobs, nfcat, nfobs)

	    # Update fitting parameter data in the equation substructure.
	    call pr_trpar (sym, npar)

	    # Enter equation string offsets.
	    call pr_psymc (sym, PTEQFIT, trneq)
	    call pr_psymc (sym, PTEQREF, refeq)

	    # Enter null strings for error, weight, plot equations, and
	    # derivative equations, because they might not be defined
	    # afterwards, and because they can't be initialized at
	    # allocation time.
	    call pr_psymc (sym, PTEQERROR, "")
	    call pr_psymc (sym, PTEQERRMIN, "")
	    call pr_psymc (sym, PTEQERRMAX, "")
	    call pr_psymc (sym, PTEQWEIGHT, "")
	    call pr_psymc (sym, PTEQWTSMIN, "")
	    call pr_psymc (sym, PTEQWTSMAX, "")
	    call pr_psymc (sym, PTEQXPLOT, "")
	    call pr_psymc (sym, PTEQYPLOT, "")
	    do i = 1, npar
		call pr_pderc (sym, i, PTEQDER, "")

	    # Enter equation codes.
	    call pr_psymp (sym, PTEQRPNFIT, pr_cput (rpntrn, lentrn))
	    call pr_psymp (sym, PTEQRPNREF, pr_cput (rpnref, lenref))

	    # Clear temporary tables.
	    call mct_reset (pr_getp (TROBSTABLE))
	    call mct_reset (pr_getp (TRCATTABLE))
	    call mct_reset (pr_getp (TFOBSTABLE))
	    call mct_reset (pr_getp (TFCATTABLE))
	    call mct_reset (pr_getp (TPARTABLE))

	} else { 
	    call sprintf (aux, SZ_LINE,
		"Transformation equation [%s] declared more than once")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_TRPAR -- Update fitting parameters in the equation substructure.
# Fitting and constant parameters for the current equation were stored
# in the temporary table when the equation was parsed.
# Count fitting (active) parameters, and update the parameter
# values, and fitting parameter list.

procedure pr_trpar (sym, npar)

int	sym		# equation symbol
int	npar		# number of parameters

int	nfpar		# number of fitting parameters
int	symp		# parameter symbol
int	i

#bool	clgetb()
int	pr_gsymi(), pr_gpari()
pointer	pr_getp(), pr_gsymp(), mct_getbuf()
real	pr_gsymr()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_trpar (sym=%d) (npar=%d)\n")
	        #call pargi (sym)
		#call pargi (npar)
	#}

	# Move parameter offsets from temporary table to
	# equation substructure, if there are any.
	if (npar > 0) {
	    call amovi (Memi[mct_getbuf (pr_getp (TPARTABLE))],
			Memi[P2I(pr_gsymp (sym, PTEQSPAR))], npar)
	}

	# Clear the fitting parameter list.
	call aclri (Memi[P2I(pr_gsymp (sym, PTEQSPLIST))], npar)

	# Reset number of fitting parameters, and iterate
	# for all the parameters in the equation.
	nfpar = 0
	do i = 1, npar {

	    # Get parameter symbol and process it.
	    symp = pr_gpari (sym, i, PTEQPAR)
	    if (!IS_INDEFI (symp)) {

		# Enter value.
		call pr_pparr (sym, i, PTEQPARVAL, pr_gsymr (symp, PFITVALUE))

		# Enter fitting parameter number to the list.
		if (pr_gsymi (symp, PSYMTYPE) == PTY_FITPAR) {
		    nfpar = nfpar + 1
		    call pr_ppari (sym, nfpar, PTEQPLIST, i)
		}

	    } else
		call error (0, "pr_trpar: Undefined parameter symbol")
	}

	# Enter number of fitting (active) parameters.
	call pr_psymi (sym, PTEQNFPAR, nfpar)
end


# PR_TRVAR -- Update variable symbols and counters in the equation
# substructure. Variable symbols and counters for the reference and
# fit equations were stored in the temporary tables when the equation
# was parsed. The offsets and counters come from two different sequential
# tables, but are stored in one place in the equation substructure.

procedure pr_trvar (sym, nrcat, nrobs, nfcat, nfobs)

int	sym		# equation symbol
int	nrcat, nrobs	# reference eq. counters
int	nfcat, nfobs	# fit eq. counters

int	i
pointer	table

#bool	clgetb()
int	mct_geti()
pointer	pr_getp()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf (
	        #"pr_trvar (sym=%d) (nrc=%d) (nro=%d) (nfc=%d) (nfo=%d)\n")
	        #call pargi (sym)
		#call pargi (nrcat)
		#call pargi (nrobs)
		#call pargi (nfcat)
		#call pargi (nfobs)
	#}

	# Update reference equation symbols and counters.
	table = pr_getp (TRCATTABLE)
	do i = 1, nrcat {
	    call pr_pvari (sym, i, PTEQREFVAR, mct_geti (table, i, 1))
	    call pr_pvari (sym, i, PTEQREFCNT, mct_geti (table, i, 2))
	}
	table = pr_getp (TROBSTABLE)
	do i = nrcat + 1, nrcat + nrobs {
	    call pr_pvari (sym, i, PTEQREFVAR, mct_geti (table, i - nrcat, 1))
	    call pr_pvari (sym, i, PTEQREFCNT, mct_geti (table, i - nrcat, 2))
	}

	# Update fit equation symbols and counters
	table = pr_getp (TFCATTABLE)
	do i = 1, nfcat {
	    call pr_pvari (sym, i, PTEQFITVAR, mct_geti (table, i, 1))
	    call pr_pvari (sym, i, PTEQFITCNT, mct_geti (table, i, 2))
	}
	table = pr_getp (TFOBSTABLE)
	do i = nfcat + 1, nfcat + nfobs {
	    call pr_pvari (sym, i, PTEQFITVAR, mct_geti (table, i - nfcat, 1))
	    call pr_pvari (sym, i, PTEQFITCNT, mct_geti (table, i - nfcat, 2))
	}
end


# PR_TRDER -- Enter the derivative of a given equation with respect to
# a fitting parameter or constant into the symbol table.

procedure pr_trder (name, param, equation, rpneq, leneq)

char	name[ARB]		# equation name
char	param[ARB]		# parameter name
char	equation[ARB]		# derivative equation
pointer	rpneq			# derivative code
int	leneq			# code length

char	aux[SZ_LINE]
int	np
int	type
int	syme, symp

#bool	clgetb()
int	pr_gsymi()
int	pr_trpnum()
int	pr_getsym()
pointer	pr_cput()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_trder (%s) (%s) (%s) (%d,%d)\n")
	        #call pargstr (name)
	        #call pargstr (param)
	        #call pargstr (equation)
		#call pargi (rpneq)
		#call pargi (leneq)
	#}

	# Get parameter symbol from the table if it's already
	# there. Otherwise issue an error message.
	symp = pr_getsym (param)
	if (!IS_INDEFI (symp)) {
	    type = pr_gsymi (symp, PSYMTYPE)
	    if (type != PTY_FITPAR && type != PTY_CONST) {
	        call sprintf (aux, SZ_LINE,
		    "Derivative with respect of non-parameter [%s]")
		    call pargstr (param)
	        call pr_error (aux, PERR_SEMANTIC)
		return
	    }
	} else {
	    call sprintf (aux, SZ_LINE,
		"Derivative with respect of undefined parameter [%s]")
		call pargstr (param)
	    call pr_error (aux, PERR_SEMANTIC)
	    return
	}

	# Enter the derivative into the symbol table if the equation
	# is already there, and if the fitting parameter belongs to
	# the equation.
	syme = pr_getsym (name)
	if (!IS_INDEFI (syme)) {
	    if (pr_gsymi (syme, PSYMTYPE) == PTY_TRNEQ) {

		# Get parameter number for the equation. An undefined
		# value means that it doesn't belong to it.
		np = pr_trpnum (syme, symp)

		# If the parameter was found enter the derivative
		# equation, and code in the substructure under the
		# parameter number
		if (!IS_INDEFI (np)) {
		    call pr_pderc (syme, np, PTEQDER, equation)
		    call pr_pderp (syme, np, PTEQRPNDER, pr_cput (rpneq, leneq))
		} else {
		    call sprintf (aux, SZ_LINE,
		    "Derivative with respect to unappropiate parameter [%s]")
			call pargstr (param)
	            call pr_error (aux, PERR_WARNING)
		}

	    } else {
	        call sprintf (aux, SZ_LINE,
		    "Derivative of non-transformation equation [%s]")
		    call pargstr (name)
	        call pr_error (aux, PERR_SEMANTIC)
	    }

	} else {
	    call sprintf (aux, SZ_LINE, "Derivative of undefined equation [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_TRPLOT -- Enter plot equations of a given transformation equation
# into the symbol table.

procedure pr_trplot (name, xploteq, yploteq, rpnxplot, lenxplot,
	rpnyplot, lenyplot)

char	name[ARB]		# equation name
char	xploteq[ARB]		# x plot equation
char	yploteq[ARB]		# y plot equation
pointer	rpnxplot		# x plot equation code
int	lenxplot		# x plot code length
pointer	rpnyplot		# y plot equation code
int	lenyplot		# y plot code length

char	aux[SZ_LINE]
int	sym
#bool	clgetb()
int	pr_gsymi(), pr_getsym()
pointer	pr_cput()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_trplot (%s) (%s) (%s)\n")
	        #call pargstr (name)
	        #call pargstr (xploteq)
	        #call pargstr (yploteq)
	    #call eprintf (
		#"pr_trplot (xplot=%d,%d) (yplot=%d,%d)\n")
		#call pargi (rpnxplot)
		#call pargi (lenxplot)
		#call pargi (rpnyplot)
		#call pargi (lenyplot)
	#}

	# Enter the plot equations into the symbol table if the
	# equation is already there.
	sym = pr_getsym (name)
	if (!IS_INDEFI (sym)) {
	    if (pr_gsymi (sym, PSYMTYPE) == PTY_TRNEQ) {

		# Enter equation string offsets
		call pr_psymc (sym, PTEQXPLOT, xploteq)
		call pr_psymc (sym, PTEQYPLOT, yploteq)

		# Enter equation codes
		call pr_psymp (sym, PTEQRPNXPLOT, pr_cput (rpnxplot, lenxplot))
		call pr_psymp (sym, PTEQRPNYPLOT, pr_cput (rpnyplot, lenyplot))

	    } else {
	        call sprintf (aux, SZ_LINE,
		    "Plot of non-transformation equation [%s]")
		    call pargstr (name)
	        call pr_error (aux, PERR_SEMANTIC)
	    }

	} else {
	    call sprintf (aux, SZ_LINE, "Plot of undefined equation [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_TRPNUM -- Get parameter number for the equation.

int procedure pr_trpnum (syme, symp)

int	syme			# equation symbol
int	symp			# symbol symbol

int	i, np
#bool	clgetb()
int	pr_gsymi(), pr_gpari()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_trpnum (%d) (%d)\n")
		#call pargi (syme)
		#call pargi (symp)
	#}

	# Initialize to undefined.
	np = INDEFI

	# Search for the parameter into the equation substructure.
	do i = 1, pr_gsymi (syme, PTEQNPAR) {
	    if (symp == pr_gpari (syme, i, PTEQPAR)) {
		np = i
		break
	    }
	}

	# Return parameter number.
	return (np)
end


# PR_ERREQ -- Enter the error equation of a given transformation or 
# set equation into the symbol table.

procedure pr_erreq (name, erreq, mineq, maxeq, rpnerr, lenerr, rpnmin,
	lenmin, rpnmax, lenmax)

char	name[ARB]		# equation name
char	erreq[ARB]		# error equation
char	mineq[ARB]		# min equation
char	maxeq[ARB]		# max equation
pointer	rpnerr			# error code
int	lenerr			# error code length
pointer	rpnmin			# min code
int	lenmin			# min code length
pointer	rpnmax			# max code
int	lenmax			# max code length

char	aux[SZ_LINE]
int	sym, type
#bool	clgetb()
int	pr_gsymi(), pr_getsym()
pointer	pr_cput()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_erreq (%s) (%s) (%s) (%s)\n")
	        #call pargstr (name)
	        #call pargstr (erreq)
	        #call pargstr (mineq)
	        #call pargstr (maxeq)
	    #call eprintf (
		#"pr_erreq (err=%d,%d) (min=%d,%d) (max=%d,%d)\n")
		#call pargi (rpnerr)
		#call pargi (lenerr)
		#call pargi (rpnmin)
		#call pargi (lenmin)
		#call pargi (rpnmax)
		#call pargi (lenmax)
	#}

	# Enter the error, maximum, and minimum equations into the
	# symbol table if the equation is already there.
	sym = pr_getsym (name)
	if (!IS_INDEFI (sym)) {
	    type = pr_gsymi (sym, PSYMTYPE)
	    if (type == PTY_SETEQ) {

		# Enter equation string offsets
		call pr_psymc (sym, PSEQERROR, erreq)
		call pr_psymc (sym, PSEQERRMIN, mineq)
		call pr_psymc (sym, PSEQERRMAX, maxeq)

		# Enter equation codes
		call pr_psymp (sym, PSEQRPNERROR, pr_cput (rpnerr, lenerr))
		if (lenmin > 0)
		    call pr_psymp (sym, PSEQRPNERRMIN, pr_cput (rpnmin, lenmin))
		if (lenmax > 0)
		    call pr_psymp (sym, PSEQRPNERRMAX, pr_cput (rpnmax, lenmax))

	    } else if (type == PTY_TRNEQ) {

		# Enter equation string offsets
		call pr_psymc (sym, PTEQERROR, erreq)
		call pr_psymc (sym, PTEQERRMIN, mineq)
		call pr_psymc (sym, PTEQERRMAX, maxeq)

		# Enter equation codes
		call pr_psymp (sym, PTEQRPNERROR, pr_cput (rpnerr, lenerr))
		if (lenmin > 0)
		    call pr_psymp (sym, PTEQRPNERRMIN, pr_cput (rpnmin, lenmin))
		if (lenmax > 0)
		    call pr_psymp (sym, PTEQRPNERRMAX, pr_cput (rpnmax, lenmax))

	    } else {
	        call sprintf (aux, SZ_LINE,
		    "Error of non transformation or set equation [%s]")
		    call pargstr (name)
	        call pr_error (aux, PERR_SEMANTIC)
	    }

	} else {
	    call sprintf (aux, SZ_LINE, "Error of undefined equation [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_WTSEQ -- Enter the weight equation of a given transformation or set
# equation into the symbol table.

procedure pr_wtseq (name, wghteq, mineq, maxeq, rpnwght, lenwght, rpnmin,
	lenmin, rpnmax, lenmax)

char	name[ARB]		# equation name
char	wghteq[ARB]		# weight equation
char	mineq[ARB]		# min equation
char	maxeq[ARB]		# max equation
pointer	rpnwght			# weight code
int	lenwght			# weight code length
pointer	rpnmin			# min code
int	lenmin			# min code length
pointer	rpnmax			# max code
int	lenmax			# max code length

char	aux[SZ_LINE]
int	sym, type
#bool	clgetb()
int	pr_gsymi(), pr_getsym()
pointer	pr_cput()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_wtseq (%s) (%s) (%s) (%s)\n")
	        #call pargstr (name)
	        #call pargstr (wghteq)
	        #call pargstr (mineq)
	        #call pargstr (maxeq)
	    #call eprintf (
		#"pr_wtseq (wght=%d,%d) (min=%d,%d) (max=%d,%d)\n")
		#call pargi (rpnwght)
		#call pargi (lenwght)
		#call pargi (rpnmin)
		#call pargi (lenmin)
		#call pargi (rpnmax)
		#call pargi (lenmax)
	#}

	# Enter the weight, maximum, and minimum equations into the
	# symbol table if the equation is already there.
	sym = pr_getsym (name)
	if (!IS_INDEFI (sym)) {
	    type = pr_gsymi (sym, PSYMTYPE)
	    if (type == PTY_SETEQ) {

		# Enter equation string offsets
		call pr_psymc (sym, PSEQWEIGHT, wghteq)
		call pr_psymc (sym, PSEQWTSMIN, mineq)
		call pr_psymc (sym, PSEQWTSMAX, maxeq)

		# Enter equation codes
		call pr_psymp (sym, PSEQRPNWEIGHT, pr_cput (rpnwght, lenwght))
		if (lenmin > 0)
		    call pr_psymp (sym, PSEQRPNWTSMIN, pr_cput (rpnmin, lenmin))
		if (lenmax > 0)
		    call pr_psymp (sym, PSEQRPNWTSMAX, pr_cput (rpnmax, lenmax))

	    } else if (type == PTY_TRNEQ) {

		# Enter equation string offsets
		call pr_psymc (sym, PTEQWEIGHT, wghteq)
		call pr_psymc (sym, PTEQWTSMIN, mineq)
		call pr_psymc (sym, PTEQWTSMAX, maxeq)

		# Enter equation codes
		call pr_psymp (sym, PTEQRPNWEIGHT, pr_cput (rpnwght, lenwght))
		if (lenmin > 0)
		    call pr_psymp (sym, PTEQRPNWTSMIN, pr_cput (rpnmin, lenmin))
		if (lenmax > 0)
		    call pr_psymp (sym, PTEQRPNWTSMAX, pr_cput (rpnmax, lenmax))

	    } else {
	        call sprintf (aux, SZ_LINE,
		    "Weight of non transformation or set equation [%s]")
		    call pargstr (name)
	        call pr_error (aux, PERR_SEMANTIC)
	    }

	} else {
	    call sprintf (aux, SZ_LINE, "Weight of undefined equation [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end


# PR_SECTION -- Set the equation section.

procedure pr_section (section)

int	section		# equation section

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_section (%d)\n")
	        #call pargi (section)
	#}

	# Set the type flag. 
	call pr_puti (FLAGEQSECT, section)
end


# PR_CHKID -- Check the identifier according to the equation section.

procedure pr_chkid (name)

char	name[ARB]		# identifier name

bool	found
char	aux[SZ_LINE]
int	row, nrows, type, sym
pointer	table

#bool	clgetb()
int	mct_nrows(), mct_geti()
int	pr_geti(), pr_gsymi()
int	pr_getsym()
pointer	pr_getp()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_chkid (%s)\n")
	        #call pargstr (name)
	#}

	# Check if identfier is in the table.
	sym = pr_getsym (name)
	if (!IS_INDEFI (sym)) {

	    # Get symbol type.
	    type = pr_gsymi (sym, PSYMTYPE)

	    # Check equation section.
	    switch (pr_geti (FLAGEQSECT)) {
		case PRS_SETEQ:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_SETEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in set equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		case PRS_TRNREF:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_SETEQ  && type != PTY_TRNEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in reference equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		    # Enter observational or catalog variable into the
		    # corresponding sequential table, if it was not
		    # already there. Otherwise increment variable counter.
		    if (type == PTY_OBSVAR || type == PTY_CATVAR) {

			# Select temporary table
			switch (type) {
			case PTY_OBSVAR:
			    table = pr_getp (TROBSTABLE)
			case PTY_CATVAR:
			    table = pr_getp (TRCATTABLE)
			}

			# Search for symbol in the sequential table
			found = false
			nrows = mct_nrows (table)
			do row = 1, nrows {
			    if (sym == mct_geti (table, row, 1)) {
				found = true
				break
			    }
			}

			# Increment counter if the variable was found.
			# Otherwise enter symbol and initialize counter
			# to one.
			if (found) {
			    call mct_puti (table, row, 2,
					   mct_geti (table, row, 2) + 1)
			} else {
			    call mct_puti (table, nrows + 1, 1, sym)
			    call mct_puti (table, nrows + 1, 2, 1)
			}
		    }

		case PRS_TRNFIT:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_FITPAR && type != PTY_CONST  &&
			type != PTY_SETEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in fit equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		    # Enter observational variable, catalog variable,
		    # fitting parameter, or constant parameter into the
		    # corresponding sequential table, if it was not already
		    # there. Otherwise, for variables, increment counter.
		    # For fitting parameters, also  update the symbol number
		    # relative to the current equation.
		    if (type == PTY_OBSVAR || type == PTY_CATVAR ||
			type == PTY_FITPAR || type == PTY_CONST) {

			# Select temporary table
			switch (type) {
			case PTY_OBSVAR:
			    table = pr_getp (TFOBSTABLE)
			case PTY_CATVAR:
			    table = pr_getp (TFCATTABLE)
			case PTY_FITPAR, PTY_CONST:
			    table = pr_getp (TPARTABLE)
			}

			# Search for symbol in the sequential table
			found = false
			nrows = mct_nrows (table)
			do row = 1, nrows {
			    if (sym == mct_geti (table, row, 1)) {
				found = true
				break
			    }
			}

			# Enter symbol into the sequential table if it was
			# not found. For variables initialize counter, and
			# for parameters update the symbol number.
			# Otherwise, increment the variable counter.
			if (found) {
			    if (type == PTY_CATVAR || type == PTY_OBSVAR)
			        call mct_puti (table, row, 2,
					       mct_geti (table, row, 2) + 1)
			} else {
			    call mct_puti (table, nrows + 1, 1, sym)
			    if (type == PTY_CATVAR || type == PTY_OBSVAR)
			        call mct_puti (table, nrows + 1, 2, 1)
			    else if (type == PTY_FITPAR || type == PTY_CONST)
			        call pr_psymi (sym, PSYMNUM, nrows + 1)
			}
		    }

		case PRS_TRNDER:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_FITPAR && type != PTY_CONST &&
			type != PTY_SETEQ && type != PTY_TRNEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in derivative equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		case PRS_TRNPLOT:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_FITPAR && type != PTY_CONST &&
			type != PTY_SETEQ  && type != PTY_TRNEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in plot equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		case PRS_ERREQ:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_FITPAR && type != PTY_CONST  &&
			type != PTY_SETEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in error equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		case PRS_WTSEQ:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_FITPAR && type != PTY_CONST  &&
			type != PTY_SETEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in weight equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		case PRS_LMTEQ:
		    if (type != PTY_OBSVAR && type != PTY_CATVAR &&
			type != PTY_FITPAR && type != PTY_CONST  &&
			type != PTY_SETEQ) {
			call sprintf (aux, SZ_LINE, 
			    "Illegal identifier in min or max equation [%s]")
			    call pargstr (name)
			call pr_error (aux, PERR_SEMANTIC)
		    }

		default: call error (0, "pr_chkid: Unknown section type")
	    }

	} else {
	    call sprintf (aux, SZ_LINE, 
		"Undefined identifier in expression [%s]")
		call pargstr (name)
	    call pr_error (aux, PERR_SEMANTIC)
	}
end
