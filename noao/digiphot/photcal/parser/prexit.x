.help prexit
Parser Exit Handling.

After the compilation has finished without errors, the parser runs the
pr_exit() routine in order to make sure that there are no inconsistencies in
the parser symbol table, and to perform all steps that can be done only
with the full symbol table. This procedure performs the following actions:

- Builds the list of sequential tables for each type of variable, parameter,
and equation in the symbol table. These tables are used later to access
each type sequentially.

- Sets the minimum and maximum values for observational and catalog variables.

- Checks that there are no duplications in either the observational or catalog
input columns.

- Checks that all the derivatives for transformation equations are defined.

If an error or inconsistency is detected an error message is issued.

Entry point:

	pr_exit()		Exit procedure
.endhelp

include <mach.h>
include	"../lib/parser.h"
include	"../lib/prdefs.h"


# PR_EXIT - Parser exit procedure. 

procedure pr_exit ()

bool	derflag, dltflag
int	i1, i2, incol, errcol, wtscol, mincol, maxcol, par, type
int	npar, sym
pointer	sp, aux, symtab, der
#real	delta

#bool	clgetb()
int	mct_nrows(), mct_geti(), pr_geti(), pr_gsymi(), pr_gpari()
pointer	sthead(), stnext(), pr_xgetname(), pr_offset, pr_getp(), pr_gderp()
real	pr_gsymr()

begin
	# Debug ?
	#if (clgetb ("debug.parcode"))
	    #call eprintf ("pr_exit.in\n")

	# Allocate working space
	call smark (sp)
	call salloc (aux, SZ_LINE, TY_CHAR)

	# Initialize minimum and maximum column values.
	# Check for empty sections to initialize with the
	# right value.

	if (pr_geti (NOBSVARS) > 0) {
	    call pr_puti (MINOBSCOL,  MAX_INT)
	    call pr_puti (MAXOBSCOL, -MAX_INT)
	} else {
	    call pr_puti (MINOBSCOL, INDEFI)
	    call pr_puti (MAXOBSCOL, INDEFI)
	}
	if (pr_geti (NCATVARS) > 0) {
	    call pr_puti (MINCATCOL,  MAX_INT)
	    call pr_puti (MAXCATCOL, -MAX_INT)
	} else {
	    call pr_puti (MINCATCOL, INDEFI)
	    call pr_puti (MAXCATCOL, INDEFI)
	}

	# Build sequential tables from the parser symbol table,
	# and compute minimum and maximum column numbers.

	symtab = sthead (pr_getp (SYMTABLE))
	while (symtab != NULL) {

	    # Convert SYMTAB pointer into symbol offset.
	    sym = pr_offset (symtab)

	    # Get symbol type.
	    type = pr_gsymi (sym, PSYMTYPE)

	    # Check symbol type consistency, and enter each symbol in a
	    # sequential table acording with its type

	    switch (type) {
	    case PTY_OBSVAR:
		call mct_sputi (pr_getp (OBSTABLE), sym)
		incol = pr_gsymi (sym, PINPCOL)
		mincol = incol
		maxcol = incol
		errcol = pr_gsymi (sym, PINPERRCOL)
		if (! IS_INDEFI(errcol)) {
		    mincol = min (mincol, errcol)
		    maxcol = max (maxcol, errcol)
		}
		wtscol = pr_gsymi (sym, PINPWTSCOL)
		if (! IS_INDEFI(wtscol)) {
		    mincol = min (mincol, wtscol)
		    maxcol = max (maxcol, wtscol)
		}
		if (mincol < pr_geti (MINOBSCOL))
		    call pr_puti (MINOBSCOL, mincol)
		if (maxcol > pr_geti (MAXOBSCOL))
		    call pr_puti (MAXOBSCOL, maxcol)
	    case PTY_CATVAR:
		call mct_sputi (pr_getp (CATTABLE), sym)
		incol = pr_gsymi (sym, PINPCOL)
		mincol = incol
		maxcol = incol
		errcol = pr_gsymi (sym, PINPERRCOL)
		if (! IS_INDEFI(errcol)) {
		    mincol = min (mincol, errcol)
		    maxcol = max (maxcol, errcol)
		}
		wtscol = pr_gsymi (sym, PINPWTSCOL)
		if (! IS_INDEFI(wtscol)) {
		    mincol = min (mincol, wtscol)
		    maxcol = max (maxcol, wtscol)
		}
		if (mincol < pr_geti (MINCATCOL))
		    call pr_puti (MINCATCOL, mincol)
		if (maxcol > pr_geti (MAXCATCOL))
		    call pr_puti (MAXCATCOL, maxcol)
	    case PTY_FITPAR, PTY_CONST:
		call mct_sputi (pr_getp (PARTABLE), sym)
	    case PTY_SETEQ:
		call mct_sputi (pr_getp (SETTABLE), sym)
	    case PTY_EXTEQ:
		call mct_sputi (pr_getp (EXTTABLE), sym)
	    case PTY_TRNEQ:
		call mct_sputi (pr_getp (TRNTABLE), sym)
	    default:
		call sprintf (Memc[aux], SZ_LINE,
		    "pr_exit: unknown symbol type [%d] for [%d] [%s]")
		    call pargi (type)
		    call pargi (sym)
		    call pargstr (Memc[pr_xgetname (sym)])
		call error (0, Memc[aux])
	    }

	    # Advance to next SYMTAB symbol.
	    symtab = stnext (pr_getp (SYMTABLE), symtab)
	}

	# Check for input, error, and weight column duplications.
	call pr_excol (pr_getp (CATTABLE))
	call pr_excol (pr_getp (OBSTABLE))

	# Check transfomation equation deltas and derivatives.
	do i1 = 1, mct_nrows (pr_getp (TRNTABLE)) {

	    # Get equation symbol.
	    sym = mct_geti (pr_getp (TRNTABLE), i1, 1)

	    # Get number of parameters.
	    npar = pr_gsymi (sym, PTEQNPAR)

	    # Check if there are deltas and derivatives defined for the
	    # current equation. The code has been modified so that there
	    # will always be a defined PFITDELTA.

	    derflag = false
	    dltflag = false
	    do i2 = 1, npar {
		der = pr_gderp (sym, i2, PTEQRPNDER)
		if (der != NULL)
		    derflag = true
		par = pr_gpari (sym, i2, PTEQPAR)
		if (IS_INDEFI (par))
		    next
		if (IS_INDEFR (pr_gsymr (par, PFITDELTA))) {
		    call pr_psymr (par, PFITDELTA, DEF_PFITDELTA)
		 } else if (der != NULL) {
		    call sprintf (Memc[aux], SZ_LINE,
	"Parameter delta and derivative defined for [%s] in equation [%s]")
		       call pargstr (Memc[pr_xgetname (par)])
		       call pargstr (Memc[pr_xgetname (sym)])
		    call pr_error (Memc[aux], PERR_WARNING)
		}
		dltflag = true
	    }

	    # Continue with next equation if no deltas or derivatives are
	    # defined. This error check should now never be tripped since the
	    # code has been modified so that dltflag is always true.

	    if (! (derflag || dltflag) && (npar > 0)) {
		call sprintf (Memc[aux], SZ_LINE,
		"No parameter deltas or derivatives defined for equation [%s]")
		   call pargstr (Memc[pr_xgetname (sym)])
		call pr_error (Memc[aux], PERR_POSTPROC)
		next
	    }

	    # Loop over all fitting parameters of the equation.
	    # Comment out this code since there are  now reasonable defaults
	    # and eventually delete.

	    #do i2 = 1, npar {

		# Get parameter offset, parameter delta, and derivative
		# code pointer. Skip parameters that are not used in
		# the equation.

		#par = pr_gpari (sym, i2, PTEQPAR)
		#if (IS_INDEFI (par))
		    #next
	    	#delta = pr_gsymr (par, PFITDELTA)
		#der = pr_gderp (sym, i2, PTEQRPNDER)

		# Check for exclusion between deltas and derivatives,
		# missing derivative equations, and missing deltas.

		#if (!IS_INDEFR (delta) && der != NULL) {
		    #call sprintf (Memc[aux], SZ_LINE,
	#"Parameter delta and derivative defined for [%s] in equation [%s]")
		       #call pargstr (Memc[pr_xgetname (par)])
		       #call pargstr (Memc[pr_xgetname (sym)])
		    #call pr_error (Memc[aux], PERR_POSTPROC)
		#} else if (der == NULL && derflag) {
		    #call sprintf (Memc[aux], SZ_LINE,
		    #"Missing derivative for parameter [%s] in equation [%s]")
		       #call pargstr (Memc[pr_xgetname (par)])
		       #call pargstr (Memc[pr_xgetname (sym)])
		    #call pr_error (Memc[aux], PERR_POSTPROC)
		#} else if (IS_INDEFR (delta) && dltflag) {
		    #call sprintf (Memc[aux], SZ_LINE,
		    #"Missing delta for parameter [%s] in equation [%s]")
		       #call pargstr (Memc[pr_xgetname (par)])
		       #call pargstr (Memc[pr_xgetname (sym)])
		    #call pr_error (Memc[aux], PERR_POSTPROC)
		#}
	    #}
	}

	# Debug ?
	#if (clgetb ("debug.parcode"))
	    #call eprintf ("pr_exit.out\n")

	call sfree (sp)
end


# PR_EXCOL -- Check for input variable column duplications.

procedure pr_excol (table)

pointer	table			# table pointer

int	i1, i2, sym1, sym2, col1, col2, errcol1, errcol2, wtscol1, wtscol2
pointer	sp, aux
int	mct_nrows(), mct_geti(), pr_gsymi()
pointer	pr_xgetname()

begin
	call smark (sp)
	call salloc (aux, SZ_LINE, TY_CHAR)

	do i1 = 1, mct_nrows (table) - 1 {

	    # Get first symbol columns.
	    sym1    = mct_geti (table, i1, 1)
	    col1    = pr_gsymi (sym1, PINPCOL)
	    errcol1 = pr_gsymi (sym1, PINPERRCOL)
	    wtscol1 = pr_gsymi (sym1, PINPWTSCOL)

	    # Skip spare variable.
	    if (pr_gsymi (sym1, PINPSPARE) == YES)
		next

	    # Check the first symbol against itself.
	    if ((!IS_INDEFI (errcol1) && (col1 == errcol1)) ||
		(!IS_INDEFI (wtscol1) && (col1 == wtscol1)) ||
		(!IS_INDEFI (errcol1) && !IS_INDEFI (wtscol1) &&
		(errcol1 == wtscol1))) {
		call sprintf (Memc[aux], SZ_LINE,
		    "Duplicate column for input variable [%s]")
		    call pargstr (Memc[pr_xgetname (sym1)])
		call pr_error (Memc[aux], PERR_WARNING)
	    }

	    # Compare the first symbol against all others in the table.
	    do i2 = i1 + 1, mct_nrows (table) {

		# Get second symbol columns.
		sym2    = mct_geti (table, i2, 1)
		col2    = pr_gsymi (sym2, PINPCOL)
		errcol2 = pr_gsymi (sym2, PINPERRCOL)
		wtscol2 = pr_gsymi (sym2, PINPWTSCOL)

	        # Skip spare variable.
	        if (pr_gsymi (sym2, PINPSPARE) == YES)
		    next

		# Check first symbol against the second symbol.
		if ((col1 == col2)                                     ||
		    #(!IS_INDEFI (errcol2) && (col1 == errcol2))        ||
		    #(!IS_INDEFI (wtscol2) && (col1 == wtscol2))        ||
		    #(!IS_INDEFI (errcol1) && (col2 == errcol1))        ||
		    #(!IS_INDEFI (wtscol1) && (col2 == wtscol1))        ||
		    (!IS_INDEFI (errcol1) && !IS_INDEFI (errcol2) &&
		    (errcol1 == errcol2))                              ||
		    (!IS_INDEFI (wtscol1) && !IS_INDEFI (wtscol2) &&
		    (wtscol1 == wtscol2))                              ||
		    (!IS_INDEFI (errcol1) && !IS_INDEFI (wtscol2) &&
		    (errcol1 == wtscol2))                              ||
		    (!IS_INDEFI (errcol2) && !IS_INDEFI (wtscol1) &&
		    (errcol2 == wtscol1))) {
		    call sprintf (Memc[aux], SZ_LINE,
		        "Duplicate column for input variables [%s] and [%s]")
			call pargstr (Memc[pr_xgetname (sym1)])
			call pargstr (Memc[pr_xgetname (sym2)])
		    call pr_error (Memc[aux], PERR_WARNING)
		}
	    }
	}

	call sfree (sp)
end
