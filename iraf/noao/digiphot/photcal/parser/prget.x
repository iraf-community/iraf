.help prget
Low Level Parser Retrieval

These procedures retrieve parameters (attributes) from the parser, symbols,
symbol variables, symbol derivatives, and symbol fitting parameters, stored
in the parser common, and the parser symbol table. 
.sp
These should be the ONLY procedures that access the parser common and symbol
table directly, by using the macro definitions in prtable.h.
All other procedures should try to use these procedures as the entry point to
access any common or symbol table parameter.

.nf
Entry points:

	int 	= pr_getsym (name)		Get symbol from name

	pointer = pr_xgetname (offset)		Get charp. from symbol pointer

	int	= pr_gsym (number, type)	Get symbol from number and type

	value	= pr_get[ip] (param)		Get general integer parameter

	value	= pr_gsym[cirp] (offset, param)   Get symbol parameter

	value	= pr_gvar[i]  (offset, nv, param) Get variable parameter
	value	= pr_gpar[ir] (offset, np, param) Get fitting param. parameter
	value	= pr_gder[cp] (offset, nd, param) Get derivative parameter

.fi
.endhelp

include	"../lib/parser.h"
include	"../lib/prstruct.h"


# PR_GETSYM -- Get symbol offset from symbol name.

int procedure pr_getsym (name)

char	name[ARB]		# symbol name

include	"parser.com"

int	pr_offset()
pointer	stfind()

begin
	# Return symbol pointer
	return (pr_offset (stfind (symtable, name)))
end


# PR_XGETNAME -- Get symbol character pointer from symbol offset. The 'X'
# is necessary to remove a system library symbol collision.

pointer procedure pr_xgetname (offset)

pointer offset			# symbol offset

pointer	sym

include	"parser.com"

pointer	stname()
pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym != NULL)
	    return (stname (symtable, sym))
	else
	    return (NULL)
end


# PR_GSYM -- Get symbol by number, and type.

int procedure pr_gsym (number, type)

int	number			# quantity number
int	type			# type

int	 aux

include	"parser.com"

int	mct_nrows()
int	mct_geti()

begin
	# Brach on parameter type
	switch (type) {
	case PTY_OBSVAR:
	    aux = mct_nrows (obstable) - number + 1
	    return (mct_geti (obstable, aux, 1))

	case PTY_CATVAR:
	    aux = mct_nrows (cattable) - number + 1
	    return (mct_geti (cattable, aux, 1))

	case PTY_FITPAR, PTY_CONST:
	    aux = mct_nrows (partable) - number + 1
	    return (mct_geti (partable, aux, 1))

	case PTY_SETEQ:
	    aux = mct_nrows (settable) - number + 1
	    return (mct_geti (settable, aux, 1))

	case PTY_EXTEQ:
	    aux = mct_nrows (exttable) - number + 1
	    return (mct_geti (exttable, aux, 1))

	case PTY_TRNEQ:
	    aux = mct_nrows (trntable) - number + 1
	    return (mct_geti (trntable, aux, 1))

	default:
	    call error (type, "pr_gsym: Unknown parameter")
	}
end


# PR_GETI -- Get parser integer parameter.

int procedure pr_geti (param)

int	param			# parameter

include	"parser.com"

begin
	# Brach on parameter type
	switch (param) {
	case NERRORS:
	    return (nerrors)

	case NWARNINGS:
	    return (nwarnings)

	case NOBSVARS:
	    return (nobsvars)

	case NCATVARS:
	    return (ncatvars)

	case NFITPARS:
	    return (nfitpars)

	case NTOTPARS:
	    return (ntotpars)

	case NSETEQS:
	    return (nseteqs)

	case NEXTEQS:
	    return (nexteqs)

	case NTRNEQS:
	    return (ntrneqs)

	case MINCOL:
	    return (mincol)

	case MINOBSCOL:
	    return (minobscol)

	case MAXOBSCOL:
	    return (maxobscol)

	case MINCATCOL:
	    return (mincatcol)

	case MAXCATCOL:
	    return (maxcatcol)

	case FLAGEQSECT:
	    return (flageqsect)

	case FLAGERRORS:
	    return (flagerrors)

	default:
	    call error (param, "pr_geti: Unknown parameter")
	}
end


# PR_GETP -- Get parser pointer parameter.

pointer procedure pr_getp (param)

int	param			# parameter

include	"parser.com"

begin
	# Brach on parameter type
	switch (param) {
	case SYMTABLE:
	    return (symtable)

	case OBSTABLE:
	    return (obstable)

	case CATTABLE:
	    return (cattable)

	case PARTABLE:
	    return (partable)

	case SETTABLE:
	    return (settable)

	case EXTTABLE:
	    return (exttable)

	case TRNTABLE:
	    return (trntable)

	case TRCATTABLE:
	    return (trcattable)

	case TROBSTABLE:
	    return (trobstable)

	case TFCATTABLE:
	    return (tfcattable)

	case TFOBSTABLE:
	    return (tfobstable)

	case TPARTABLE:
	    return (tpartable)

	default:
	    call error (param, "pr_getp: Unknown parameter")
	}
end


# PR_GSYMC -- Get symbol character pointer parameter.

pointer procedure pr_gsymc (offset, param)

int	offset			# symbol offset
int	param			# parameter

pointer	sym

pointer	pr_pointer(), pr_charp()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gsymc: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PSEQEQ:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_EQ (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQEQ)")

	case PSEQERROR:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_ERROR (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQERROR)")

	case PSEQERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_ERRMIN (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQERRMIN)")

	case PSEQERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_ERRMAX (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQERRMAX)")

	case PSEQWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_WEIGHT (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQWEIGHT)")

	case PSEQWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_WTSMIN (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQWTSMIN)")

	case PSEQWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PSEQ_WTSMAX (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PSEQWTSMAX)")

	case PTEQFIT:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_FIT (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQFIT)")

	case PTEQREF:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_REF (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQREF)")

	case PTEQERROR:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_ERROR (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQERROR)")

	case PTEQERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_ERRMIN (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQERRMIN)")

	case PTEQERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_ERRMAX (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQERRMAX)")

	case PTEQWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_WEIGHT (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQWEIGHT)")

	case PTEQWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_WTSMIN (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQWTSMIN)")

	case PTEQWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_WTSMAX (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQWTSMAX)")

	case PTEQXPLOT:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_XPLOT (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQXPLOT)")

	case PTEQYPLOT:
	    if (PSYM_SUB (sym) != NULL)
		return (pr_charp (PTEQ_YPLOT (PSYM_SUB (sym))))
	    else
		call error (0, "pr_gsymc: Null equation pointer (PTEQYPLOT)")

	default:
	    call error (param, "pr_gsymc: Unknown parameter")
	}
end


# PR_GSYMI -- Get symbol integer parameter.

int procedure pr_gsymi (offset, param)

int	offset			# symbol offset
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gsymi: Null symbol offset")

	# Brach on parameter type
	switch (param) {
	case PSYMTYPE:
	    return (PSYM_TYPE (sym))

	case PSYMNUM:
	    return (PSYM_NUM (sym))

	case PINPCOL:
	    if (PSYM_SUB (sym) != NULL)
		return (PINP_COL (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PINPCOL)")

	case PINPERRCOL:
	    if (PSYM_SUB (sym) != NULL)
		return (PINP_ERRCOL (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PINPERRCOL)")

	case PINPWTSCOL:
	    if (PSYM_SUB (sym) != NULL)
		return (PINP_WTSCOL (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PINPWTSCOL)")

	case PINPSPARE:
	    if (PSYM_SUB (sym) != NULL)
		return (PINP_SPARE (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PINPSPARE)")

	case PTEQNRCAT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NRCAT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNRCAT)")

	case PTEQNROBS:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NROBS (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNROBS)")

	case PTEQNRVAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NRVAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNRVAR)")

	case PTEQNFCAT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NFCAT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNFCAT)")

	case PTEQNFOBS:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NFOBS (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNFOBS)")

	case PTEQNFVAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NFVAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNFVAR)")

	case PTEQNVAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NVAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNVAR)")

	case PTEQNPAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NPAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNPAR)")

	case PTEQNFPAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_NFPAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymi: Null equation pointer (PTEQNFPAR)")

	default:
	    call error (param, "pr_gsymi: Unknown parameter")
	}
end


# PR_GSYMR -- Get symbol real parameter.

real procedure pr_gsymr (offset, param)

pointer	offset			# symbol offset
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gsymr: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PFITVALUE:
	    if (PSYM_SUB (sym) != NULL)
		return (PFIT_VALUE (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymr: Null equation pointer (PFITVALUE)")

	case PFITDELTA:
	    if (PSYM_SUB (sym) != NULL)
		return (PFIT_DELTA (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymr: Null equation pointer (PFITDELTA)")

	default:
	    call error (param, "pr_gsymr: Unknown parameter")
	}
end


# PR_GSYMP -- Get symbol pointer parameter.

pointer procedure pr_gsymp (offset, param)

int	offset			# symbol offset
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gsymp: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PSYMSUB:
	    return (PSYM_SUB (sym))

	case PSEQRPNEQ:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNEQ (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PSEQRPNEQ)")

	case PSEQRPNERROR:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNERROR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PSEQRPNERROR)")

	case PSEQRPNERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNERRMIN (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PSEQRPNERRMIN)")

	case PSEQRPNERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNERRMAX (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PSEQRPNERRMAX)")

	case PSEQRPNWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNWEIGHT (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PSEQRPNWEIGHT)")

	case PSEQRPNWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNWTSMIN (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PSEQRPNWTSMIN)")

	case PSEQRPNWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (PSEQ_RPNWTSMAX (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PSEQRPNWTSMAX")

	case PTEQRPNFIT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNFIT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQRPNFIT)")

	case PTEQRPNREF:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNREF (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQRPNREF)")

	case PTEQRPNERROR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNERROR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQRPNERROR)")

	case PTEQRPNERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNERRMIN (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PTEQRPNERRMIN)")

	case PTEQRPNERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNERRMAX (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PTEQRPNERRMAX)")

	case PTEQRPNWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNWEIGHT (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PTEQRPNWEIGHT)")

	case PTEQRPNWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNWTSMIN (PSYM_SUB (sym)))
	    else
		call error (0,
		    "pr_gsymp: Null equation pointer (PTEQRPNWTSMIN)")

	case PTEQRPNWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNWTSMAX (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQRPNWTSMAX")

	case PTEQRPNXPLOT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNXPLOT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQRPNXPLOT)")

	case PTEQRPNYPLOT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_RPNYPLOT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQRPNYPLOT)")

	case PTEQSREFVAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SREFVAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSREFVAR)")

	case PTEQSREFCNT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SREFCNT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSREFCNT)")

	case PTEQSFITVAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SFITVAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSFITVAR)")

	case PTEQSFITCNT:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SFITCNT (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSFITCNT)")

	case PTEQSPAR:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SPAR (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSPAR)")

	case PTEQSPARVAL:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SPARVAL (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSPARVAL)")

	case PTEQSPLIST:
	    if (PSYM_SUB (sym) != NULL)
		return (PTEQ_SPLIST (PSYM_SUB (sym)))
	    else
		call error (0, "pr_gsymp: Null equation pointer (PTEQSPLIST)")

	default:
	    call error (param, "pr_gsymp: Unknown parameter")
	}
end


# PR_GVARI -- Get variable integer parameter.

int procedure pr_gvari (offset, nv, param)

int	offset			# variable offset
int	nv			# variable number
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gvari: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQREFVAR:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    return (PTEQ_REFVAR (PSYM_SUB (sym), nv))
		else
		    call error (0,
		        "pr_gvari: Not a valid parameter number (PTEQREFVAR)")
	    } else
		call error (0, "pr_gvari: Null equation pointer (PTEQREFVAR)")

	case PTEQREFCNT:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    return (PTEQ_REFCNT (PSYM_SUB (sym), nv))
		else
		    call error (0,
		        "pr_gvari: Not a valid parameter number (PTEQREFCNT)")
	    } else
		call error (0, "pr_gvari: Null equation pointer (PTEQREFCNT)")

	case PTEQFITVAR:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    return (PTEQ_FITVAR (PSYM_SUB (sym), nv))
		else
		    call error (0,
		        "pr_gvari: Not a valid parameter number (PTEQFITVAR)")
	    } else
		call error (0, "pr_gvari: Null equation pointer (PTEQFITVAR)")

	case PTEQFITCNT:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    return (PTEQ_FITCNT (PSYM_SUB (sym), nv))
		else
		    call error (0,
		        "pr_gvari: Not a valid parameter number (PTEQFITCNT)")
	    } else
		call error (0, "pr_gvari: Null equation pointer (PTEQFITCNT)")

	default:
	    call error (param, "pr_gvari: Unknown parameter")
	}
end


# PR_GPARI -- Get fitting parameter integer parameter.

int procedure pr_gpari (offset, np, param)

int	offset			# symbol offset
int	np			# parameter number
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gpari: Null symbol pointer")

	# Brach on parameter
	switch (param) {
	case PTEQPAR:
	    if (PSYM_SUB (sym) != NULL) {
		if (np >= 1 || np <= PTEQ_NPAR (PSYM_SUB (sym)))
		    return (PTEQ_PAR (PSYM_SUB (sym), np))
		else
		    call error (0, "pr_gpari: Not a valid parameter number (PTEQPAR)")
	    } else
		call error (0, "pr_gpari: Null equation pointer (PTEQPAR)")

	case PTEQPLIST:
	    if (PSYM_SUB (sym) != NULL) {
		if (np >= 1 || np <= PTEQ_NPAR (PSYM_SUB (sym)))
		    return (PTEQ_PLIST (PSYM_SUB (sym), np))
		else
		    call error (0, "pr_gpari: Not a valid parameter number (PTEQPLIST)")
	    } else
		call error (0, "pr_gpari: Null equation pointer (PTEQPLIST)")

	default:
	    call error (param, "pr_gpari: Unknown parameter")
	}
end


# PR_GPARR -- Get fitting parameter real parameter.

real procedure pr_gparr (offset, np, param)

int	offset			# symbol offset
int	np			# parameter number
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gparr: Null symbol pointer")

	# Brach on parameter
	switch (param) {
	case PTEQPARVAL:
	    if (PSYM_SUB (sym) != NULL) {
		if (np >= 1 || np <= PTEQ_NPAR (PSYM_SUB (sym)))
		    return (PTEQ_PARVAL (PSYM_SUB (sym), np))
		else
		    call error (0, "pr_gparr: Not a valid parameter number (PTEQPARVAL)")
	    } else
		call error (0, "pr_gparr: Null equation pointer (PTEQPARVAL)")

	default:
	    call error (param, "pr_gparr: Unknown parameter")
	}
end


# PR_GDERC -- Get derivative character pointer parameter.

pointer procedure pr_gderc (offset, nd, param)

pointer	offset			# symbol offset
int	nd			# derivative number
int	param			# parameter

pointer sym

pointer	pr_pointer(), pr_charp()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gderc: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQDER:
	    if (PSYM_SUB (sym) != NULL) {
		if (nd >= 1 || nd <= PTEQ_NPAR (PSYM_SUB (sym)))
		    return (pr_charp (PTEQ_DER (PSYM_SUB (sym), nd)))
		else
	    	    call error (0, "pr_gderc: Not a valid derivative number (PTEQDER)")
	    } else
		call error (0, "pr_gderc: Null equation pointer (PTEQDER)")

	default:
	    call error (param, "pr_gderc: Unknown parameter")
	}
end


# PR_GDERP -- Get derivative pointer parameter.

pointer procedure pr_gderp (offset, nd, param)

int	offset			# symbol offset
int	nd			# derivative number
int	param			# parameter

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_gderp: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQRPNDER:
	    if (PSYM_SUB (sym) != NULL) {
		if (nd >= 1 || nd <= PTEQ_NPAR (PSYM_SUB (sym)))
		    return (PTEQ_RPNDER (PSYM_SUB (sym), nd))
		else
	    	    call error (0, "pr_gderp: Not a valid derivative number (PTEQRPNDER)")
	    } else
		call error (0, "pr_gderp: Null equation pointer (PTEQRPNDER)")

	default:
	    call error (param, "pr_gderp: Unknown parameter")
	}
end
