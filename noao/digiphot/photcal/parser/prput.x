.help prput
Low Level Parser Storage

These procedures store parameters (attributes) to the parser, symbols,
symbol variables, symbol derivatives, and symbol fitting parameters, in
the parser common, and the parser symbol table. 
.sp
These should be the ONLY procedures that access the parser common and
symbol table directly, by using the macro definitions in prtable.h.
All other procedures should try to use these procedures as the entry
point to access any common or symbol table parameter.

.nf
Entry points:

	int = pr_putsym (name)			Put symbol into table

	pr_psym (number, type, value)		Put symbol pointer by number,
						and type

	pr_put[ip] (param, value)		Put general parameter

	pr_inc[i] (param, value)		Increment general parameter
	pr_dec[i] (param, value)		Decrement general parameter

	pr_psym[cirp] (offset, param, value)	Put symbol parameter

	pr_pvar[i]  (offset, nv, param, value)	Put variable parameter
	pr_ppar[ir] (offset, np, param, value)	Put fitting parameter parameter
	pr_pder[cp] (offset, nd, param, value)	Put derivative parameter
.fi
.endhelp

include	"../lib/parser.h"
include	"../lib/prstruct.h"


# PR_PUTSYM -- Put initialized symbol in the symbol table. Does not check
# for prevoius existence of the identifier in the table.

int procedure pr_putsym (name)

char	name[ARB]		# identifier name

int	offset
pointer	sym

include	"parser.com"

int	pr_offset()
pointer	stenter()

begin
	# Enter symbol into table
	sym = stenter (symtable, name, LEN_PSYM)

	# Get symbol offset
	offset = pr_offset (sym)

	# Initialize numbers to INDEF and pointers to NULL
	call pr_psymi (offset, PSYMTYPE, INDEFI)
	call pr_psymi (offset, PSYMNUM,  INDEFI)
	call pr_psymp (offset, PSYMSUB,  NULL)

	# Return symbol offset
	return (offset)
end


# PR_PSYM -- Put symbol pointer by number, and type.

procedure pr_psym (number, type, value)

int	number			# number
int	type			# type
pointer	value			# value

int	 aux

include	"parser.com"

int	mct_nrows()

begin
	# Branch on parameter type
	switch (type) {
	case PTY_OBSVAR:
	    aux = mct_nrows (obstable) - number + 1
	    call mct_putp (obstable, aux, 1, value)

	case PTY_CATVAR:
	    aux = mct_nrows (cattable) - number + 1
	    call mct_putp (cattable, aux, 1, value)

	case PTY_FITPAR, PTY_CONST:
	    aux = mct_nrows (partable) - number + 1
	    #call mct_getp (partable, aux, 1, value)
	    call mct_putp (partable, aux, 1, value)

	case PTY_SETEQ:
	    aux = mct_nrows (settable) - number + 1
	    call mct_putp (settable, aux, 1, value)

	case PTY_EXTEQ:
	    aux = mct_nrows (exttable) - number + 1
	    call mct_putp (exttable, aux, 1, value)

	case PTY_TRNEQ:
	    aux = mct_nrows (trntable) - number + 1
	    call mct_putp (trntable, aux, 1, value)

	default:
	    call error (type, "pr_psym: Unknown parameter")
	}
end


# PR_PUTI -- Put parser integer parameter.

procedure pr_puti (param, value)

int	param			# parameter
int	value			# value

include	"parser.com"

begin
	# Brach on parameter type
	switch (param) {
	case NERRORS:
	    nerrors = value

	case NWARNINGS:
	    nwarnings = value

	case NOBSVARS:
	    nobsvars = value

	case NCATVARS:
	    ncatvars = value

	case NFITPARS:
	    nfitpars = value

	case NTOTPARS:
	    ntotpars = value

	case NSETEQS:
	    nseteqs = value

	case NEXTEQS:
	    nexteqs = value

	case NTRNEQS:
	    ntrneqs = value

	case MINCOL:
	    mincol = value

	case MINOBSCOL:
	    minobscol = value

	case MAXOBSCOL:
	    maxobscol = value

	case MINCATCOL:
	    mincatcol = value

	case MAXCATCOL:
	    maxcatcol = value

	case FLAGEQSECT:
	    flageqsect = value

	case FLAGERRORS:
	    flagerrors = value

	default:
	    call error (param, "pr_puti: Unknown parameter")
	}
end


# PR_INCI -- Increment parser integer parameter.

procedure pr_inci (param, value)

int	param			# parameter
int	value			# value

include	"parser.com"

begin
	# Brach on parameter type
	switch (param) {
	case NERRORS:
	    nerrors = nerrors + value

	case NWARNINGS:
	    nwarnings = nwarnings + value

	case NOBSVARS:
	    nobsvars = nobsvars + value

	case NCATVARS:
	    ncatvars = ncatvars + value

	case NFITPARS:
	    nfitpars = nfitpars + value

	case NTOTPARS:
	    ntotpars = ntotpars + value

	case NSETEQS:
	    nseteqs = nseteqs + value

	case NEXTEQS:
	    nexteqs = nexteqs + value

	case NTRNEQS:
	    ntrneqs = ntrneqs + value

	case MINOBSCOL:
	    minobscol = minobscol + value

	case MAXOBSCOL:
	    maxobscol = maxobscol + value

	case MINCATCOL:
	    mincatcol = mincatcol + value

	case MAXCATCOL:
	    maxcatcol = maxcatcol + value

	default:
	    call error (param, "pr_inci: Unknown parameter")
	}
end


# PR_DECI -- Decrement parser integer parameter.

procedure pr_deci (param, value)

int	param			# parameter
int	value			# value

include	"parser.com"

begin
	# Brach on parameter type
	switch (param) {
	case NERRORS:
	    nerrors = nerrors - value

	case NWARNINGS:
	    nwarnings = nwarnings - value

	case NOBSVARS:
	    nobsvars = nobsvars - value

	case NCATVARS:
	    ncatvars = ncatvars - value

	case NFITPARS:
	    nfitpars = nfitpars - value

	case NTOTPARS:
	    ntotpars = ntotpars - value

	case NSETEQS:
	    nseteqs = nseteqs - value

	case NEXTEQS:
	    nexteqs = nexteqs - value

	case NTRNEQS:
	    ntrneqs = ntrneqs - value

	case MINOBSCOL:
	    minobscol = minobscol - value

	case MAXOBSCOL:
	    maxobscol = maxobscol - value

	case MINCATCOL:
	    mincatcol = mincatcol - value

	case MAXCATCOL:
	    maxcatcol = maxcatcol - value

	default:
	    call error (param, "pr_deci: Unknown parameter")
	}
end


# PR_PUTP -- Put parser pointer parameter.

procedure pr_putp (param, value)

int	param			# parameter
pointer	value			# value

include	"parser.com"

begin
	# Brach on parameter type
	switch (param) {
	case SYMTABLE:
	    symtable = value

	case OBSTABLE:
	    obstable = value

	case CATTABLE:
	    cattable = value

	case PARTABLE:
	    partable = value

	case SETTABLE:
	    settable = value

	case EXTTABLE:
	    exttable = value

	case TRNTABLE:
	    trntable = value

	case TRCATTABLE:
	    trcattable = value

	case TROBSTABLE:
	    trobstable = value

	case TFCATTABLE:
	    tfcattable = value

	case TFOBSTABLE:
	    tfobstable = value

	case TPARTABLE:
	    tpartable = value

	default:
	    call error (param, "pr_putp: Unknown parameter")
	}
end


# PR_PSYMC -- Put a symbol character string attribute.

procedure pr_psymc (offset, param, value)

int	offset			# symbol offset
int	param			# parameter
char	value[ARB]		# value

pointer	sym

include	"parser.com"

int	stpstr()
pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_psymc: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PSEQEQ:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_EQ (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQEQ)")

	case PSEQERROR:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_ERROR (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQERROR)")

	case PSEQERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_ERRMIN (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQERRMIN)")

	case PSEQERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_ERRMAX (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQERRMAX)")

	case PSEQWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_WEIGHT (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQWEIGHT)")

	case PSEQWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_WTSMIN (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQWTSMIN)")

	case PSEQWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_WTSMAX (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PSEQWTSMAX)")

	case PTEQFIT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_FIT (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQFIT)")

	case PTEQREF:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_REF (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQREF)")

	case PTEQERROR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_ERROR (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQERROR)")

	case PTEQERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_ERRMIN (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQERRMIN)")

	case PTEQERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_ERRMAX (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQERRMAX)")

	case PTEQWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_WEIGHT (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQWEIGHT)")

	case PTEQWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_WTSMIN (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQWTSMIN)")

	case PTEQWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_WTSMAX (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQWTSMAX)")

	case PTEQXPLOT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_XPLOT (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQXPLOT)")

	case PTEQYPLOT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_YPLOT (PSYM_SUB (sym)) = stpstr (symtable, value, 0)
	    else
		call error (0, "pr_psymc: Null equation pointer (PTEQYPLOT)")

	default:
	    call error (param, "pr_psymc: Unknown parameter type")
	}
end


# PR_PSYMI -- Put symbol integer parameter.

procedure pr_psymi (offset, param, value)

int	offset			# symbol offset
int	param			# parameter
int	value			# value

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_psymi: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PSYMTYPE:
	    PSYM_TYPE (sym) = value

	case PSYMNUM:
	    PSYM_NUM (sym) = value

	case PINPCOL:
	    if (PSYM_SUB (sym) != NULL)
		PINP_COL (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PINPCOL)")

	case PINPERRCOL:
	    if (PSYM_SUB (sym) != NULL)
		PINP_ERRCOL (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PINPERRCOL)")

	case PINPWTSCOL:
	    if (PSYM_SUB (sym) != NULL)
		PINP_WTSCOL (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PINPWTSCOL)")

	case PINPSPARE:
	    if (PSYM_SUB (sym) != NULL)
		PINP_SPARE (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PINPSPARE)")

	case PTEQNRCAT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NRCAT (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNRCAT)")

	case PTEQNROBS:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NROBS (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNROBS)")

	case PTEQNRVAR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NRVAR (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNRVAR)")

	case PTEQNFCAT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NFCAT (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNFCAT)")

	case PTEQNFOBS:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NFOBS (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNFOBS)")

	case PTEQNFVAR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NFVAR (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNFVAR)")

	case PTEQNVAR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NVAR (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNVAR)")

	case PTEQNPAR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NPAR (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNPAR)")

	case PTEQNFPAR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_NFPAR (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymi: Null equation pointer (PTEQNFPAR)")

	default:
	    call error (param, "pr_psymi: Unknown parameter")
	}
end


# PR_PSYMR -- Put symbol real parameter.

procedure pr_psymr (offset, param, value)

int	offset			# symbol offset
int	param			# parameter
real	value			# value

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_psymr: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PFITVALUE:
	    if (PSYM_SUB (sym) != NULL)
		PFIT_VALUE (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymr: Null equation pointer (PFITVALUE)")

	case PFITDELTA:
	    if (PSYM_SUB (sym) != NULL)
		PFIT_DELTA (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymr: Null equation pointer (PFITDELTA)")

	default:
	    call error (param, "pr_psymr: Unknown parameter")
	}
end


# PR_PSYMP -- Put symbol pointer parameter.

procedure pr_psymp (offset, param, value)

int	offset			# symbol offset
int	param			# parameter
pointer	value			# value

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_psymp: Null symbol pointer")

	# Brach on parameter type
	switch (param) {
	case PSYMSUB:
	    PSYM_SUB (sym) = value

	case PSEQRPNEQ:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNEQ (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymp: Null equation pointer (PSEQRPNEQ)")

	case PSEQRPNERROR:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNERROR (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PSEQRPNERROR)")

	case PSEQRPNERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNERRMIN (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PSEQRPNERRMIN)")

	case PSEQRPNERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNERRMAX (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PSEQRPNERRMAX)")

	case PSEQRPNWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNWEIGHT (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PSEQRPNWEIGHT")

	case PSEQRPNWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNWTSMIN (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PSEQRPNWTSMIN)")

	case PSEQRPNWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		PSEQ_RPNWTSMAX (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PSEQRPNWTSMAX)")

	case PTEQRPNFIT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNFIT (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymp: Null equation pointer (PTEQRPNFIT)")

	case PTEQRPNREF:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNREF (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymp: Null equation pointer (PTEQRPNREF)")

	case PTEQRPNERROR:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNERROR (PSYM_SUB (sym)) = value
	    else
		call error (0, "pr_psymp: Null equation pointer (PTEQRPNERROR)")

	case PTEQRPNERRMIN:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNERRMIN (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNERRMIN)")

	case PTEQRPNERRMAX:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNERRMAX (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNERRMAX)")

	case PTEQRPNWEIGHT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNWEIGHT (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNWEIGHT")

	case PTEQRPNWTSMIN:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNWTSMIN (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNWTSMIN)")

	case PTEQRPNWTSMAX:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNWTSMAX (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNWTSMAX")

	case PTEQRPNXPLOT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNXPLOT (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNXPLOT)")

	case PTEQRPNYPLOT:
	    if (PSYM_SUB (sym) != NULL)
		PTEQ_RPNYPLOT (PSYM_SUB (sym)) = value
	    else
		call error (0,
		    "pr_psymp: Null equation pointer (PTEQRPNYPLOT)")

	default:
	    call error (param, "pr_psymp: Unknown parameter")
	}
end


# PR_PVARI -- Put variable integer parameter.

procedure pr_pvari (offset, nv, param, value)

int	offset			# symbol offset
int	nv			# variable number
int	param			# parameter
int	value			# value

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_pvari: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQREFVAR:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_REFVAR (PSYM_SUB (sym), nv) = value
		else
		    call error (0,
		        "pr_pvari: Not a valid variable number (PTEQREFVAR)")
	    } else
		call error (0, "pr_pvari: Null equation pointer (PTEQREFVAR)")

	case PTEQREFCNT:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_REFCNT (PSYM_SUB (sym), nv) = value
		else
		    call error (0,
		        "pr_pvari: Not a valid variable number (PTEQREFCNT)")
	    } else
		call error (0, "pr_pvari: Null equation pointer (PTEQREFCNT)")

	case PTEQFITVAR:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_FITVAR (PSYM_SUB (sym), nv) = value
		else
		    call error (0,
		        "pr_pvari: Not a valid variable number (PTEQFITVAR)")
	    } else
		call error (0, "pr_pvari: Null equation pointer (PTEQFITVAR)")

	case PTEQFITCNT:
	    if (PSYM_SUB (sym) != NULL) {
		if (nv >= 1 || nv <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_FITCNT (PSYM_SUB (sym), nv) = value
		else
		    call error (0,
		        "pr_pvari: Not a valid variable number (PTEQFITCNT)")
	    } else
		call error (0, "pr_pvari: Null equation pointer (PTEQFITCNT)")

	default:
	    call error (param, "pr_pvari: Unknown parameter")
	}
end


# PR_PPARI -- Put fitting parameter integer parameter.

procedure pr_ppari (offset, np, param, value)

int	offset			# symbol offset
int	np			# parameter number
int	param			# parameter
int	value			# value

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_ppari: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQPAR:
	    if (PSYM_SUB (sym) != NULL) {
		if (np >= 1 || np <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_PAR (PSYM_SUB (sym), np) = value
		else
		    call error (0,
		        "pr_ppari: Not a valid variable number (PTEQPAR)")
	    } else
		call error (0, "pr_ppari: Null equation pointer (PTEQPAR)")

	case PTEQPLIST:
	    if (PSYM_SUB (sym) != NULL) {
		if (np >= 1 || np <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_PLIST (PSYM_SUB (sym), np) = value
		else
		    call error (0,
		        "pr_ppari: Not a valid variable number (PTEQPLIST)")
	    } else
		call error (0, "pr_ppari: Null equation pointer (PTEQPLIST)")

	default:
	    call error (param, "pr_ppari: Unknown parameter")
	}
end


# PR_PPARR -- Put fitting parameter real parameter.

procedure pr_pparr (offset, np, param, value)

int	offset			# symbol offset
int	np			# parameter number
int	param			# parameter
real	value			# value

pointer	sym

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_pparr: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQPARVAL:
	    if (PSYM_SUB (sym) != NULL) {
		if (np >= 1 || np <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_PARVAL (PSYM_SUB (sym), np) = value
		else
		    call error (0,
		        "pr_pparr: Not a valid variable number (PTEQPARVAL)")
	    } else
		call error (0, "pr_pparr: Null equation pointer (PTEQPARVAL)")

	default:
	    call error (param, "pr_pparr: Unknown parameter")
	}
end


# PR_PDERC -- Put derivative character string parameter.

procedure pr_pderc (offset, nd, param, value)

int	offset			# symbol offset
int	nd			# derivative number
int	param			# parameter
char	value[ARB]		# value

pointer	sym

include	"parser.com"

int	stpstr()
pointer	pr_pointer

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_pderc: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQDER:
	    if (PSYM_SUB (sym) != NULL) {
		if (nd >= 1 || nd <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_DER (PSYM_SUB (sym), nd) = stpstr (symtable, value, 0)
		else
		    call error (0,
		        "pr_pderc: Not a valid variable number (PTEQDER)")
	    } else
		call error (param, "pr_pderc: Null equation pointer (PTEQDER)")

	default:
	    call error (param, "pr_pderc: Unknown parameter")
	}
end


# PR_PDERP -- Put derivative pointer parameter.

procedure pr_pderp (offset, nd, param, value)

int	offset			# symbol offset
int	nd			# derivative number
int	param			# parameter
pointer	value			# value

pointer	sym

include	"parser.com"

pointer	pr_pointer()

begin
	# Get symbol pointer
	sym = pr_pointer (offset)

	# Check symbol pointer
	if (sym == NULL)
	    call error (0, "pr_pderp: Null symbol pointer")

	# Branch on parameter
	switch (param) {
	case PTEQRPNDER:
	    if (PSYM_SUB (sym) != NULL) {
		if (nd >= 1 || nd <= PTEQ_NVAR (PSYM_SUB (sym)))
		    PTEQ_RPNDER (PSYM_SUB (sym), nd) = value
		else
		    call error (0,
		        "pr_pderp: Not a valid derivative number (PTEQRPNDER)")
	    } else
		call error (0, "pr_pderp: Null equation pointer (PTEQRPNDER)")

	default:
	    call error (param, "pr_pderp: Unknown parameter")
	}
end
