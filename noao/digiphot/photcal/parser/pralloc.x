.help pralloc
Parser Memory Allocation.

Entry points:

	pr_alloc ()			Allocate parser tables.

	pr_inalloc (ptr)		Allocate input variable substructure.
	pr_ftalloc (ptr)		Allocate fitting parameter substructure.
	pr_stalloc (ptr)		Allocate set equation substructure.
	pr_tralloc (ptr, npars)		Allocate transf. equation substructure.

	pr_free ()			Free parser tables.
.endhelp

include	"../lib/parser.h"
include	"../lib/prstruct.h"

# Number of expected symbols in the symbol table. The table is reallocated
# automatically by the SYMTAB procedures if this value is not enough.

define	LEN_SYMTABLE		100


# PR_ALLOC -- Allocate space for symbol table and sequential tables

procedure pr_alloc ()

pointer stopen()

include	"parser.com"

begin
	# Open symbol table
	symtable = stopen ("parser", 2 * LEN_SYMTABLE, LEN_SYMTABLE, 
			   LEN_SYMTABLE * SZ_LINE)

	# Allocate space for other tables
	call mct_alloc (obstable,   10, 1, TY_INT)
	call mct_alloc (cattable,   10, 1, TY_INT)
	call mct_alloc (partable,   30, 1, TY_INT)
	call mct_alloc (exttable,   10, 1, TY_INT)
	call mct_alloc (trntable,   10, 1, TY_INT)
	call mct_alloc (settable,   10, 1, TY_INT)
	call mct_alloc (trcattable, 20, 2, TY_INT)
	call mct_alloc (trobstable, 20, 2, TY_INT)
	call mct_alloc (tfcattable, 20, 2, TY_INT)
	call mct_alloc (tfobstable, 20, 2, TY_INT)
	call mct_alloc (tpartable,  20, 1, TY_INT)
end


# PR_INALLOC -- Allocate space for input variable substructure.

procedure pr_inalloc (ptr)

pointer	ptr		# substructure pointer (output)

begin
	# Allocate space
	call malloc (ptr, LEN_PINP, TY_STRUCT)

	# Initialize substructure
	PINP_COL    (ptr) = INDEFI
	PINP_ERRCOL (ptr) = INDEFI
	PINP_WTSCOL (ptr) = INDEFI
	PINP_SPARE  (ptr) = NO
end


# PR_FTALLOC -- Allocate space for fitting parameter substructure.

procedure pr_ftalloc (ptr)

pointer	ptr		# substructure pointer (output)

begin
	# Allocate space
	call malloc (ptr, LEN_PFIT, TY_STRUCT)

	# Initialize substructure
	PFIT_VALUE (ptr) = INDEFR
	PFIT_DELTA (ptr) = INDEFR
end


# PR_STALLOC -- Allocate and initialize a set equation substructure.
# Initialization may not be necessary for all fields in the substructure,
# but it's safer to do it anyway.

procedure pr_stalloc (ptr)

pointer	ptr		# substructure pointer (output)

begin
	# Allocate space
	call malloc (ptr, LEN_PSEQ, TY_STRUCT)

	# Initialize string offsets
	PSEQ_EQ     (ptr) = INDEFI
	PSEQ_ERROR  (ptr) = INDEFI
	PSEQ_ERRMIN (ptr) = INDEFI
	PSEQ_ERRMAX (ptr) = INDEFI
	PSEQ_WEIGHT (ptr) = INDEFI
	PSEQ_WTSMIN (ptr) = INDEFI
	PSEQ_WTSMAX (ptr) = INDEFI

	# Initialize code pointers
	PSEQ_RPNEQ     (ptr) = NULL
	PSEQ_RPNERROR  (ptr) = NULL
	PSEQ_RPNERRMIN (ptr) = NULL
	PSEQ_RPNERRMAX (ptr) = NULL
	PSEQ_RPNWEIGHT (ptr) = NULL
	PSEQ_RPNWTSMIN (ptr) = NULL
	PSEQ_RPNWTSMAX (ptr) = NULL
end


# PR_TRALLOC -- Allocate space and initialize a transformation equation
# substructure. Initialization may not be necessary for all fields in the
# substructure, but it's safer to do it anyway.

procedure pr_tralloc (ptr, nrcat, nrobs, nfcat, nfobs, npars)

pointer	ptr		# substructure pointer (output)
int	nrcat		# number of catalog variables in reference eq.
int	nrobs		# number of observation variables in reference eq.
int	nfcat		# number of catalog variables in fit eq.
int	nfobs		# number of observation variables in fit eq.
int	npars		# number of parameters

int	nvars, nrvars, nfvars

begin
	# Total number of variables
	nrvars = nrcat  + nrobs
	nfvars = nfcat  + nfobs
	nvars  = nrvars + nfvars

	# Allocate space
	call malloc (ptr, LEN_PTEQ (nvars, npars), TY_STRUCT)

	# Initialize counters
	PTEQ_NRCAT (ptr) = nrcat
	PTEQ_NROBS (ptr) = nrobs
	PTEQ_NRVAR (ptr) = nrvars
	PTEQ_NFCAT (ptr) = nfcat
	PTEQ_NFOBS (ptr) = nfobs
	PTEQ_NFVAR (ptr) = nfvars
	PTEQ_NVAR  (ptr) = nvars
	PTEQ_NPAR  (ptr) = npars
	PTEQ_NFPAR (ptr) = INDEFI

	# Initialize variable offsets and counters
	call amovki (INDEFI, PTEQ_AREFVAR (ptr), nrvars)
	call amovki (INDEFI, PTEQ_AFITVAR (ptr), nfvars)
	call aclri  (PTEQ_AREFCNT (ptr), nrvars)
	call aclri  (PTEQ_AFITCNT (ptr), nfvars)

	# Initialize parameter offsets, values, and list
	call amovki (INDEFI, PTEQ_APAR (ptr), npars)
	call amovkr (INDEFR, PTEQ_APARVAL (ptr), npars)
	call aclri  (PTEQ_APLIST (ptr), npars)

	# Initialize string offsets
	PTEQ_FIT    (ptr) = INDEFI
	PTEQ_REF    (ptr) = INDEFI
	PTEQ_ERROR  (ptr) = INDEFI
	PTEQ_ERRMIN (ptr) = INDEFI
	PTEQ_ERRMAX (ptr) = INDEFI
	PTEQ_WEIGHT (ptr) = INDEFI
	PTEQ_WTSMIN (ptr) = INDEFI
	PTEQ_WTSMAX (ptr) = INDEFI
	PTEQ_XPLOT  (ptr) = INDEFI
	PTEQ_YPLOT  (ptr) = INDEFI
	call amovki (INDEFI, PTEQ_ADER (ptr), npars)

	# Initialize code pointers
	PTEQ_RPNFIT    (ptr) = NULL
	PTEQ_RPNREF    (ptr) = NULL
	PTEQ_RPNERROR  (ptr) = NULL
	PTEQ_RPNERRMIN (ptr) = NULL
	PTEQ_RPNERRMAX (ptr) = NULL
	PTEQ_RPNWEIGHT (ptr) = NULL
	PTEQ_RPNWTSMIN (ptr) = NULL
	PTEQ_RPNWTSMAX (ptr) = NULL
	PTEQ_RPNXPLOT  (ptr) = NULL
	PTEQ_RPNYPLOT  (ptr) = NULL
	call amovki (NULL, PTEQ_ARPNDER (ptr), npars)
end


# PR_FREE - Free parser symbol table and sequential tables.

procedure pr_free ()

int	n
pointer	sym, ptr

include	"parser.com"

pointer	sthead(), stnext()

begin
	# Traverse the symbol table looking for symbol
	# substructures before closing it.
	sym = sthead (symtable)
	while (sym != NULL) {

	    # Get pointer to the equation substructure,
	    # and free it only if not NULL
	    ptr = PSYM_SUB (sym)
	    if (ptr != NULL) {

		# Free additonal buffers associated with the substructure
		switch (PSYM_TYPE (sym)) {
		case PTY_CATVAR, PTY_OBSVAR:
		    # do nothing

		case PTY_FITPAR, PTY_CONST:
		    # do nothing

		case PTY_TRNEQ:

		    # Free transformation equation codes
		    if (PTEQ_RPNFIT (ptr) != NULL)
			call mfree (PTEQ_RPNFIT (ptr), TY_STRUCT)
		    if (PTEQ_RPNREF (ptr) != NULL)
			call mfree (PTEQ_RPNREF (ptr), TY_STRUCT)

		    # Free error equation codes
		    if (PTEQ_RPNERROR (ptr) != NULL)
			call mfree (PTEQ_RPNERROR (ptr), TY_STRUCT)
		    if (PTEQ_RPNERRMIN (ptr) != NULL)
			call mfree (PTEQ_RPNERRMIN (ptr), TY_STRUCT)
		    if (PTEQ_RPNERRMAX (ptr) != NULL)
			call mfree (PTEQ_RPNERRMAX (ptr), TY_STRUCT)

		    # Free weight equation codes
		    if (PTEQ_RPNWEIGHT (ptr) != NULL)
			call mfree (PTEQ_RPNWEIGHT (ptr), TY_STRUCT)
		    if (PTEQ_RPNWTSMIN (ptr) != NULL)
			call mfree (PTEQ_RPNWTSMIN (ptr), TY_STRUCT)
		    if (PTEQ_RPNWTSMAX (ptr) != NULL)
			call mfree (PTEQ_RPNWTSMAX (ptr), TY_STRUCT)

		    # Free plot equation codes
		    if (PTEQ_RPNXPLOT (ptr) != NULL)
			call mfree (PTEQ_RPNXPLOT (ptr), TY_STRUCT)
		    if (PTEQ_RPNYPLOT (ptr) != NULL)
			call mfree (PTEQ_RPNYPLOT (ptr), TY_STRUCT)
		    do n = 1, PTEQ_NPAR (ptr)
			call mfree (PTEQ_RPNDER (ptr, n), TY_STRUCT)

		case PTY_SETEQ:

		    # Free set equation code
		    if (PSEQ_RPNEQ (ptr) != NULL)
			call mfree (PSEQ_RPNEQ (ptr), TY_STRUCT)

		    # Free error equation codes
		    if (PSEQ_RPNERROR (ptr) != NULL)
			call mfree (PSEQ_RPNERROR (ptr), TY_STRUCT)
		    if (PSEQ_RPNERRMIN (ptr) != NULL)
			call mfree (PSEQ_RPNERRMIN (ptr), TY_STRUCT)
		    if (PSEQ_RPNERRMAX (ptr) != NULL)
			call mfree (PSEQ_RPNERRMAX (ptr), TY_STRUCT)

		    # Free weight equation codes
		    if (PSEQ_RPNWEIGHT (ptr) != NULL)
			call mfree (PSEQ_RPNWEIGHT (ptr), TY_STRUCT)
		    if (PSEQ_RPNWTSMIN (ptr) != NULL)
			call mfree (PSEQ_RPNWTSMIN (ptr), TY_STRUCT)
		    if (PSEQ_RPNWTSMAX (ptr) != NULL)
			call mfree (PSEQ_RPNWTSMAX (ptr), TY_STRUCT)

		default:
		    call error (0, "pr_free: unknown equation symbol type")
		}

		# Free equation substructure
		call mfree (ptr, TY_STRUCT)
	    }

	    # Advance to next symbol
	    sym = stnext (symtable, sym)
	}

	# Close symbol table
	call stclose (symtable)

	# Close other tables
	call mct_free (obstable)
	call mct_free (cattable)
	call mct_free (partable)
	call mct_free (exttable)
	call mct_free (trntable)
	call mct_free (settable)
	call mct_free (trcattable)
	call mct_free (trobstable)
	call mct_free (tfcattable)
	call mct_free (tfobstable)
	call mct_free (tpartable)
end
