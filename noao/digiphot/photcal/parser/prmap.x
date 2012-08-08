.help prmap
Column/variable mapping.

These procedures map input column numbers, specified in the configuration
file, with variable numbers that are used to index information in memory
tables. Variable numbers are assigned internally by the parser.
.sp
The main goal of these routines is to provide a fast way of converting from
column number to a variable number, if any, since this kind of operation is
performed by the i/o routines for eavery column in all lines in each input
file.
.sp
First the calling program should map columns with variables in the catalog
or observation section of the configuration file, by using either pr_obsmap()
or pr_catmap(). Once the mapping is built the corresponding variable number
for a given column can be obtained with pr_findmap(). All memory allocated
by these procedures is free by calling pr_unmap().
.nf

Main entry points:

    pr_obsmap (table, nobs)	    Build observational variables map
    pr_catmap (table, ncat)	    Build catalog variables map
int pr_findmap (table, col, nums, max_nums) Find var numbers for given column
int pr_findmap1 (table, col)        Find first var number for given column
    pr_unmap (table)	    	    Free space of mapped variables

Low level entry points:

		pr_inmap (table, nvars, type)	Map observational/catalog var.
.endhelp

include	"../lib/parser.h"
include	"../lib/prdefs.h"

# Table structure
define	LEN_TABLE	5		# fixed section length
define	TABLE_MIN	Memi[$1+0]	# minimum column number
define	TABLE_MAX	Memi[$1+1]	# maximum column number
define	TABLE_NVARS	Memi[$1+2]	# number of variables
define	TABLE_PCOLS	Memi[$1+3]	# pointer to column indices
define	TABLE_PNCOLS	Memi[$1+4]	# pointer to column indices count


# PR_OBSTAB -- Map program observational input variables.

procedure pr_obsmap (table, nobs)

pointer	table			# table pointer (output)
int	nobs			# number of observational variables (output)

begin
	# Tabulate the observational variables.
	call pr_inmap (table, nobs, PTY_OBSVAR)
end


# PR_CATTAB -- Map catalog star input variables.

procedure pr_catmap (table, ncat)

pointer	table			# table pointer (output)
int	ncat			# number of catalog variables (output)

begin
	# Tabulate the catalog variables.
	call pr_inmap (table, ncat, PTY_CATVAR)
end


# PR_INMAP -- Map input variables from the sequential variable table, and
# build a table with this mapping. A pointer to the map table is returned,
# for future use by pr_findmap().

procedure pr_inmap (table, nvars, type)

pointer	table			# table pointer (output)
int	nvars			# number of variables (output)
int	type			# symbol type

int	i, mincol, maxcol, sym, col
pointer	pcols, pncols, colptr
int	pr_geti(), pr_gsym(), pr_gsymi()

begin
	# Get the sequential table pointer for the symbol type, and the
	# maximum and minimum number of columns
	if (type == PTY_OBSVAR) {
	    nvars  = pr_geti (NOBSVARS)
	    mincol = pr_geti (MINOBSCOL)
	    maxcol = pr_geti (MAXOBSCOL)
	} else if (type == PTY_CATVAR) {
	    nvars  = pr_geti (NCATVARS)
	    mincol = pr_geti (MINCATCOL)
	    maxcol = pr_geti (MAXCATCOL)
	} else
	    call error (0, "pr_inmap: Illegal symbol type")

	# Allocate the basic table structure.
	call malloc (table, LEN_TABLE, TY_STRUCT)
	TABLE_MIN (table) = mincol
	TABLE_MAX (table) = maxcol
	TABLE_NVARS (table) = nvars

	# Initialize.
	call malloc (pcols, maxcol * nvars, TY_INT)
	call amovki (INDEFI, Memi[pcols], maxcol * nvars)
	call calloc (pncols, maxcol, TY_INT)

	# Traverse symbols and store variable number at the corresponding
	# table location indexed by column number. There may be more than
	# one symbol per column number.

	do i = 1, nvars {
	    sym = pr_gsym (i, type)
	    col = pr_gsymi (sym, PINPCOL)
	    colptr = pcols + (col - 1) * nvars + Memi[pncols+col-1] 
	    Memi[colptr] = pr_gsymi (sym, PSYMNUM)
	    Memi[pncols+col-1] = Memi[pncols+col-1] + 1
	}

	TABLE_PCOLS(table) = pcols
	TABLE_PNCOLS(table) = pncols
end


# PR_FINDMAP -- Find the list of symbol numbers for a given input column.

int procedure pr_findmap (table, col, indices, max_nindices)

pointer	table			# table pointer
int	col			# input column
int	indices[ARB]		# output array of indices
int	max_nindices		# maximum permitted number of indices

int	nvars, nindices
pointer	colptr

begin
	# Check pointer.
	if (table == NULL)
	    call error (0, "pr_findmap: Null table pointer")

	# Check the table size.
	nvars = TABLE_NVARS(table)
	if (max_nindices < nvars)
	    call error (0, "pr_findmap: The output index array is too short")

	# Look for variable number.
	if (col < TABLE_MIN (table) || col > TABLE_MAX (table)) {
	    nindices = 0
	    call amovki (INDEFI, indices, nvars)
	} else {
	    nindices = Memi[TABLE_PNCOLS(table)+col-1]
	    colptr = TABLE_PCOLS(table) + (col - 1) * nvars
	    call amovi (Memi[colptr], indices, nindices)
	}

	return (nindices)
end


# PR_FINDMAP1 -- Find the symbol number for a given input column.

int procedure pr_findmap1 (table, col)

pointer	table			# table pointer
int	col			# input column

begin
	# Check pointer.
	if (table == NULL)
	    call error (0, "pr_findmap: Null table pointer")

	# Look for variable number.
	if (col < TABLE_MIN (table) || col > TABLE_MAX (table))
	    return (INDEFI)
	else
	    return (Memi[TABLE_PCOLS(table)+(col-1)*TABLE_NVARS(table)])
end


# PR_UNMAP -- Free space of mapped variables.

procedure pr_unmap (table)

pointer	table			# table pointer

begin
	# Check pointer.
	if (table == NULL)
	    call error (0, "pr_unmap: Null table pointer")

	# Free pointer table.
	call mfree (TABLE_PCOLS(table), TY_INT)
	call mfree (TABLE_PNCOLS(table), TY_INT)

	# Free table structure.
	call mfree (table, TY_INT)
end
