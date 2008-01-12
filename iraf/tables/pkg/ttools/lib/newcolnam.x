include	<ctype.h>
include	<tbset.h>
include	"reloperr.h"

# NEWCOLNAM -- Create a new, unique column name
#
# This procedure receives as input an array of column pointers from two or 
# more tables and an index into that array. If the name of the column pointed
# to by that index is unique, it is output as the new name. If it is not
# unique, a suffix of the form "_i" is appended to the name, where i is
# a digit which (hopefully) makes the name unique.
#
# B.Simon	3-Nov-87	first code
# B.Simon	4-Sep-90	Replaced call to strncmp with streq

procedure newcolnam (numcol, colptr, colidx, newnam, maxch)

int	numcol		# i: Number of column pointers
pointer	colptr[ARB]	# i: Array of column pointers
int	colidx		# i: Index to column to be renamed
char	newnam[ARB]	# o: New column name
int	maxch		# i: Maximum characters in new name
#--
int	olen, nmatch, nbefore, icol
pointer sp, oldnam, colnam, errtxt

string	notuniq "Cannot create a unique column name (%s)"

bool	streq()
int	strlen()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (oldnam, SZ_COLNAME, TY_CHAR)
	call salloc (colnam, SZ_COLNAME, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Read column name pointed to by index

	call tbcigt (colptr[colidx], TBL_COL_NAME, Memc[oldnam], SZ_COLNAME)
	call strupr (Memc[oldnam])

	# See if the name is unique, and if not, how many columns with
	# the same name precede this one

	nmatch = 0
	nbefore = 0
	do icol = 1, numcol {
	    call tbcigt (colptr[icol], TBL_COL_NAME, Memc[colnam], SZ_COLNAME)
	    call strupr (Memc[colnam])

	    if (streq (Memc[colnam], Memc[oldnam])) {
		nmatch = nmatch + 1
		if (icol <= colidx)
		    nbefore = nbefore + 1
	    }
	}

	# If the name is not unique, add a suffix of the form "_i"

	if (nmatch > 1) {

	    # Check for ridiculous values of maxch

	    olen = min (maxch-2, strlen(Memc[oldnam]))
	    if (olen < 1) {
		call sprintf (Memc[errtxt], SZ_LINE, notuniq)
		call pargstr (Memc[oldnam])
		call error (SYNTAX, Memc[errtxt])
	    }

	    # Add the suffix

	    Memc[oldnam+olen] = '_'
	    Memc[oldnam+olen+1] = TO_DIGIT (nbefore)
	    Memc[oldnam+olen+2] = EOS

	    # Make sure it is unique

	    do icol = 1, numcol {
		call tbcigt (colptr[icol], TBL_COL_NAME, Memc[colnam],
			     SZ_COLNAME)
		if (streq (Memc[oldnam], Memc[colnam])) {
		    call sprintf (Memc[errtxt], SZ_LINE, notuniq)
		    call pargstr (Memc[oldnam])
		    call error (SYNTAX, Memc[errtxt])
		}
	    }
	}			

	# Copy to the output string

	call strcpy (Memc[oldnam], newnam, maxch)
	call sfree (sp)
end
