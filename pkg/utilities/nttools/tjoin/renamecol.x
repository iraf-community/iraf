include	<ctype.h>
include	<tbset.h>
include "tjoin.h"

# RENAMECOL -- Rename a column to make its name unique
#
# If the name of the column pointed to by that index is unique, it is output 
# as the new name. If it is not unique, a suffix of the form "_i" is appended 
# to the name, where i is a digit which (hopefully) makes the name unique.
#
# B.Simon	03-Nov-87	first code
# B.Simon	04-Sep-90	Replaced call to strncmp with streq
# B.Simon	16-Apr-99	Revised version to work with tjoin

procedure renamecol (tji, jtab, jcol, colname, maxch)

pointer	tji[2]		# i: Array of table info descriptors
int	jtab		# i: Index of table containing column
int	jcol		# i: Index of column within table
char	colname		# u: Column name
int	maxch		# i: Max length of column name
#--
bool	before
int	olen, nmatch, nbefore, itab, icol
pointer sp, oldnam, tmpnam, errtxt

string	notuniq "Cannot create a unique column name (%s)"

bool	streq()
int	strlen()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (oldnam, SZ_COLNAME, TY_CHAR)
	call salloc (tmpnam, SZ_COLNAME, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Copy name to temporrary variable

	call strcpy (colname, Memc[oldnam], SZ_COLNAME)
	call strupr (Memc[oldnam])

	# See if the name is unique, and if not, how many columns with
	# the same name precede this one

	nmatch = 0
	nbefore = 0
	before = true

	do itab = 1, 2 {
	    do icol = 1, TJ_DNUM(tji[itab]) {
		call tbcigt (TJ_DCOL(tji[itab],icol), TBL_COL_NAME, 
			     Memc[tmpnam], SZ_COLNAME)
		call strupr (Memc[tmpnam])

		if (streq (Memc[tmpnam], Memc[oldnam])) {
		    nmatch = nmatch + 1

		    if (before)
			nbefore = nbefore + 1
		}

		if (itab == jtab && icol == jcol)
		    before = false
	    }
	}

	# If the name is not unique, add a suffix of the form "_i"

	if (nmatch > 1) {

	    # Check for ridiculous values of maxch

	    olen = min (maxch-2, strlen(Memc[oldnam]))
	    if (olen < 1) {
		call sprintf (Memc[errtxt], SZ_LINE, notuniq)
		call pargstr (Memc[oldnam])
		call error (1, Memc[errtxt])
	    }

	    # Add the suffix

	    Memc[oldnam+olen] = '_'
	    Memc[oldnam+olen+1] = TO_DIGIT (nbefore)
	    Memc[oldnam+olen+2] = EOS

	    # Make sure it is unique

	    do itab = 1, 2 {
		do icol = 1, TJ_DNUM(tji[itab]) {
		    call tbcigt (TJ_DCOL(tji[itab],icol), TBL_COL_NAME, 
				 Memc[tmpnam], SZ_COLNAME)

		    if (streq (Memc[oldnam], Memc[tmpnam])) {
			call sprintf (Memc[errtxt], SZ_LINE, notuniq)
			call pargstr (Memc[oldnam])
			call error (1, Memc[errtxt])
		    }
		}
	    }
	}			

	# Copy to the output string

	call strcpy (Memc[oldnam], colname, maxch)
	call sfree (sp)
end
