include	<error.h>
include	"refspectra.h"


# REFTABLE -- For each input image select reference spectrum list from a table.
# The table is read from the file and stored in a simple symbol table.
#
# The table consists of pairs of words.  The first word is a list of spectra
# and the second word is the reference spectrum list to be used for each
# spectrum in the first list.  Note that the first list is  not an input
# list.  As a convenience if a reference list is missing the preceding list
# is implied.  Some examples follow.
#
#	spec1		spec2,spec3,spec4
#	spec5
#	spec6,spec7	spect8,spec9
#	spec10		spec11
#	spec12		spec13
#	spec14		spec15

procedure reftable (list, table, select)

pointer	list			# List of input spectra
char	table[ARB]		# Reference table
int	select			# Selection method

int	i, fd, input, refs
pointer	stp, sym
pointer	sp, image, ref1, ref2

pointer	stopen(), strefsbuf(), stenter(), stpstr(), stfind()
int	odr_getim(), open(), fscan(), nscan()
errchk	open

begin
	# Read the table.  Return an error if the file can't be opened.
	# Read each table entry of spectrum list and reference list.
	# Expand the input list to make a symbol table keyed on the
	# spectrum with the reference list string as it's value.
	# As a convenience if a reference list is missing the preceding
	# list is implied.

	fd = open (table, READ_ONLY, TEXT_FILE)

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (ref1, SZ_FNAME, TY_CHAR)
	call salloc (ref2, SZ_FNAME, TY_CHAR)

	stp = stopen ("table", 10, 10, 20*SZ_FNAME)
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[image], SZ_FNAME)
	    call gargwrd (Memc[ref1], SZ_FNAME)
	    if (nscan() < 1)
		next
	    if (nscan() < 2)
		call strcpy (Memc[ref2], Memc[ref1], SZ_FNAME)
	    else
		call strcpy (Memc[ref1], Memc[ref2], SZ_FNAME)

	    i = stpstr (stp, Memc[ref1], SZ_FNAME)

	    call odr_open (Memc[image], "", input)
	    while (odr_getim (input, Memc[image], SZ_FNAME) != EOF) {
	        sym = stenter (stp, Memc[image], 1)
	        Memi[sym] = i
	    }
	    call odr_close (input)
	}
	call close (fd)

	# For each input spectrum find the appropriate reference spectrum list.
	# If no list is found print a message and continue.  Switch on the
	# selection method.

	while (odr_getim (list, Memc[image], SZ_FNAME) != EOF) {
	    sym = stfind (stp, Memc[image])
	    if (sym == NULL) {
		call refmsgs (NO_REFSPEC, Memc[image], 0., "", "")
		next
	    }

	    call odr_open (Memc[image], "", input)
	    call odr_open (Memc[strefsbuf (stp, Memi[sym])], "", refs)

	    switch (select) {
	    case MATCH:
	        call refmatch(input, refs)
	    case NEAREST:
	        call refnearest (input, refs)
	    case PRECEDING:
	        call refprecede (input, refs)
	    case FOLLOWING:
	        call reffollow (input, refs)
	    case INTERP:
	        call refinterp (input, refs)
	    case AVERAGE:
	        call refaverage (input, refs)
	    }

	    call odr_close (input)
	    call odr_close (refs)
	}

	call stclose (stp)
	call sfree (sp)
end
