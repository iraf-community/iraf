include	<ctype.h>


# CCDSUBSET -- Return the CCD subset identifier.
#
# 1. Get the subset string and search the subset record file for the ID string.
# 2. If the subset string is not in the record file define a default ID string
#    based on the first word of the subset string.  If the first word is not
#    unique append a integer to the first word until it is unique.
# 3. Add the new subset string and identifier to the record file.
# 4. Since the ID string is used to generate image names replace all
#    nonimage name characters with '_'.
#
# It is an error if the record file cannot be created or written when needed.

procedure ccdsubset (im, subset, sz_name)

pointer	im			# Image
char	subset[sz_name]		# CCD subset identifier
int	sz_name			# Size of subset string

bool	streq()
int	i, fd, ctowrd(), open(), fscan()
pointer	sp, fname, str1, str2, subset1, subset2, subset3
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (subset1, SZ_LINE, TY_CHAR)
	call salloc (subset2, SZ_LINE, TY_CHAR)
	call salloc (subset3, SZ_LINE, TY_CHAR)

	# Get the subset record file and the subset string.
	call clgstr ("ssfile", Memc[fname], SZ_LINE)
	call hdmgstr (im, "subset", Memc[str1], SZ_LINE)

	# The default subset identifier is the first word of the subset string.
	i = 1
	i = ctowrd (Memc[str1], i, Memc[subset1], SZ_LINE)

	# A null subset string is ok.  If not null check for conflict
	# with previous subset IDs.
	if (Memc[str1] != EOS) {
	    call strcpy (Memc[subset1], Memc[subset3], SZ_LINE)

	    # Search the subset record file for the same subset string.
	    # If found use the ID string.  If the subset ID has been
	    # used for another subset string then increment an integer
	    # suffix to the default ID and check the list again.

	    i = 1
	    ifnoerr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[str2], SZ_LINE)
		    call gargwrd (Memc[subset2], SZ_LINE)
		    if (streq (Memc[str1], Memc[str2])) {
			i = 0
			call strcpy (Memc[subset2], Memc[subset1], SZ_LINE)
			break
		    } if (streq (Memc[subset1], Memc[subset2])) {
			call sprintf (Memc[subset1], SZ_LINE, "%s%d")
			    call pargstr (Memc[subset3])
			    call pargi (i)
			i = i + 1
			call seek (fd, BOF)
		    }
		}
		call close (fd)
	    }

	    # If the subset is not in the record file add it.
	    if (i > 0) {
		fd = open (Memc[fname], APPEND, TEXT_FILE)
		call fprintf (fd, "'%s'\t%s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[subset1])
		call close (fd)
	    }
	}

	# Set the subset ID string and replace magic characters by '_'
	# since the subset ID is used in forming image names.

	call strcpy (Memc[subset1], subset, sz_name)
	for (i=1; subset[i]!=EOS; i=i+1)
	    if (!(IS_ALNUM(subset[i])||subset[i]=='.'))
		subset[i] = '_'

	call sfree (sp)
end
