# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PL_SAVEF -- Store a mask in external format in a binary file.  This simple
# code permits only one mask per file; more sophisticated mask storage
# facilities are planned.  These will likely obsolete this routine.

procedure pl_savef (pl, fname, title, flags)
 
pointer	pl			#I mask descriptor
char	fname[ARB]		#I file name
char	title[ARB]		#I mask title
int	flags			#I save flags

int	fd, masklen, buflen, junk
pointer	sp, fullname, extn, bp, sv
errchk	open, pl_save, write, mfree
int	open(), fnextn(), strlen(), pl_save()
bool	strne()

begin
	call smark (sp)
	call salloc (sv, LEN_SVDES, TY_STRUCT)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (fullname, SZ_PATHNAME, TY_CHAR)

	# Add the ".pl" filename extension if not already present.
	call strcpy (fname, Memc[fullname], SZ_PATHNAME)
	junk = fnextn (fname, Memc[extn], SZ_FNAME)
	if (strne (Memc[extn], "pl"))
	    call strcat (".pl", Memc[fullname], SZ_PATHNAME)

	# The update flag is required to allow overwriting an existing mask.
	if (and (flags, PL_UPDATE) != 0)
	    iferr (call delete (Memc[fullname]))
		;

	# Encode the mask.
	bp = NULL
	masklen = pl_save (pl, bp, buflen, flags)

	# Set up the savefile descriptor.
	SV_MAGIC(sv) = PLIO_SVMAGIC
	SV_TITLELEN(sv) = strlen (title) + 1
	SV_MASKLEN(sv) = masklen

	# Write the savefile.
	fd = open (Memc[fullname], NEW_FILE, BINARY_FILE)

	call miiwritei (fd, Memi[sv], LEN_SVDES)
	call miiwritec (fd, title, SV_TITLELEN(sv))
	call write (fd, Mems[bp], masklen * SZ_SHORT)
	call mfree (bp, TY_SHORT)

	call close (fd)
	call sfree (sp)
end
