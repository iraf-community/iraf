# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MTSTATUS -- Print the status of an allocated magtape device, i.e., print the
# lock file as a text file.  Called by the DEVSTATUS task.

procedure mtstatus (out, mtname)

int	out			#I output file
char	mtname[ARB]		#I magtape specification

int	in
pointer	sp, lockfile
errchk	open, fcopyo
int	open(), access()

begin
	call smark (sp)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)

	call mt_sync (OK)

	call mt_glock (mtname, Memc[lockfile], SZ_FNAME)
	if (access (Memc[lockfile], 0, 0) == NO) {
	    call fprintf (out, "tape position for %s is undefined\n")
		call pargstr (mtname)
	} else {
	    # Print the lockfile.
	    in = open (Memc[lockfile], READ_ONLY, TEXT_FILE)
	    call fcopyo (in, out)
	    call close (in)
	}
	
	call sfree (sp)
end
