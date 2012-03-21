# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>

# FUNLINK -- Remove a symlink.

procedure funlink (lname)

char	lname[ARB]			# link name

int	status
pointer	sp, oslnk
int	access()
errchk	syserrs

begin
	call smark (sp)
	call salloc (oslnk, SZ_PATHNAME, TY_CHAR)

	# It is an error if the link file doesn't exist.
	if (access (lname, 0, 0) == NO)
	    call syserrs (SYS_FOPEN, lname)
	
	# Try to remove the symlink.
        iferr (call fmapfn (lname, Memc[oslnk], SZ_PATHNAME))
            call syserrs (SYS_FSYMLINK, lname)
	call zfulnk (Memc[oslnk], status)
	if (status == ERR)
	    call syserrs (SYS_FUNLINK, lname)

	call sfree (sp)
end
