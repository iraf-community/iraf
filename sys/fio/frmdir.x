# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>

# FRMDIR -- Remove an empty directory.  An error action is taken if the
# name of the directory is too long, if directory exists but is not
# empty, or if there is no write permission on the directory.

procedure frmdir (dir)

char	dir[ARB]		# virtual or OS-dependent directory spec

int	status
pointer	sp, osfn, dirname
int	access()
errchk	syserrs

begin
	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (dirname, SZ_PATHNAME, TY_CHAR)

	iferr (call fmapfn (dir, Memc[osfn], SZ_PATHNAME))
	    call syserrs (SYS_FMKDIRFNTL, dir)
	
	# Always present ZFRMDR with a directory pathname (rather than an
	# absolute or cwd relative filename), in case the kernel procedure
	# is not smart enough to handle all these possibilities.

	call strupk (Memc[osfn], Memc[osfn], SZ_PATHNAME)
	call zfpath (Memc[osfn], Memc[dirname], SZ_PATHNAME, status)
	if (status != ERR)
	    call zfsubd (Memc[dirname], SZ_PATHNAME, "", status)

	# Try to remove the directory.  If the directory cannot be removed
	# use the OS name of the directory in the error message to close the
	# loop with the user.

	if (status != ERR) {
	    call strpak (Memc[dirname], Memc[osfn], SZ_PATHNAME)
	    call zfrmdr (Memc[osfn], status)
	}
	if (status == ERR)
	    call syserrs (SYS_FRMDIR, Memc[dirname])

	call sfree (sp)
end
