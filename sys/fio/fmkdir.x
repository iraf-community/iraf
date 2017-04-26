# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>

# FMKDIR -- Create a new, empty directory.  An error action is taken if the
# name of the new directory is too long, if a file already exists with the
# same name, or if there is no write permission on the directory.

procedure fmkdir (newdir)

char	newdir[ARB]		# virtual or OS-dependent directory spec

int	status
pointer	sp, osfn, dirname
int	access()
errchk	syserrs

begin
	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (dirname, SZ_PATHNAME, TY_CHAR)

	# It is an error if the named file already exists, be it a directory
	# or not.  If the file does not exist but the filename cannot be
	# mapped that indicates that the directory name is too long and
	# FMAPFN tried to access the mapping file.  Filename mapping does not
	# currently map long directory names so we do not permit directories
	# with long names to be created here.  Filename mapping (using the
	# mapping file) is intentionally not supported for reasons of
	# efficiency and to discourage use of very long diectory names, which
	# would tend to overflow filename buffers.

	if (access (newdir, 0, 0) == YES)
	    call syserrs (SYS_FMKDIR, newdir)
	iferr (call fmapfn (newdir, Memc[osfn], SZ_PATHNAME))
	    call syserrs (SYS_FMKDIRFNTL, newdir)
	
	# Always present ZFMKDR with a directory pathname (rather than an
	# absolute or cwd relative filename), in case the kernel procedure
	# is not smart enough to handle all these possibilities.

	call strupk (Memc[osfn], Memc[osfn], SZ_PATHNAME)
	call zfpath (Memc[osfn], Memc[dirname], SZ_PATHNAME, status)
	if (status != ERR)
	    call zfsubd (Memc[dirname], SZ_PATHNAME, "", status)

	# Try to create the new directory.  If the directory cannot be created
	# use the OS name of the directory in the error message to close the
	# loop with the user.

	if (status != ERR) {
	    call strpak (Memc[dirname], Memc[osfn], SZ_PATHNAME)
	    call zfmkdr (Memc[osfn], status)
	}
	if (status == ERR)
	    call syserrs (SYS_FMKDIR, Memc[dirname])

	call sfree (sp)
end
