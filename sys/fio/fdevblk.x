# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<fset.h>

# FDEVBLK -- Get the device block size of the device on which the named logical
# directory resides.  The named logical directory must have write permission.
# A file pathname may be used to pass the logical directory name.

int procedure fdevblk (path)

char	path[ARB]			# pathname of directory or file

pointer	sp, fname, ldir, tempfn
int	fd, junk, block_size
int	fstati(), open(), fnldir()
errchk	mktemp, open, close

begin
	call smark (sp)
	call salloc (ldir,   SZ_PATHNAME, TY_CHAR)
	call salloc (fname,  SZ_PATHNAME, TY_CHAR)
	call salloc (tempfn, SZ_PATHNAME, TY_CHAR)

	# Generate the name of a temporary file in named directory.
	junk = fnldir (path, Memc[ldir], SZ_PATHNAME)
	call strcpy (Memc[ldir], Memc[fname], SZ_PATHNAME)
	call strcat ("zbk", Memc[fname], SZ_PATHNAME)
	call mktemp (Memc[fname], Memc[tempfn], SZ_PATHNAME)

	# Open the file and get the device block size.
	iferr {
	    fd = open (Memc[tempfn], NEW_FILE, BINARY_FILE)
	    block_size = fstati (fd, F_BLKSIZE)
	    call close (fd)
	    call delete (Memc[tempfn])
	} then
	    call syserrs (SYS_FACCDIR, Memc[ldir])

	call sfree (sp)
	return (block_size)
end
