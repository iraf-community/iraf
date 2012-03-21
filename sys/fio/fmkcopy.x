# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>
include	<error.h>
include	<fio.h>

# FMKCOPY -- Create a null length copy of an existing file.  The new file
# inherits all the directory attributes of the original file.  For example,
# if the oldfile is an executable file, the new one will be too.  This avoids
# file copy operations which copy the file data but lose file attributes.
# Interrupts are disabled while the VFN database is open to protect the
# database, ensure that that the lock on the mapping file is cleared, and to
# ensure that the mapping file is closed.

procedure fmkcopy (oldfile, newfile)

char	oldfile[ARB]		# file to be copied
char	newfile[ARB]		# newfile

char	url[SZ_PATHNAME], old[SZ_PATHNAME]
int	status, file_exists, junk
pointer	vp, sp, oldosfn, newosfn
int	vfnadd(), strncmp(), nowhite()
pointer	vfnopen()
errchk	delete, filerr, fmapfn, fclobber
include	<fio.com>
define	close_ 91
define	abort_ 92

begin
	call smark (sp)
	call salloc (oldosfn, SZ_PATHNAME, TY_CHAR)
	call salloc (newosfn, SZ_PATHNAME, TY_CHAR)


        # If we're given a URL to a file, cache it.
	call aclrc (old, SZ_PATHNAME)
        if (strncmp ("http:", oldfile, 5) == 0)
	    return
        else if (strncmp ("file:", oldfile, 5) == 0)
	    return
        else {
            # Strip any whitespace at either end of the filename.
            if (nowhite (oldfile, old, SZ_PATHNAME) == 0)
                call syserr (SYS_FNOFNAME)
        }

	# Get OSFN of old file and verify that the file exists.

	call fmapfn (old, Memc[oldosfn], SZ_PATHNAME)
	call zfacss (Memc[oldosfn], 0, 0, file_exists)
	if (file_exists == NO)
	    call filerr (oldfile, SYS_FOPEN)

	# Perform clobber checking, delete old file if one exists.
	# Note that this must be done before opening the new VFN for
	# writing or deadlock may occur.

	call fclobber (newfile)
	
	# Add the new VFN to the VFN database and create the new file.

	call intr_disable()
	iferr (vp = vfnopen (newfile, VFN_WRITE))
	    goto abort_
	iferr (junk = vfnadd (vp, Memc[newosfn], SZ_PATHNAME))
	    goto close_

	call zfmkcp (Memc[oldosfn], Memc[newosfn], status)
	if (status == ERR) {
	    iferr (call filerr (newfile, SYS_FMKCOPY))
		goto close_
	} else
	    iferr (call vfnclose (vp, VFN_UPDATE))
		goto abort_

	call intr_enable()
	call sfree (sp)
	return


	# Error recovery nasties.
close_
	iferr (call vfnclose (vp, VFN_NOUPDATE))
	    ;
abort_
	call intr_enable()
	call sfree (sp)
	call erract (EA_ERROR)
end
