# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<syserr.h>
include	<error.h>
include	<config.h>
include	<fio.h>

# DELETE -- Delete a single physical file.  It is an error if the file does
# not exist, is protected, or if the file simply cannot be deleted.  DELETEFG
# should be called if deletion of subfiles or multiple versions is desired.
# Interrupts are disabled while the VFN database is open to protect the
# database, ensure that that the lock on the mapping file is cleared, and to
# ensure that the mapping file is closed.

procedure delete (fname)

char	fname[ARB]		# file to be deleted

int	status
bool	nosuchfile
pointer	vp, sp, osfn

int	vfndel()
bool	fnullfile()
pointer	vfnopen()
define	abort_ 91
define	close_ 92

begin
	# The null file "dev$null" is a special case; ignore attempts to
	# delete this file.

	if (fnullfile (fname))
	    return

	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	call intr_disable()
	iferr (vp = vfnopen (fname, VFN_WRITE))
	    goto abort_

	# Delete the VFN and determine if the file actually exists.
	nosuchfile = false
	iferr (status = vfndel (vp, Memc[osfn], SZ_PATHNAME))
	    goto close_

	if (status == ERR)
	    nosuchfile = true
	else {
	    call zfacss (Memc[osfn], 0, 0, status)
	    if (status == NO) {
		#  If the file is a symlink pointing to a non-existent file,
		#  we'll delete the link below.
	        call zfacss (Memc[osfn], 0, SYMLINK_FILE, status)
	        if (status == YES)
		    nosuchfile = false
	        else
		    nosuchfile = true
	    }
	}

	# It is an error to try to delete a nonexistent file.
	if (nosuchfile) {
	    iferr (call filerr (fname, SYS_FDELNXF))
		goto close_
	}

	# Is the file protected?
	call zfprot (Memc[osfn], QUERY_PROTECTION, status)

	if (status == YES) {
	    iferr (call filerr (fname, SYS_FDELPROTFIL))
		goto close_
	} else {
	    # Try to delete the file.  If the delete operation succeeds but
	    # the file still exists, an older version has surfaced and the
	    # VFN must not be deleted from the file table.

	    call zfdele (Memc[osfn], status)
	    if (status == ERR) {
		iferr (call filerr (fname, SYS_FDELETE))
		    goto close_
	    } else {
		call zfacss (Memc[osfn], 0, 0, status)
		if (status == YES) {
		    iferr (call vfnclose (vp, VFN_NOUPDATE))
			goto abort_
		} else {
		    iferr (call vfnclose (vp, VFN_UPDATE))
			goto abort_
		}
	    }
	}

	call sfree (sp)
	call intr_enable()
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
