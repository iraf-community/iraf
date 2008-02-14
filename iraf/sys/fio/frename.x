# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>
include	<error.h>
include	<fio.h>

# FRENAME -- Change the name of a file, or move a file from one directory
# to another.  All file attributes, including file protection, are
# transferred with the file.  If a file already exists with the new name,
# protection and clobber checking are performed and the old file is deleted
# if permitted.  Interrupts are disabled while the VFN database is open to
# protect the database, ensure that that the lock on the mapping file is
# cleared, and to ensure that the mapping file is closed.

procedure frename (oldfname, newfname)

char	oldfname[ARB]		# old filename
char	newfname[ARB]		# new filename

int	file_exists
int	status, junk
pointer	sp, vp, oldosfn, newosfn, errmsg

pointer	vfnopen()
bool	fnullfile()
int	vfnadd(), vfndel()
include	<fio.com>
errchk	syserrs

define	fixvfn_ 91
define	close_ 92
define	abort_ 93

begin
	# The null file "dev$null" is a special case; ignore attempts to
	# rename this file.

	if (fnullfile (oldfname))
	    call syserrs (SYS_FRENAME, oldfname)
	else if (fnullfile (newfname))
	    call syserrs (SYS_FRENAME, newfname)

	call smark (sp)
	call salloc (oldosfn, SZ_PATHNAME, TY_CHAR)
	call salloc (newosfn, SZ_PATHNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Format the string "oldfname -> newfname" for error messages.
	call strcpy (oldfname, Memc[errmsg], SZ_LINE)
	call strcat (" --> ",  Memc[errmsg], SZ_LINE)
	call strcat (newfname, Memc[errmsg], SZ_LINE)

	# Get OSFN of old file and verify that the file exists.  Delete
	# the old file from the database.

	call intr_disable()
	iferr (vp = vfnopen (oldfname, VFN_WRITE))
	    goto abort_
	iferr (status = vfndel (vp, Memc[oldosfn], SZ_PATHNAME))
	    goto close_
	if (status == ERR)
	    file_exists = NO
	else
	    call zfacss (Memc[oldosfn], 0, 0, file_exists)

	if (file_exists == NO) {
	    iferr (call syserrs (SYS_FRENAME, Memc[errmsg]))
		goto close_
	} else iferr (call vfnclose (vp, VFN_UPDATE))
	    goto abort_

	# Perform clobber checking, delete old newfile if one exists.
	# Note that this must be done before opening the new VFN for
	# writing or deadlock may occur.

	iferr (call fclobber (newfname))
	    goto fixvfn_

	# Add the new VFN to the VFN database and create the new file.
	# If the vfnadd or the physical rename operation fail then we
	# must go back and undelete the old VFN.

	iferr (vp = vfnopen (newfname, VFN_WRITE))
	    goto abort_
	iferr (status = vfnadd (vp, Memc[newosfn], SZ_PATHNAME))
	    goto close_
	if (status != ERR)
	    call zfrnam (Memc[oldosfn], Memc[newosfn], status)

	if (status == ERR) {
	    # Close new VFN, reopen old one and undelete old VFN.
	    iferr (call vfnclose (vp, VFN_NOUPDATE))
		goto abort_
fixvfn_
	    iferr (vp = vfnopen (oldfname, VFN_WRITE))
		goto abort_
	    iferr (junk = vfnadd (vp, Memc[oldosfn], SZ_PATHNAME))
		goto close_
	    iferr (call vfnclose (vp, VFN_UPDATE))
		goto abort_
	    iferr (call syserrs (SYS_FRENAME, Memc[errmsg]))
		goto abort_
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
