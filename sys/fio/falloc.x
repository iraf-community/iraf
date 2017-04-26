# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	<syserr.h>
include	<error.h>
include	<config.h>
include	<fio.h>

# FALLOC -- Create a binary file of a given size, and open as a binary file
# with read write permission.  Interrupts are disabled while the VFN database
# is open to protect the database, ensure that that the lock on the mapping
# file is cleared, and to ensure that the mapping file is closed.

procedure falloc (fname, file_size)

char	fname[ARB]			# virtual file name
long	file_size			# file size in chars

int	status, junk
pointer	vp

int	vfnadd()
pointer	vfnopen()
bool	fnullfile()
errchk	fclobber
include	<fio.com>

define	close_ 91
define	abort_ 92

begin
	# The null file "dev$null" is a special case; ignore attempts to
	# create this file.

	if (fnullfile (fname))
	    return

	# Perform clobber checking, delete old file if one exists.
	# Note that this must be done before opening the new VFN for
	# writing or deadlock on the VFN database may occur.

	call fclobber (fname)

	# Add new VFN and get the OSFN of the new file.
	# Allocate the file and update VFN database.

	call intr_disable()
	iferr (vp = vfnopen (fname, VFN_WRITE))
	    goto abort_
	iferr (junk = vfnadd (vp, pathname, SZ_PATHNAME))
	    goto close_

	call zfaloc (pathname, file_size * SZB_CHAR, status)
	if (status == ERR) {
	    iferr (call filerr (fname, SYS_FALLOC))
		goto close_
	} else
	    iferr (call vfnclose (vp, VFN_UPDATE))
		goto abort_

	call intr_enable()
	return


	# Error recovery nasties.
close_
	iferr (call vfnclose (vp, VFN_NOUPDATE))
	    ;
abort_
	call intr_enable()
	call erract (EA_ERROR)
end
