# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<knet.h>
include	"mtio.h"

# MTREWIND -- Rewind the named magtape device.  In general we do not want to
# wait for a rewind to complete, so we do not want to use MTOPEN to do the
# rewind.  We start the rewind, update the saved position, and return.
# We assume that the OS driver provides synchronization after rewind.
# We do not protect against a rewind while i/o is in progress by another
# process (i.e., a bkg process) belonging to the same user.

procedure mtrewind (filespec)

char	filespec[ARB]		# device to be rewound

pointer	sp, osdev
int	mt, junk, status, density
errchk	mt_parse, syserrs, mt_osdev
int	mt_devallocated()
include	"mtio.com"

begin
	call smark (sp)
	call salloc (osdev, SZ_FNAME, TY_CHAR)

	# Get scratch mtio file descriptor slot for updating lockfile.
	# Poke OS to rewind drive, then update saved position.

	for (mt=1;  mt <= MT_MAXTAPES && MT_OSCHAN(mt) != NULL;  mt=mt+1)
	    ;
	if (mt > MT_MAXTAPES)				# cannot happen
	    call syserrs (SYS_MTMULTOPEN, filespec)

	# Get tape drive name from filespec.
	call mt_parse (filespec, MT_DRIVE(mt), SZ_DRIVE, density, junk, junk)
	call mt_osdev (MT_DRIVE(mt), density, Memc[osdev], SZ_FNAME, junk)

	# Make sure that the device has been physically allocated.
	if (mt_devallocated (Memc[osdev]) == NO)
	    call syserrs (SYS_MTNOTALLOC, filespec)

	# Make sure that a lock file exists.  The mt_allocate procedure does
	# not physically allocate the drive, rather it just writes the lock
	# file for MTIO.

	call mt_allocate (filespec)

	MT_FILE(mt)     = 1
	MT_RECORD(mt)   = 1
	MT_NRECORDS(mt) = 0

	call strpak (Memc[osdev], Memc[osdev], SZ_FNAME)
	call zzrwmt (Memc[osdev], status)

	# Save the current position, i.e., update the lock file to show that
	# the tape is positioned to record 1 of file 1.

	if (status == ERR) {
	    MT_FILE(mt) = -1
	    call mt_savepos (mt)
	    call mt_sync (OK)
	    call syserrs (SYS_MTREW, filespec)
	} else {
	    call mt_savepos (mt)
	    call mt_sync (OK)
	}

	call sfree (sp)
end
