# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<error.h>
include	"mtio.h"

.help mtcache
.nf _________________________________________________________________________
MTCACHE -- Cache the magtape position (file and record) between file opens.
The lock file is used for permanent storage, but is only read and written
when necessary.  When multiple tape files are accessed by a single task the
tape position is kept in the cache between successive MTOPEN - CLOSE file
accesses.

	 mt_getpos (mt, file, record)
	mt_savepos (mt)
	   mt_sync (status)
       mt_clrcache ()

SAVEPOS updates the tape descriptor in the cache; SYNC updates the cache
on disk in the lock file.  SYNC is automatically called at task termination
time (including during abnormal termination), but is not normally called
when a tape file is closed.

ERROR RECOVERY is a bit tricky.  If error recovery takes place the ONERROR
procedures are called first thing, before FIO cleanup takes place.  That means
that SYNC will be called while the magtape channel is still open, and before
SAVEPOS has been called by ZCLSMT (which is called by fio_cleanup which is
called by the main).  If an error occurs on a magtape channel we want to
record an undefined file position.  We must see that SAVEPOS is called at
file open time to mark the cache for updating, causing SYNC to write an
undefined position lock file when the error occurs.  ZCLSMT, when called
during error recovery, should set the position to undefined but need not
sync the cache.

The cache is invalidated at process startup time and at SYNC time in case
the user loads a new tape or runs a different magtape process while the
current process is idling between tasks.  In other words, the cache is only
valid while the task using it is executing.
.endhelp ___________________________________________________________________

define	SZ_CACHE	MT_MAXTAPES


# MT_GETPOS -- Return the position (file and record) of a drive given its
# descriptor.  -1 is returned if the current position is undefined.

procedure mt_getpos (mt, file, record)

int	mt		# MTIO descriptor
int	file		# receives current file number
int	record		# receives current record number

int	slot
bool	streq()
include	"mtcache.com"
include	"mtio.com"

begin
	# First look in the cache.
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1)
	    if (streq (MT_DRIVE(mt), c_drive[1,slot])) {
		call amovi (c_mtdes[1,slot], MT_STRUCT(0), LEN_MTIODES)
		call strcpy (c_drive[1,slot], MT_DRIVE(0), SZ_DRIVE)
		file = MT_FILE(0)
		record = MT_RECORD(0)
		return
	    }

	# Get the current position from the lock file, if there is one.
	call mt_read_lockfile (mt, file, record)
end


# MT_SAVEPOS -- Save the current position in the cache.  The entire descriptor
# is saved since most of the information therein is needed for SYNC, even
# though only the file and record numbers will be used by GETPOS.

procedure mt_savepos (mt)

int	mt		# MTIO descriptor

int	prev, slot
bool	streq(), strne()
extern	mt_sync()
include	"mtcache.com"
include	"mtio.com"
data	prev /0/
define	cache_ 91

begin
	# Post termination handler to sync the cache at task termination time.
	call onerror (mt_sync)

	# Do not update the cache if the file position is undefined.
	if (MT_FILE(mt) <= 0)
	    return

	# Are we updating a entry already in the cache?
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1)
	    if (streq (MT_DRIVE(mt), c_drive[1,slot]))
		goto cache_

	# Add the entry to the cache.  Resync the contents of the old slot
	# if it is for a different drive and has been modified since it was
	# last synced.

	slot = prev + 1
	if (slot > SZ_CACHE)
	    slot = 1
	prev = slot

	if (c_modified[slot] == YES && strne (MT_DRIVE(mt), c_drive[1,slot])) {
	    call amovi (c_mtdes[1,slot], MT_STRUCT(0), LEN_MTIODES)
	    call strcpy (c_drive[1,slot], MT_DRIVE(0), SZ_DRIVE)
	    call mt_update_lockfile (0)
	}

cache_
	call amovi (MT_STRUCT(mt), c_mtdes[1,slot], LEN_MTIODES)
	call strcpy (MT_DRIVE(mt), c_drive[1,slot], SZ_DRIVE)
	c_modified[slot] = YES
end


# MT_SYNC -- Update all modified entries in the cache.  Set the position to
# undefined (file=-1) if we are called during error recovery.  We are called
# at task termination by the IRAF Main.

procedure mt_sync (status)

int	status		# task termination status

int	slot
include	"mtcache.com"
include	"mtio.com"

begin
	# Update the .lok files of any active devices.
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1)
	    if (c_modified[slot] == YES) {
		call amovi (c_mtdes[1,slot], MT_STRUCT(0), LEN_MTIODES)
		call strcpy (c_drive[1,slot], MT_DRIVE(0), SZ_DRIVE)
		if (status != OK)
		    MT_FILE(0) = -1

		# Ignore errors if we are called during error recovery.
		iferr (call mt_update_lockfile (0))
		    if (status == OK)
			call erract (EA_ERROR)

		c_modified[slot] = NO
		c_drive[1,slot]  = EOS
	    }

	# Invalidate the cache when a task terminates.
	call mt_clrcache()

	# If we are called during error recovery, set the file position for
	# all open tapes to undefined to prevent mt_savepos from being called
	# by zclsmt when the tape file is closed during error recovery.

	if (status != OK)
	    do slot = 1, MT_MAXTAPES {
		MT_FILE(slot) = -1
		MT_RECORD(slot) = -1
	    }
end


# MT_CLRCACHE -- Initialize the cache.

procedure mt_clrcache()

int	slot
include	"mtcache.com"

begin
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1) {
	    c_modified[slot] = NO
	    c_drive[1,slot]  = EOS
	}
end
