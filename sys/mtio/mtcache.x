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

	 mt_getpos (mtname, mt)
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


# MT_GETPOS -- Return the device position descriptor of a drive given its
# unit number.

procedure mt_getpos (mtname, mt)

char	mtname[ARB]		#I device name
int	mt			#I MTIO descriptor

int	slot
bool	streq()
include	"mtcache.com"
include	"mtio.com"

begin
	# First look in the cache.
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1)
	    if (streq (MT_LKNAME(mt), c_lkname[1,slot])) {
		call amovi (c_mtdes[1,slot], MT_DEVPOS(mt), LEN_DEVPOS)
		call strcpy (c_device[1,slot], MT_DEVICE(mt), SZ_DEVICE)
		call strcpy (c_lkname[1,slot], MT_LKNAME(mt), SZ_LKNAME)
		call strcpy (c_iodev[1,slot], MT_IODEV(mt), SZ_IODEV)
		return
	    }

	# Get the current position from the lock file, if there is one.
	call mt_read_lockfile (mtname, mt)
end


# MT_SAVEPOS -- Save the current position in the cache.  The entire descriptor
# is saved since most of the information therein is needed for SYNC, even
# though only a portion of the information will be used by GETPOS.

procedure mt_savepos (mt)

int	mt			#I MTIO descriptor

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
	if (MT_FILNO(mt) <= 0)
	    return

	# Are we updating an entry already in the cache?
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1)
	    if (streq (MT_LKNAME(mt), c_lkname[1,slot]))
		goto cache_

	# Add the entry to the cache.  Resync the contents of the old slot
	# if it is for a different drive and has been modified since it was
	# last synced.

	slot = prev + 1
	if (slot > SZ_CACHE)
	    slot = 1
	prev = slot

	if (c_modified[slot] == YES && strne(MT_LKNAME(mt),c_lkname[1,slot])) {
	    call amovi (c_mtdes[1,slot], MT_DEVPOS(0), LEN_DEVPOS)
	    call strcpy (c_device[1,slot], MT_DEVICE(0), SZ_DEVICE)
	    call strcpy (c_lkname[1,slot], MT_LKNAME(0), SZ_LKNAME)
	    call strcpy (c_iodev[1,slot], MT_IODEV(0), SZ_IODEV)
	    call mt_update_lockfile (0)
	}

cache_
	call amovi (MT_DEVPOS(mt), c_mtdes[1,slot], LEN_DEVPOS)
	call strcpy (MT_DEVICE(mt), c_device[1,slot], SZ_DEVICE)
	call strcpy (MT_LKNAME(mt), c_lkname[1,slot], SZ_LKNAME)
	call strcpy (MT_IODEV(mt), c_iodev[1,slot], SZ_IODEV)
	c_modified[slot] = YES
end


# MT_SYNC -- Update all modified entries in the cache.  Set the position to
# undefined (file=-1) if we are called during error recovery.  We are called
# at task termination by the IRAF Main.

procedure mt_sync (status)

int	status			#I task termination status

int	slot
include	"mtcache.com"
include	"mtio.com"

begin
	# Update the .lok files of any active devices.
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1) {
	    if (c_modified[slot] == YES) {
		# If called during error recovery mark the file position undef.
		if (status != OK) {
		    call amovi (c_mtdes[1,slot], MT_DEVPOS(0), LEN_DEVPOS)
		    MT_FILNO(0) = -1
		    MT_RECNO(0) = -1
		    call amovi (MT_DEVPOS(0), c_mtdes[1,slot], LEN_DEVPOS)
		}

		# Update the lockfile.
		call amovi (c_mtdes[1,slot], MT_DEVPOS(0), LEN_DEVPOS)
		call strcpy (c_device[1,slot], MT_DEVICE(0), SZ_DEVICE)
		call strcpy (c_lkname[1,slot], MT_LKNAME(0), SZ_LKNAME)
		call strcpy (c_iodev[1,slot], MT_IODEV(0), SZ_IODEV)

		# Ignore errors if we are called during error recovery.
		iferr (call mt_update_lockfile (0))
		    if (status == OK)
			call erract (EA_ERROR)

		c_modified[slot] = NO
		c_device[1,slot] = EOS
	    }
	}

	# Invalidate the cache when a task terminates.
	call mt_clrcache()

	# If we are called during error recovery, set the file position for
	# all open tapes to undefined to prevent mt_savepos from being called
	# by zclsmt when the tape file is closed during error recovery.

	if (status != OK)
	    do slot = 1, MT_MAXTAPES {
		MT_FILNO(slot) = -1
		MT_RECNO(slot) = -1
	    }
end


# MT_CLRCACHE -- Initialize the cache.

procedure mt_clrcache()

int	slot
include	"mtcache.com"

begin
	for (slot=1;  slot <= SZ_CACHE;  slot=slot+1) {
	    c_modified[slot] = NO
	    c_device[1,slot] = EOS
	    c_lkname[1,slot] = EOS
	    c_iodev[1,slot] = EOS
	    call aclri (c_mtdes[1,slot], LEN_DEVPOS)
	}
end
