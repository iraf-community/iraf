# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<knet.h>
include	"mtio.h"

# ZCLSMT -- Close a magtape file and device.  Update lockfile so that we
# will know where the tape is positioned next time we open the device.
# Deallocate the mtio descriptor so that it may be reused again.
# 
# We are being called during error recovery if "new_mtchan" is not null.
# If MT_OSCHAN has also been set, then ZZOPMT was interrupted, probably while
# trying to position the tape, and the position of the tape is indefinite.
# Close the tape with acmode=read so that no tape marks are written, and write
# the lockfile with file = -1 to signify that the position is indefinite.

procedure zclsmt (mtchan, status)

int	mtchan				#I i/o channel
int	status				#O close status

int	mt
bool	error_recovery
include	"mtio.com"

begin
	# Called by error recovery while positioning tape? (during open)
	if (new_mtchan != NULL) {
	    mt = new_mtchan
	    if (MT_OSCHAN(mt) != NULL)
		call zzclmt (MT_OSCHAN(mt), MT_DEVPOS(mt), status)

	    call mt_savepos (mt)
	    call mt_sync (ERR)
	    new_mtchan = NULL

	} else {
	    mt = mtchan

	    # If a task aborts while a tape file is open, mt_sync will
	    # already have been called to update the position,
	    # and the current file will have been set to undefined (-1).

	    error_recovery = (MT_FILNO(mt) == -1)

	    # Close device.  This clobbers MT_FILNO (see above).
	    call zzclmt (MT_OSCHAN(mt), MT_DEVPOS(mt), status)

	    # Update the tape position if not recovering from an abort.
	    if (!error_recovery)
		call mt_savepos (mt)
	}

	MT_OSCHAN(mt) = NULL
end
