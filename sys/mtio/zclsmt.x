# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<knet.h>
include	"mtio.h"

define	SZ_NULLSTR	8		# "NULLFILE"


# ZCLSMT -- Close a magtape file and device.  If file was opened for writing
# but nothing was written, write out the "NULLFILE" record.  Update lockfile
# so that we will know where the tape is positioned next time we open the
# device.  Deallocate the mtio descriptor so that it may be reused again.
# 
# We are being called during error recovery if "new_mtchan" is not null.
# If MT_OSCHAN has also been set, then ZZOPMT was interrupted, probably while
# trying to position the tape, and the position of the tape is indefinite.
# Close the tape with acmode=read so that no tape marks are written, and write
# the lockfile with file=0 to signify that the position is indefinite.

procedure zclsmt (mtchan, status)

int	mtchan				# index of mtio descriptor
int	status

char	nullstr[SZ_NULLSTR]
int	mt, junk, nfiles, nrecords
include	"mtio.com"

begin
	# Called by error recovery while positioning tape? (during open)
	if (new_mtchan != NULL) {
	    mt = new_mtchan
	    if (MT_OSCHAN(mt) != NULL)
		call zzclmt (MT_OSCHAN(mt), READ_ONLY, nrecords, nfiles, status)
	    MT_FILE(mt) = -1
	    call mt_savepos (mt)
	    new_mtchan = NULL

	} else {
	    mt = mtchan

	    # Write nullfile record if tape opened for writing but no data
	    # was written.

	    if (MT_ACMODE(mt) == WRITE_ONLY && MT_NRECORDS(mt) == 0) {
		call chrpak ("NULLFILE", 1, nullstr, 1, SZ_NULLSTR)
		call zawrmt (mt, nullstr, SZ_NULLSTR, 0)
		call zawtmt (mt, junk)
	    }

	    # Close device.
	    call zzclmt (MT_OSCHAN(mt), MT_ACMODE(mt), nrecords, nfiles, status)

	    # Update the position.  If a task aborts while a tape file is open,
	    # mt_sync will already have been called to update the position, and
	    # the current file will have been set to undefined (-1).

	    if (MT_FILE(mt) != -1) {
		if (nfiles > 0) {
		    MT_FILE(mt) = MT_FILE(mt) + nfiles
		    MT_RECORD(mt) = 1
		} else
		    MT_RECORD(mt) = MT_RECORD(mt) + nrecords
		call mt_savepos (mt)
	    }
	}

	MT_OSCHAN(mt) = NULL
end
