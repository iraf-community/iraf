# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	"mtio.h"

# ZOPNMT -- Open magtape device at the specifed file.  We are called indirectly
# by MTOPEN (via fopnbf), which sets up a new mtio device decriptor pointed
# to by NEW_MTCHAN, and passes it via the mtio common.  On entry, the field
# MT_NRECORDS will be set to MT_MAGIC if we were called by MTOPEN.

procedure zopnmt (drive, access_mode, mtchan)

char	drive[ARB]		# PACKED drive name string
int	access_mode		# read,write,append
int	mtchan			# return value (mt descriptor index)
int	mt, new_file
include	"mtio.com"
define	err_ 91

begin
	# Pick up index of mt descriptor slot set up by MTOPEN.  Make sure
	# that we were called by MTOPEN and not somebody else.

	mt = new_mtchan
	if (mt < 1 || mt > MT_MAXTAPES || MT_NRECORDS(mt) != MT_MAGIC)
	    goto err_

	new_file = MT_FILE(mt)
	call zzopmt (drive, MT_DENSITY(mt), MT_ACMODE(mt),
	    MT_OLDRECORD(mt), MT_OLDFILE(mt), new_file, MT_OSCHAN(mt))
	if (MT_OSCHAN(mt) == ERR)
	    goto err_

	# Check for end-of-tape.  ZZOPMT returns the actual file number in
	# new_file.  It is ok to try to position to a file beyond EOT for
	# either reading or writing.

	if (MT_FILE(mt) == EOT || new_file < MT_FILE(mt)) {
	    MT_ATEOF(mt) = YES
	    MT_ATEOT(mt) = YES
	}
	MT_FILE(mt) = new_file

	# If " new_mtchan" is nonzero when ZCLSMT is called, it implies that
	# CLOSE was called during error recovery due to an interrupt of ZZOPMT
	# and the position of the tape is undefined.  Clear the flag since the
	# open is now complete and we were not interrupted.

	MT_NRECORDS(mt) = 0
	new_mtchan = NULL
	mtchan = mt

	return

err_
	# Z-routines can only return ERR in the event of an error.
 	MT_OSCHAN(mt) = NULL
	mtchan = ERR
end
