# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	"mtio.h"

# ZAWTMT -- Wait for the last i/o transfer to complete, update tape position
# counters, return nbytes|status to caller.

procedure zawtmt (mtchan, status)

int	mtchan
int	status
int	nrecords, nfiles
include	"mtio.com"

begin
	# Keep returning nbytes=0 after hitting EOF or EOT on a read.
	if ((MT_ACMODE(mtchan) == READ_ONLY) &&
	    (MT_ATEOF(mtchan) == YES || MT_ATEOT(mtchan) == YES)) {
	    status = 0
	    return
	}

	call zzwtmt (MT_OSCHAN(mtchan), nrecords, nfiles, status)

	if (status == 0) {
	    MT_ATEOF(mtchan) = YES
	    if (MT_RECORD(mtchan) == 1)
		MT_ATEOT(mtchan) = YES
	}
	if (nfiles > 0)
	    MT_RECORD(mtchan) = 1

	# Keep track of position on tape and within file.  Note that "nrecords"
	# should be zero if we pass a filemark (nfiles > 0).

	MT_FILE(mtchan) = MT_FILE(mtchan) + nfiles
	MT_RECORD(mtchan) = MT_RECORD(mtchan) + nrecords
	MT_NRECORDS(mtchan) = MT_NRECORDS(mtchan) + nrecords
end
