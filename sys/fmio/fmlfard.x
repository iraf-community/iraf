# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FM_LFAREAD -- Asynchronous blocked read from an lfile.  The differences
# between text and binary lfiles are not visible above this level; both
# appear as binary files to FIO.

procedure fm_lfaread (lf, buf, maxbytes, offset)

pointer	lf			#I lfile descriptor
char	buf[ARB]		#O output data buffer
int	maxbytes		#I max bytes to read
long	offset			#I lfile offset

int	status, nb

begin
	# If reading text data, unpack the text data in place.
	if (and (LF_FLAGS(lf), LFF_TEXTFILE) != 0) {
	    nb = (maxbytes + SZB_CHAR-1) / SZB_CHAR
	    call fm_lfbinread (lf, buf, nb, (offset-1) / SZB_CHAR + 1)
	    call fm_lfbinwait (lf, status)
	    if (status > 0)
		call chrupk (buf, 1, buf, 1, min(maxbytes,status))
	} else
	    call fm_lfbinread (lf, buf, maxbytes, offset)
end
