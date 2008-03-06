# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FM_LFAREAD -- Asynchronous blocked read from an lfile.  The differences
# between text and binary lfiles are not visible above this level; both
# appear as binary files to FIO.

procedure fm_lfaread (lf_chan, buf, maxbytes, offset)

int	lf_chan			#I lfile descriptor
char	buf[ARB]		#O output data buffer
size_t	maxbytes		#I max bytes to read
long	offset			#I lfile offset

size_t	sz_val
pointer	lf
long	status
size_t	nb, c_1

include "fmio.com"

begin
	c_1 = 1
	lf = Memp[lf_ptrs+lf_chan]
	# If reading text data, unpack the text data in place.
	if (and (LF_FLAGS(lf), LFF_TEXTFILE) != 0) {
	    nb = (maxbytes + SZB_CHAR-1) / SZB_CHAR
	    call fm_lfbinread (lf_chan, buf, nb, (offset-1) / SZB_CHAR + 1)
	    call fm_lfbinwait (lf_chan, status)
	    if (status > 0) {
		sz_val = min(maxbytes,status)
		call chrupk (buf, c_1, buf, c_1, sz_val)
	    }
	} else
	    call fm_lfbinread (lf_chan, buf, maxbytes, offset)
end
