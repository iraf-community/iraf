# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FM_LFAWRITE -- Asynchronous blocked write to an lfile.  The differences
# between text and binary lfiles are not visible above this level; both
# appear as binary files to FIO.

procedure fm_lfawrite (lf_chan, buf, nbytes, offset)

int	lf_chan			#I lfile descriptor
char	buf[ARB]		#O input data buffer
size_t	nbytes			#I nbytes to write
long	offset			#I lfile offset

pointer	lf
long	status
size_t	nb, c_1
pointer	sp, pk_buf

include "fmio.com"

begin
	c_1 = 1
	lf = Memp[lf_ptrs+lf_chan]

	if (and (LF_FLAGS(lf), LFF_TEXTFILE) == 0)
	    call fm_lfbinwrite (lf_chan, buf, nbytes, offset)
	else {
	    call smark (sp)
	    call salloc (pk_buf, nbytes / SZB_CHAR, TY_CHAR)

	    # Wait for i/o to complete before freeing buffer!
	    nb = (nbytes + SZB_CHAR-1) / SZB_CHAR
	    call chrpak (buf, c_1, Memc[pk_buf], c_1, nb)
	    call fm_lfbinwrite (lf_chan, Memc[pk_buf], nb, (offset-1)/SZB_CHAR+1)
	    call fm_lfbinwait (lf_chan, status)

	    call sfree (sp)
	}
end
