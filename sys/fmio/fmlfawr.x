# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FM_LFAWRITE -- Asynchronous blocked write to an lfile.  The differences
# between text and binary lfiles are not visible above this level; both
# appear as binary files to FIO.

procedure fm_lfawrite (lf, buf, nbytes, offset)

pointer	lf			#I lfile descriptor
char	buf[ARB]		#O input data buffer
int	nbytes			#I nbytes to write
long	offset			#I lfile offset

int	status, nb
pointer	sp, pk_buf

begin
	if (and (LF_FLAGS(lf), LFF_TEXTFILE) == 0)
	    call fm_lfbinwrite (lf, buf, nbytes, offset)
	else {
	    call smark (sp)
	    call salloc (pk_buf, nbytes / SZB_CHAR, TY_CHAR)

	    # Wait for i/o to complete before freeing buffer!
	    nb = (nbytes + SZB_CHAR-1) / SZB_CHAR
	    call chrpak (buf, 1, Memc[pk_buf], 1, nb)
	    call fm_lfbinwrite (lf, Memc[pk_buf], nb, (offset-1)/SZB_CHAR+1)
	    call fm_lfbinwait (lf, status)

	    call sfree (sp)
	}
end
